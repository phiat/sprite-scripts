import Foundation
#if canImport(Glibc)
import Glibc
#elseif canImport(Darwin)
import Darwin
#endif

/// Run the launch subcommand.
func launchCommand(dryRun: Bool, noCheckpoint: Bool, uploadDirs: [String],
                   spriteName: String, planFile: String?) {
    let config = Config.load()
    let sx = SpriteExec(sprite: spriteName, dryRun: dryRun)
    let checkpointing = !noCheckpoint

    // 1. Check/install sprite CLI
    if !commandExists("sprite") {
        if dryRun {
            print("  [dry-run] Would install sprite CLI")
        } else {
            print("Installing sprite CLI...")
            let process = Process()
            process.executableURL = URL(fileURLWithPath: "/bin/sh")
            process.arguments = ["-c", "curl -fsSL https://sprites.dev/install.sh | sh"]
            do {
                try process.run()
                process.waitUntilExit()
                if process.terminationStatus != 0 {
                    fputs("Sprite CLI installation failed\n", stderr)
                    exit(1)
                }
            } catch {
                fputs("Failed to install sprite CLI: \(error)\n", stderr)
                exit(1)
            }
            // Update PATH for this process
            if let home = ProcessInfo.processInfo.environment["HOME"] {
                let currentPath = ProcessInfo.processInfo.environment["PATH"] ?? ""
                setenv("PATH", "\(home)/.local/bin:\(currentPath)", 1)
            }
        }
    }

    // 2. Auth sprite
    if !config.spriteToken.isEmpty {
        print("Authenticating sprite with token...")
        if !dryRun {
            let process = Process()
            process.executableURL = URL(fileURLWithPath: "/usr/bin/env")
            process.arguments = ["sprite", "auth", "setup", "--token", config.spriteToken]
            do {
                try process.run()
                process.waitUntilExit()
                if process.terminationStatus != 0 {
                    fputs("sprite auth setup failed\n", stderr)
                    exit(1)
                }
            } catch {
                fputs("Failed to run sprite auth setup: \(error)\n", stderr)
                exit(1)
            }
        }
    } else {
        print("No SPRITE_TOKEN set. Running interactive login...")
        if !dryRun {
            let process = Process()
            process.executableURL = URL(fileURLWithPath: "/usr/bin/env")
            process.arguments = ["sprite", "login"]
            process.standardInput = FileHandle.standardInput
            process.standardOutput = FileHandle.standardOutput
            process.standardError = FileHandle.standardError
            do {
                try process.run()
                process.waitUntilExit()
                if process.terminationStatus != 0 {
                    fputs("sprite login failed\n", stderr)
                    exit(1)
                }
            } catch {
                fputs("Failed to run sprite login: \(error)\n", stderr)
                exit(1)
            }
        }
    }

    // 3. Create sprite (or use existing)
    if dryRun {
        print("  [dry-run] Would create or reuse sprite '\(spriteName)'")
    } else {
        let lsOutput = spriteCmd(["ls"])
        let spriteExists = lsOutput
            .components(separatedBy: .newlines)
            .contains { line in
                line.components(separatedBy: .whitespaces).contains(spriteName)
            }
        if spriteExists {
            print("Sprite '\(spriteName)' already exists, using it.")
        } else {
            print("Creating sprite: \(spriteName)")
            let process = Process()
            process.executableURL = URL(fileURLWithPath: "/usr/bin/env")
            process.arguments = ["sprite", "create", "-skip-console", spriteName]
            do {
                try process.run()
                process.waitUntilExit()
                if process.terminationStatus != 0 {
                    fputs("sprite create failed\n", stderr)
                    exit(1)
                }
            } catch {
                fputs("Failed to create sprite: \(error)\n", stderr)
                exit(1)
            }
        }
    }

    // 4. Push .env to sprite
    if FileManager.default.fileExists(atPath: config.envFile) {
        print("Pushing \(config.envFile)...")
        sx.pushFile(src: config.envFile, dest: "/home/sprite/.env")
    }

    // 5. Push plan file if provided
    if let plan = planFile, FileManager.default.fileExists(atPath: plan) {
        print("Pushing \(plan)...")
        sx.pushFile(src: plan, dest: "/home/sprite/plan.md")
    }

    // 6. Upload directories if provided
    for dir in uploadDirs {
        var isDir: ObjCBool = false
        if FileManager.default.fileExists(atPath: dir, isDirectory: &isDir), isDir.boolValue {
            let dirname = (dir as NSString).lastPathComponent
            let remoteDest = "/home/sprite/\(dirname)"
            print("Uploading directory: \(dir) -> \(remoteDest)")
            sx.pushDir(src: dir, dest: remoteDest)
        } else {
            print("WARNING: --upload dir '\(dir)' not found, skipping.")
        }
    }

    // 7. Setup git + beads
    print("Initializing git...")
    sx.sx("cd /home/sprite && git init -b main 2>/dev/null || true")

    print("Installing beads...")
    sx.sx("curl -fsSL https://raw.githubusercontent.com/steveyegge/beads/main/scripts/install.sh | bash")

    // 8. Install and auth coding agent
    switch config.agent {
    case "claude":
        print("Setting up claude...")
        sx.sx("command -v claude >/dev/null 2>&1 || npm install -g @anthropic-ai/claude-code")

        if config.claudeAuth == "subscription" {
            let home = ProcessInfo.processInfo.environment["HOME"] ?? "/root"
            let credsPath = "\(home)/.claude/.credentials.json"
            if FileManager.default.fileExists(atPath: credsPath) {
                print("Copying claude subscription credentials...")
                sx.pushFile(src: credsPath, dest: "/home/sprite/.claude/.credentials.json")
                sx.sx("chmod 600 ~/.claude/.credentials.json")
            } else {
                fputs("ERROR: ~/.claude/.credentials.json not found\n", stderr)
                fputs("Run 'claude' locally first to authenticate, then re-run this script.\n", stderr)
                exit(1)
            }
        } else if config.claudeAuth == "apikey" && !config.anthropicApiKey.isEmpty {
            print("Setting ANTHROPIC_API_KEY in sprite...")
            sx.sx("grep -q ANTHROPIC_API_KEY ~/.bashrc 2>/dev/null || echo 'export ANTHROPIC_API_KEY=\"\(config.anthropicApiKey)\"' >> ~/.bashrc")
        } else {
            fputs("ERROR: No valid claude auth configured\n", stderr)
            fputs("Set CLAUDE_AUTH=subscription (default) or CLAUDE_AUTH=apikey with ANTHROPIC_API_KEY\n", stderr)
            exit(1)
        }

    case "opencode":
        print("Setting up opencode...")
        sx.sx("[ -x ~/.opencode/bin/opencode ] || curl -fsSL https://opencode.ai/install | bash")
        sx.sx("grep -q 'source.*\\.env' ~/.bashrc 2>/dev/null || echo '[ -f /home/sprite/.env ] && set -a && source /home/sprite/.env && set +a' >> ~/.bashrc")

    default:
        fputs("ERROR: Unknown AGENT '\(config.agent)'. Use 'claude' or 'opencode'.\n", stderr)
        exit(1)
    }

    // 9. Launch agent with plan (or open console)
    print("")
    print("==========================================")
    print("Sprite '\(spriteName)' is ready!")
    if config.model.isEmpty {
        print("Agent: \(config.agent)")
    } else {
        print("Agent: \(config.agent) (model: \(config.model))")
    }
    if checkpointing {
        print("Checkpointing: every \(config.checkpointInterval)s")
    }
    print("==========================================")

    if dryRun {
        print("")
        print("[dry-run] Would launch \(config.agent) with plan. No changes were made.")
        return
    }

    if planFile != nil {
        // Start auto-checkpointing before agent runs
        var stopCheckpointing = false
        var checkpointThread: Thread? = nil

        if checkpointing {
            let interval = config.checkpointInterval
            let name = spriteName
            checkpointThread = Thread {
                checkpointLoop(sprite: name, interval: interval, stop: &stopCheckpointing)
            }
            checkpointThread?.start()
        }

        print("Launching \(config.agent) with plan...")

        switch config.agent {
        case "claude":
            let modelFlag = config.model.isEmpty ? "" : "--model \(config.model) "
            sx.sxInteractive("cd /home/sprite && claude \(modelFlag)-p 'read plan.md and complete the plan please'")

        case "opencode":
            let ocModel = config.model.isEmpty ? "opencode/big-pickle" : config.model
            sx.sxInteractive("set -a && source /home/sprite/.env 2>/dev/null && set +a && cd /home/sprite && ~/.opencode/bin/opencode run -m \(ocModel) 'read plan.md and complete the plan please'")

        default:
            break
        }

        // Stop checkpointing
        stopCheckpointing = true
        // Give the checkpoint thread a moment to notice and exit
        Thread.sleep(forTimeInterval: 2)

        // Final checkpoint
        print("Creating final checkpoint...")
        let cpProc = Process()
        cpProc.executableURL = URL(fileURLWithPath: "/usr/bin/env")
        cpProc.arguments = ["sprite", "checkpoint", "create", "-s", spriteName]
        cpProc.standardOutput = FileHandle.nullDevice
        cpProc.standardError = FileHandle.nullDevice
        do {
            try cpProc.run()
            cpProc.waitUntilExit()
            if cpProc.terminationStatus == 0 {
                print("Final checkpoint saved.")
            } else {
                print("Final checkpoint failed (non-fatal).")
            }
        } catch {
            print("Final checkpoint failed (non-fatal).")
        }
    } else {
        print("Opening console...")
        let process = Process()
        process.executableURL = URL(fileURLWithPath: "/usr/bin/env")
        process.arguments = ["sprite", "console", "-s", spriteName]
        process.standardInput = FileHandle.standardInput
        process.standardOutput = FileHandle.standardOutput
        process.standardError = FileHandle.standardError
        do {
            try process.run()
            process.waitUntilExit()
            if process.terminationStatus != 0 {
                fputs("sprite console exited with error\n", stderr)
                exit(1)
            }
        } catch {
            fputs("Failed to open sprite console: \(error)\n", stderr)
            exit(1)
        }
    }
}

/// Background checkpoint loop. Runs `sprite checkpoint create` every `interval` seconds
/// until `stop` is set to true.
private func checkpointLoop(sprite: String, interval: Int, stop: UnsafeMutablePointer<Bool>) {
    print("Auto-checkpointing every \(interval)s (background thread)")
    while true {
        // Sleep in 1-second increments to check the stop flag promptly
        for _ in 0..<interval {
            if stop.pointee {
                return
            }
            Thread.sleep(forTimeInterval: 1)
        }
        if stop.pointee {
            return
        }

        let timeStr = getTimestamp()
        print("[checkpoint] Creating checkpoint at \(timeStr)...")
        let process = Process()
        process.executableURL = URL(fileURLWithPath: "/usr/bin/env")
        process.arguments = ["sprite", "checkpoint", "create", "-s", sprite]
        process.standardOutput = FileHandle.nullDevice
        process.standardError = FileHandle.nullDevice
        do {
            try process.run()
            process.waitUntilExit()
            if process.terminationStatus == 0 {
                print("[checkpoint] Done.")
            } else {
                print("[checkpoint] Failed (non-fatal).")
            }
        } catch {
            print("[checkpoint] Failed (non-fatal).")
        }
    }
}
