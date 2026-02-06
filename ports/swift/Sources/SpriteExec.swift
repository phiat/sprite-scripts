import Foundation

/// Wrapper for running commands on a sprite via `sprite exec`.
struct SpriteExec {
    let sprite: String
    let dryRun: Bool

    init(sprite: String, dryRun: Bool) {
        self.sprite = sprite
        self.dryRun = dryRun
    }

    /// Run a bash command on the sprite. Returns stdout as a string.
    /// Terminates on failure.
    @discardableResult
    func sx(_ cmd: String) -> String {
        if dryRun {
            print("  [dry-run] sprite exec -s \(sprite) bash -c \"\(cmd)\"")
            return ""
        }
        let process = Process()
        process.executableURL = URL(fileURLWithPath: "/usr/bin/env")
        process.arguments = ["sprite", "exec", "-s", sprite, "bash", "-c", cmd]
        let outPipe = Pipe()
        let errPipe = Pipe()
        process.standardOutput = outPipe
        process.standardError = errPipe
        do {
            try process.run()
        } catch {
            fputs("Failed to run sprite exec: \(cmd)\n\(error)\n", stderr)
            exit(1)
        }
        process.waitUntilExit()
        let outData = outPipe.fileHandleForReading.readDataToEndOfFile()
        let errData = errPipe.fileHandleForReading.readDataToEndOfFile()
        if process.terminationStatus != 0 {
            let stderrStr = String(data: errData, encoding: .utf8) ?? ""
            fputs("sprite exec failed (exit \(process.terminationStatus)): \(cmd)\nstderr: \(stderrStr)\n", stderr)
            exit(1)
        }
        return String(data: outData, encoding: .utf8) ?? ""
    }

    /// Run a bash command on the sprite, ignoring errors. Returns trimmed stdout.
    func sxQuiet(_ cmd: String) -> String {
        if dryRun {
            print("  [dry-run] sprite exec -s \(sprite) bash -c \"\(cmd)\"")
            return ""
        }
        let process = Process()
        process.executableURL = URL(fileURLWithPath: "/usr/bin/env")
        process.arguments = ["sprite", "exec", "-s", sprite, "bash", "-c", cmd]
        let outPipe = Pipe()
        process.standardOutput = outPipe
        process.standardError = FileHandle.nullDevice
        do {
            try process.run()
        } catch {
            return ""
        }
        process.waitUntilExit()
        let outData = outPipe.fileHandleForReading.readDataToEndOfFile()
        return (String(data: outData, encoding: .utf8) ?? "").trimmingCharacters(in: .whitespacesAndNewlines)
    }

    /// Run a bash command on the sprite interactively (inheriting stdio).
    func sxInteractive(_ cmd: String) {
        if dryRun {
            print("  [dry-run] sprite exec -s \(sprite) bash -c \"\(cmd)\"")
            return
        }
        let process = Process()
        process.executableURL = URL(fileURLWithPath: "/usr/bin/env")
        process.arguments = ["sprite", "exec", "-s", sprite, "bash", "-c", cmd]
        process.standardInput = FileHandle.standardInput
        process.standardOutput = FileHandle.standardOutput
        process.standardError = FileHandle.standardError
        do {
            try process.run()
        } catch {
            fputs("Failed to run sprite exec: \(cmd)\n\(error)\n", stderr)
            exit(1)
        }
        process.waitUntilExit()
        if process.terminationStatus != 0 {
            fputs("sprite exec failed (exit \(process.terminationStatus)): \(cmd)\n", stderr)
            exit(1)
        }
    }

    /// Push a local file to a remote path on the sprite.
    func pushFile(src: String, dest: String) {
        if dryRun {
            print("  [dry-run] push \(src) -> sprite:\(dest)")
            return
        }
        let destDir = (dest as NSString).deletingLastPathComponent
        sx("mkdir -p '\(destDir)'")

        guard let fileData = FileManager.default.contents(atPath: src) else {
            fputs("Failed to read local file: \(src)\n", stderr)
            exit(1)
        }

        let process = Process()
        process.executableURL = URL(fileURLWithPath: "/usr/bin/env")
        process.arguments = ["sprite", "exec", "-s", sprite, "bash", "-c", "cat > '\(dest)'"]
        let inPipe = Pipe()
        process.standardInput = inPipe
        process.standardOutput = FileHandle.nullDevice
        process.standardError = Pipe()
        do {
            try process.run()
        } catch {
            fputs("Failed to spawn sprite exec for file push: \(error)\n", stderr)
            exit(1)
        }
        inPipe.fileHandleForWriting.write(fileData)
        inPipe.fileHandleForWriting.closeFile()
        process.waitUntilExit()
        if process.terminationStatus != 0 {
            fputs("push_file failed for \(dest)\n", stderr)
            exit(1)
        }
    }

    /// Push a local directory to a remote path on the sprite via tar piping.
    func pushDir(src: String, dest: String) {
        if dryRun {
            print("  [dry-run] push dir \(src) -> sprite:\(dest)")
            return
        }
        let srcPath = src as NSString
        let parent = srcPath.deletingLastPathComponent.isEmpty ? "." : srcPath.deletingLastPathComponent
        let base = srcPath.lastPathComponent

        sx("mkdir -p '\(dest)'")

        let destParent = (dest as NSString).deletingLastPathComponent
        let destParentStr = destParent.isEmpty ? "/" : destParent

        // Spawn local tar to create archive
        let tarProc = Process()
        tarProc.executableURL = URL(fileURLWithPath: "/usr/bin/env")
        tarProc.arguments = ["tar", "czf", "-", "-C", parent, base]
        let tarOutPipe = Pipe()
        tarProc.standardOutput = tarOutPipe
        tarProc.standardError = Pipe()

        // Spawn sprite exec to receive and extract archive
        let spriteProc = Process()
        spriteProc.executableURL = URL(fileURLWithPath: "/usr/bin/env")
        spriteProc.arguments = ["sprite", "exec", "-s", sprite, "bash", "-c",
                                "tar xzf - -C '\(destParentStr)'"]
        let spriteInPipe = Pipe()
        spriteProc.standardInput = spriteInPipe
        spriteProc.standardOutput = FileHandle.nullDevice
        spriteProc.standardError = Pipe()

        do {
            try tarProc.run()
            try spriteProc.run()
        } catch {
            fputs("Failed to spawn processes for dir push: \(error)\n", stderr)
            exit(1)
        }

        // Pipe tar stdout -> sprite stdin in background
        DispatchQueue.global().async {
            let data = tarOutPipe.fileHandleForReading.readDataToEndOfFile()
            spriteInPipe.fileHandleForWriting.write(data)
            spriteInPipe.fileHandleForWriting.closeFile()
        }

        tarProc.waitUntilExit()
        spriteProc.waitUntilExit()

        if tarProc.terminationStatus != 0 {
            fputs("Local tar failed\n", stderr)
            exit(1)
        }
        if spriteProc.terminationStatus != 0 {
            fputs("Remote tar extract failed\n", stderr)
            exit(1)
        }
    }
}

/// Run a sprite CLI command directly (not via exec). Returns stdout.
func spriteCmd(_ args: [String]) -> String {
    let process = Process()
    process.executableURL = URL(fileURLWithPath: "/usr/bin/env")
    process.arguments = ["sprite"] + args
    let outPipe = Pipe()
    process.standardOutput = outPipe
    process.standardError = FileHandle.nullDevice
    do {
        try process.run()
    } catch {
        return ""
    }
    process.waitUntilExit()
    let outData = outPipe.fileHandleForReading.readDataToEndOfFile()
    return String(data: outData, encoding: .utf8) ?? ""
}

/// Check if a command exists on PATH.
func commandExists(_ name: String) -> Bool {
    let process = Process()
    process.executableURL = URL(fileURLWithPath: "/usr/bin/env")
    process.arguments = ["which", name]
    process.standardOutput = FileHandle.nullDevice
    process.standardError = FileHandle.nullDevice
    do {
        try process.run()
    } catch {
        return false
    }
    process.waitUntilExit()
    return process.terminationStatus == 0
}

/// Get a simple HH:MM:SS timestamp.
func getTimestamp() -> String {
    let process = Process()
    process.executableURL = URL(fileURLWithPath: "/usr/bin/env")
    process.arguments = ["date", "+%H:%M:%S"]
    let outPipe = Pipe()
    process.standardOutput = outPipe
    process.standardError = FileHandle.nullDevice
    do {
        try process.run()
    } catch {
        return "??:??:??"
    }
    process.waitUntilExit()
    let data = outPipe.fileHandleForReading.readDataToEndOfFile()
    return (String(data: data, encoding: .utf8) ?? "??:??:??").trimmingCharacters(in: .whitespacesAndNewlines)
}
