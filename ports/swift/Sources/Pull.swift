import Foundation

/// Run the pull subcommand: pull a file or directory from a sprite.
func pullCommand(remotePath: String, localPath: String, spriteName: String?) {
    var spriteArgs: [String] = ["exec"]
    if let name = spriteName {
        spriteArgs.append("-s")
        spriteArgs.append(name)
    }

    // Check if remote path is a directory or file
    let isDir = checkRemoteIsDir(remotePath: remotePath, spriteArgs: spriteArgs)

    if isDir {
        print("Pulling directory: \(remotePath) -> \(localPath)")
        pullDirectory(remotePath: remotePath, localPath: localPath, spriteArgs: spriteArgs)
    } else {
        print("Pulling file: \(remotePath) -> \(localPath)")
        pullFile(remotePath: remotePath, localPath: localPath, spriteArgs: spriteArgs)
    }

    print("Done.")
}

/// Check if a remote path is a directory by running a test on the sprite.
private func checkRemoteIsDir(remotePath: String, spriteArgs: [String]) -> Bool {
    let bashCmd = "[ -d '\(remotePath)' ] && echo dir || echo file"
    var args = spriteArgs
    args.append(contentsOf: ["bash", "-c", bashCmd])

    let process = Process()
    process.executableURL = URL(fileURLWithPath: "/usr/bin/env")
    process.arguments = ["sprite"] + args
    let outPipe = Pipe()
    process.standardOutput = outPipe
    process.standardError = FileHandle.nullDevice

    do {
        try process.run()
    } catch {
        fputs("Failed to check remote path type: \(error)\n", stderr)
        exit(1)
    }
    process.waitUntilExit()

    let data = outPipe.fileHandleForReading.readDataToEndOfFile()
    let stdout = (String(data: data, encoding: .utf8) ?? "")
        .trimmingCharacters(in: .whitespacesAndNewlines)
    return stdout == "dir"
}

/// Pull a directory by tar-piping: sprite exec tar czf -> local tar xzf
private func pullDirectory(remotePath: String, localPath: String, spriteArgs: [String]) {
    // Ensure local directory exists
    do {
        try FileManager.default.createDirectory(atPath: localPath,
                                                 withIntermediateDirectories: true,
                                                 attributes: nil)
    } catch {
        fputs("Failed to create local directory: \(localPath)\n\(error)\n", stderr)
        exit(1)
    }

    // Spawn sprite exec to create tar archive
    var remoteArgs = spriteArgs
    remoteArgs.append(contentsOf: ["tar", "czf", "-", "-C", remotePath, "."])

    let spriteProc = Process()
    spriteProc.executableURL = URL(fileURLWithPath: "/usr/bin/env")
    spriteProc.arguments = ["sprite"] + remoteArgs
    let spriteOutPipe = Pipe()
    spriteProc.standardOutput = spriteOutPipe
    spriteProc.standardError = Pipe()

    // Spawn local tar to extract
    let tarProc = Process()
    tarProc.executableURL = URL(fileURLWithPath: "/usr/bin/env")
    tarProc.arguments = ["tar", "xzf", "-", "-C", localPath]
    let tarInPipe = Pipe()
    tarProc.standardInput = tarInPipe
    tarProc.standardOutput = FileHandle.nullDevice
    tarProc.standardError = Pipe()

    do {
        try spriteProc.run()
        try tarProc.run()
    } catch {
        fputs("Failed to spawn processes for directory pull: \(error)\n", stderr)
        exit(1)
    }

    // Pipe sprite stdout -> tar stdin in background
    DispatchQueue.global().async {
        let data = spriteOutPipe.fileHandleForReading.readDataToEndOfFile()
        tarInPipe.fileHandleForWriting.write(data)
        tarInPipe.fileHandleForWriting.closeFile()
    }

    spriteProc.waitUntilExit()
    tarProc.waitUntilExit()

    if spriteProc.terminationStatus != 0 {
        fputs("Remote tar failed\n", stderr)
        exit(1)
    }
    if tarProc.terminationStatus != 0 {
        fputs("Local tar extract failed\n", stderr)
        exit(1)
    }
}

/// Pull a single file by capturing sprite exec cat output.
private func pullFile(remotePath: String, localPath: String, spriteArgs: [String]) {
    // Ensure parent directory exists locally
    let parentDir = (localPath as NSString).deletingLastPathComponent
    if !parentDir.isEmpty {
        do {
            try FileManager.default.createDirectory(atPath: parentDir,
                                                     withIntermediateDirectories: true,
                                                     attributes: nil)
        } catch {
            fputs("Failed to create local directory: \(parentDir)\n\(error)\n", stderr)
            exit(1)
        }
    }

    var args = spriteArgs
    args.append(contentsOf: ["cat", remotePath])

    let process = Process()
    process.executableURL = URL(fileURLWithPath: "/usr/bin/env")
    process.arguments = ["sprite"] + args
    let outPipe = Pipe()
    process.standardOutput = outPipe
    process.standardError = Pipe()

    do {
        try process.run()
    } catch {
        fputs("Failed to run sprite exec cat: \(error)\n", stderr)
        exit(1)
    }
    process.waitUntilExit()

    if process.terminationStatus != 0 {
        fputs("Failed to pull file: \(remotePath)\n", stderr)
        exit(1)
    }

    let data = outPipe.fileHandleForReading.readDataToEndOfFile()
    let url = URL(fileURLWithPath: localPath)
    do {
        try data.write(to: url)
    } catch {
        fputs("Failed to write local file: \(localPath)\n\(error)\n", stderr)
        exit(1)
    }
}
