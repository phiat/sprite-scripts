import Foundation

/// Run the push subcommand: push a local file or directory to a sprite.
func pushCommand(localPath: String, remotePath: String, spriteName: String?) {
    if !FileManager.default.fileExists(atPath: localPath) {
        fputs("Error: \(localPath) does not exist\n", stderr)
        exit(1)
    }

    var spriteArgs: [String] = ["exec"]
    if let name = spriteName {
        spriteArgs.append("-s")
        spriteArgs.append(name)
    }

    var isDir: ObjCBool = false
    FileManager.default.fileExists(atPath: localPath, isDirectory: &isDir)

    if isDir.boolValue {
        print("Pushing directory: \(localPath) -> \(remotePath)")
        pushDirectory(localPath: localPath, remotePath: remotePath, spriteArgs: spriteArgs)
    } else {
        print("Pushing file: \(localPath) -> \(remotePath)")
        pushFile(localPath: localPath, remotePath: remotePath, spriteArgs: spriteArgs)
    }

    print("Done.")
}

/// Push a directory by tar-piping: local tar czf -> sprite exec tar xzf
private func pushDirectory(localPath: String, remotePath: String, spriteArgs: [String]) {
    let srcPath = localPath as NSString
    let parent = srcPath.deletingLastPathComponent.isEmpty ? "." : srcPath.deletingLastPathComponent
    let base = srcPath.lastPathComponent

    // Spawn local tar to create archive
    let tarProc = Process()
    tarProc.executableURL = URL(fileURLWithPath: "/usr/bin/env")
    tarProc.arguments = ["tar", "czf", "-", "-C", parent, base]
    let tarOutPipe = Pipe()
    tarProc.standardOutput = tarOutPipe
    tarProc.standardError = Pipe()

    // Spawn sprite exec to receive and extract archive
    let bashCmd = "mkdir -p '\(remotePath)' && tar xzf - -C '\(remotePath)' --strip-components=1"
    var args = spriteArgs
    args.append(contentsOf: ["bash", "-c", bashCmd])

    let spriteProc = Process()
    spriteProc.executableURL = URL(fileURLWithPath: "/usr/bin/env")
    spriteProc.arguments = ["sprite"] + args
    let spriteInPipe = Pipe()
    spriteProc.standardInput = spriteInPipe
    spriteProc.standardOutput = FileHandle.nullDevice
    spriteProc.standardError = Pipe()

    do {
        try tarProc.run()
        try spriteProc.run()
    } catch {
        fputs("Failed to spawn processes for directory push: \(error)\n", stderr)
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

/// Push a single file by piping its content to sprite exec cat.
private func pushFile(localPath: String, remotePath: String, spriteArgs: [String]) {
    let remoteDir = (remotePath as NSString).deletingLastPathComponent
    let remoteDirStr = remoteDir.isEmpty ? "/" : remoteDir

    let bashCmd = "mkdir -p '\(remoteDirStr)' && cat > '\(remotePath)'"
    var args = spriteArgs
    args.append(contentsOf: ["bash", "-c", bashCmd])

    guard let fileData = FileManager.default.contents(atPath: localPath) else {
        fputs("Failed to read: \(localPath)\n", stderr)
        exit(1)
    }

    let process = Process()
    process.executableURL = URL(fileURLWithPath: "/usr/bin/env")
    process.arguments = ["sprite"] + args
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
        fputs("File push failed for \(remotePath)\n", stderr)
        exit(1)
    }
}
