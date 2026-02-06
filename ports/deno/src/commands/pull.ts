/**
 * pull.ts — Pull file or directory from a sprite
 *
 * Usage: sprite-tool pull <remote-path> <local-path> [sprite-name]
 */

import { pullFile, pullDir, isRemoteDir } from "../lib/transfer.ts";

function usage(): never {
  console.log(`Usage: sprite-tool pull <remote-path> <local-path> [sprite-name]

Examples:
  sprite-tool pull /home/sprite/file.txt ./file.txt
  sprite-tool pull /home/sprite/mydir ./mydir
  sprite-tool pull /tmp/file.txt ./file.txt ember-red-hawk`);
  Deno.exit(1);
}

export async function pull(args: string[]): Promise<void> {
  if (args.length < 2) {
    usage();
  }

  const remotePath = args[0];
  const localPath = args[1];
  const sprite = args[2] || "";

  if (isRemoteDir(sprite, remotePath)) {
    console.log(`Pulling directory: ${remotePath} -> ${localPath}`);
    await pullDir(sprite, remotePath, localPath);
  } else {
    console.log(`Pulling file: ${remotePath} -> ${localPath}`);
    await pullFile(sprite, remotePath, localPath);
  }

  console.log("Done.");
}
