/**
 * push.ts — Push local file or directory to a sprite
 *
 * Usage: sprite-tool push <local-path> <remote-path> [sprite-name]
 */

import { pushFile, pushDirStripComponents } from "../lib/transfer.ts";

function usage(): never {
  console.log(`Usage: sprite-tool push <local-path> <remote-path> [sprite-name]

Examples:
  sprite-tool push ./file.txt /home/sprite/file.txt
  sprite-tool push ./mydir /home/sprite/mydir
  sprite-tool push ./file.txt /tmp/file.txt ember-red-hawk`);
  Deno.exit(1);
}

export async function push(args: string[]): Promise<void> {
  if (args.length < 2) {
    usage();
  }

  const localPath = args[0];
  const remotePath = args[1];
  const sprite = args[2] || "";

  // Check local path exists
  let stat: Deno.FileInfo;
  try {
    stat = Deno.statSync(localPath);
  } catch {
    console.error(`Error: ${localPath} does not exist`);
    Deno.exit(1);
  }

  if (stat!.isDirectory) {
    console.log(`Pushing directory: ${localPath} -> ${remotePath}`);
    await pushDirStripComponents(sprite, localPath, remotePath, false);
  } else {
    console.log(`Pushing file: ${localPath} -> ${remotePath}`);
    await pushFile(sprite, localPath, remotePath, false);
  }

  console.log("Done.");
}
