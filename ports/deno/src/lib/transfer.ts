/**
 * transfer.ts — tar pipe helpers for pushing/pulling files and directories
 */

import { spawnSpriteExecPiped, spawnSpriteExecStdout, sx } from "./sprite.ts";

/**
 * dirname — return the parent directory of a path (pure string operation).
 */
function dirname(path: string): string {
  const idx = path.lastIndexOf("/");
  if (idx <= 0) return "/";
  return path.slice(0, idx);
}

/**
 * basename — return the last component of a path (pure string operation).
 */
function basename(path: string): string {
  const parts = path.replace(/\/+$/, "").split("/");
  return parts[parts.length - 1] || path;
}

/**
 * Push a local file to a sprite at the given remote path.
 */
export async function pushFile(
  sprite: string,
  localPath: string,
  remotePath: string,
  dryRun: boolean,
): Promise<void> {
  if (dryRun) {
    console.log(`  [dry-run] push ${localPath} -> sprite:${remotePath}`);
    return;
  }

  const remoteDir = dirname(remotePath);
  sx(sprite, `mkdir -p '${remoteDir}'`);

  const fileData = Deno.readFileSync(localPath);

  const proc = spawnSpriteExecPiped(sprite, `cat > '${remotePath}'`);

  const writer = proc.stdin.getWriter();
  await writer.write(fileData);
  await writer.close();

  const status = await proc.status;
  if (!status.success) {
    throw new Error(`Failed to push file ${localPath} to sprite:${remotePath}`);
  }
}

/**
 * Push a local directory to a sprite at the given remote path.
 * Uses tar to stream the directory contents.
 */
export async function pushDir(
  sprite: string,
  localPath: string,
  remotePath: string,
  dryRun: boolean,
): Promise<void> {
  if (dryRun) {
    console.log(`  [dry-run] push dir ${localPath} -> sprite:${remotePath}`);
    return;
  }

  sx(sprite, `mkdir -p '${remotePath}'`);

  // Local tar: create archive from the directory
  const parentDir = dirname(localPath);
  const dirName = basename(localPath);

  const tarProc = new Deno.Command("tar", {
    args: ["czf", "-", "-C", parentDir, dirName],
    stdin: "null",
    stdout: "piped",
    stderr: "piped",
  }).spawn();

  // Remote: extract into the parent of the remote path
  const remoteParent = dirname(remotePath);
  const spriteProc = spawnSpriteExecPiped(
    sprite,
    `tar xzf - -C '${remoteParent}'`,
  );

  // Pipe tar stdout -> sprite stdin
  await tarProc.stdout.pipeTo(spriteProc.stdin);

  const tarStatus = await tarProc.status;
  const spriteStatus = await spriteProc.status;

  if (!tarStatus.success) {
    throw new Error(`Local tar failed for ${localPath}`);
  }
  if (!spriteStatus.success) {
    throw new Error(
      `Remote tar extract failed for sprite:${remotePath}`,
    );
  }
}

/**
 * Push a local directory to a sprite using --strip-components=1 semantics
 * (used by sprite-push where the contents go directly into remotePath).
 */
export async function pushDirStripComponents(
  sprite: string,
  localPath: string,
  remotePath: string,
  dryRun: boolean,
): Promise<void> {
  if (dryRun) {
    console.log(`  [dry-run] push dir ${localPath} -> sprite:${remotePath}`);
    return;
  }

  // Local tar: create archive from the directory
  const parentDir = dirname(localPath);
  const dirName = basename(localPath);

  const tarProc = new Deno.Command("tar", {
    args: ["czf", "-", "-C", parentDir, dirName],
    stdin: "null",
    stdout: "piped",
    stderr: "piped",
  }).spawn();

  // Remote: mkdir + extract with --strip-components=1
  const spriteProc = spawnSpriteExecPiped(
    sprite,
    `mkdir -p '${remotePath}' && tar xzf - -C '${remotePath}' --strip-components=1`,
  );

  // Pipe tar stdout -> sprite stdin
  await tarProc.stdout.pipeTo(spriteProc.stdin);

  const tarStatus = await tarProc.status;
  const spriteStatus = await spriteProc.status;

  if (!tarStatus.success) {
    throw new Error(`Local tar failed for ${localPath}`);
  }
  if (!spriteStatus.success) {
    throw new Error(
      `Remote tar extract failed for sprite:${remotePath}`,
    );
  }
}

/**
 * Pull a file from a sprite to a local path.
 */
export async function pullFile(
  sprite: string,
  remotePath: string,
  localPath: string,
): Promise<void> {
  const localDir = dirname(localPath);
  Deno.mkdirSync(localDir, { recursive: true });

  // Use Deno.Command.output() directly (not .spawn()) so we get the
  // full stdout as a Uint8Array, which is simpler for file writes.
  const spriteArgs = ["exec"];
  if (sprite) {
    spriteArgs.push("-s", sprite);
  }
  spriteArgs.push("cat", remotePath);

  const result = await new Deno.Command("sprite", {
    args: spriteArgs,
    stdin: "null",
    stdout: "piped",
    stderr: "piped",
  }).output();

  if (!result.success) {
    const decoder = new TextDecoder();
    throw new Error(
      `Failed to pull sprite:${remotePath}: ${decoder.decode(result.stderr)}`,
    );
  }

  Deno.writeFileSync(localPath, result.stdout);
}

/**
 * Pull a directory from a sprite to a local path.
 * Uses tar to stream the directory contents.
 */
export async function pullDir(
  sprite: string,
  remotePath: string,
  localPath: string,
): Promise<void> {
  Deno.mkdirSync(localPath, { recursive: true });

  // Remote: tar up the directory
  const spriteProc = spawnSpriteExecStdout(sprite, [
    "tar",
    "czf",
    "-",
    "-C",
    remotePath,
    ".",
  ]);

  // Local: extract into localPath
  const tarProc = new Deno.Command("tar", {
    args: ["xzf", "-", "-C", localPath],
    stdin: "piped",
    stdout: "piped",
    stderr: "piped",
  }).spawn();

  // Pipe sprite stdout -> local tar stdin
  await spriteProc.stdout.pipeTo(tarProc.stdin);

  const spriteStatus = await spriteProc.status;
  const tarStatus = await tarProc.status;

  if (!spriteStatus.success) {
    throw new Error(
      `Remote tar failed for sprite:${remotePath}`,
    );
  }
  if (!tarStatus.success) {
    throw new Error(`Local tar extract failed for ${localPath}`);
  }
}

/**
 * Check if a remote path is a directory on the sprite.
 */
export function isRemoteDir(sprite: string, remotePath: string): boolean {
  const result = sx(
    sprite,
    `[ -d '${remotePath}' ] && echo 'dir' || echo 'file'`,
    { ignoreError: true },
  );
  return result.trim() === "dir";
}
