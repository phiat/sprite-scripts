/**
 * sprite.ts — Deno.Command wrapper for the `sprite` CLI
 */

const decoder = new TextDecoder();

/** Options shared across sprite exec helpers */
export interface ExecOptions {
  sprite?: string;
  dryRun?: boolean;
}

/**
 * Build the base argument array for `sprite exec`.
 * If a sprite name is provided, adds `-s <name>`.
 */
function spriteExecArgs(sprite?: string): string[] {
  const args = ["exec"];
  if (sprite) {
    args.push("-s", sprite);
  }
  return args;
}

/**
 * Run a bash command inside a sprite synchronously.
 * Returns decoded stdout. Throws on non-zero exit (unless ignoreError).
 */
export function sx(
  sprite: string,
  cmd: string,
  opts: { dryRun?: boolean; ignoreError?: boolean } = {},
): string {
  if (opts.dryRun) {
    console.log(`  [dry-run] sprite exec -s ${sprite} bash -c "${cmd}"`);
    return "";
  }

  const result = new Deno.Command("sprite", {
    args: [...spriteExecArgs(sprite), "bash", "-c", cmd],
    stdin: "null",
    stdout: "piped",
    stderr: "piped",
  }).outputSync();

  const stdout = decoder.decode(result.stdout).trim();

  if (!result.success && !opts.ignoreError) {
    const stderr = decoder.decode(result.stderr).trim();
    throw new Error(
      `sprite exec failed (code ${result.code}): ${stderr || stdout}`,
    );
  }

  return stdout;
}

/**
 * Run a bash command inside a sprite asynchronously.
 * Returns decoded stdout.
 */
export async function sxAsync(
  sprite: string,
  cmd: string,
  opts: { dryRun?: boolean; ignoreError?: boolean } = {},
): Promise<string> {
  if (opts.dryRun) {
    console.log(`  [dry-run] sprite exec -s ${sprite} bash -c "${cmd}"`);
    return "";
  }

  const result = await new Deno.Command("sprite", {
    args: [...spriteExecArgs(sprite), "bash", "-c", cmd],
    stdin: "null",
    stdout: "piped",
    stderr: "piped",
  }).output();

  const stdout = decoder.decode(result.stdout).trim();

  if (!result.success && !opts.ignoreError) {
    const stderr = decoder.decode(result.stderr).trim();
    throw new Error(
      `sprite exec failed (code ${result.code}): ${stderr || stdout}`,
    );
  }

  return stdout;
}

/**
 * Run a top-level sprite CLI command synchronously (e.g. sprite ls, sprite create).
 * Returns decoded stdout.
 */
export function spriteCmd(
  args: string[],
  opts: { dryRun?: boolean; ignoreError?: boolean } = {},
): string {
  if (opts.dryRun) {
    console.log(`  [dry-run] sprite ${args.join(" ")}`);
    return "";
  }

  const result = new Deno.Command("sprite", {
    args,
    stdin: "null",
    stdout: "piped",
    stderr: "piped",
  }).outputSync();

  const stdout = decoder.decode(result.stdout).trim();

  if (!result.success && !opts.ignoreError) {
    const stderr = decoder.decode(result.stderr).trim();
    throw new Error(
      `sprite command failed (code ${result.code}): ${stderr || stdout}`,
    );
  }

  return stdout;
}

/**
 * Run a top-level sprite CLI command asynchronously.
 * Returns decoded stdout.
 */
export async function spriteCmdAsync(
  args: string[],
  opts: { dryRun?: boolean; ignoreError?: boolean } = {},
): Promise<string> {
  if (opts.dryRun) {
    console.log(`  [dry-run] sprite ${args.join(" ")}`);
    return "";
  }

  const result = await new Deno.Command("sprite", {
    args,
    stdin: "null",
    stdout: "piped",
    stderr: "piped",
  }).output();

  const stdout = decoder.decode(result.stdout).trim();

  if (!result.success && !opts.ignoreError) {
    const stderr = decoder.decode(result.stderr).trim();
    throw new Error(
      `sprite command failed (code ${result.code}): ${stderr || stdout}`,
    );
  }

  return stdout;
}

/**
 * Check whether the `sprite` CLI is on PATH.
 */
export function isSpriteInstalled(): boolean {
  try {
    const result = new Deno.Command("sprite", {
      args: ["--version"],
      stdin: "null",
      stdout: "null",
      stderr: "null",
    }).outputSync();
    return result.success;
  } catch {
    return false;
  }
}

/**
 * Install the sprite CLI via the official install script.
 */
export async function installSprite(): Promise<void> {
  const curl = new Deno.Command("bash", {
    args: ["-c", "curl -fsSL https://sprites.dev/install.sh | sh"],
    stdin: "null",
    stdout: "inherit",
    stderr: "inherit",
  });
  const result = await curl.output();
  if (!result.success) {
    throw new Error("Failed to install sprite CLI");
  }
  // Add common install location to PATH
  const home = Deno.env.get("HOME") || "";
  const currentPath = Deno.env.get("PATH") || "";
  Deno.env.set("PATH", `${home}/.local/bin:${currentPath}`);
}

/**
 * Spawn a sprite exec process with piped stdin, for streaming data in.
 * Returns the ChildProcess.
 */
export function spawnSpriteExecPiped(
  sprite: string,
  cmd: string,
): Deno.ChildProcess {
  return new Deno.Command("sprite", {
    args: [...spriteExecArgs(sprite), "bash", "-c", cmd],
    stdin: "piped",
    stdout: "piped",
    stderr: "piped",
  }).spawn();
}

/**
 * Spawn a sprite exec process with stdout piped out (for pulling data).
 * Returns the ChildProcess.
 */
export function spawnSpriteExecStdout(
  sprite: string,
  args: string[],
): Deno.ChildProcess {
  return new Deno.Command("sprite", {
    args: [...spriteExecArgs(sprite), ...args],
    stdin: "null",
    stdout: "piped",
    stderr: "piped",
  }).spawn();
}
