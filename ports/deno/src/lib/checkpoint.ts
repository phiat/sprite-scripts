/**
 * checkpoint.ts — Background auto-checkpointing loop
 */

import { spriteCmdAsync } from "./sprite.ts";

let intervalId: number | undefined;

/**
 * Start a background checkpoint loop that creates a sprite checkpoint
 * every `intervalSec` seconds.
 */
export function startCheckpointing(
  sprite: string,
  intervalSec: number,
): void {
  console.log(
    `Auto-checkpointing every ${intervalSec}s`,
  );

  intervalId = setInterval(async () => {
    const now = new Date();
    const ts = now.toTimeString().slice(0, 8); // HH:MM:SS
    console.log(`[checkpoint] Creating checkpoint at ${ts}...`);
    try {
      await spriteCmdAsync(
        ["checkpoint", "create", "-s", sprite],
        { ignoreError: true },
      );
      console.log("[checkpoint] Done.");
    } catch {
      console.log("[checkpoint] Failed (non-fatal).");
    }
  }, intervalSec * 1000);
}

/**
 * Stop the background checkpoint loop.
 */
export function stopCheckpointing(): void {
  if (intervalId !== undefined) {
    clearInterval(intervalId);
    intervalId = undefined;
  }
}

/**
 * Create a single checkpoint (used for final checkpoint).
 */
export async function createCheckpoint(sprite: string): Promise<boolean> {
  try {
    await spriteCmdAsync(["checkpoint", "create", "-s", sprite]);
    return true;
  } catch {
    return false;
  }
}
