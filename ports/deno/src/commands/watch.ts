/**
 * watch.ts — Poll a sprite's beads tracker task for progress
 *
 * Usage: sprite-tool watch <sprite-name> [task-id] [poll-interval]
 */

import { sxAsync } from "../lib/sprite.ts";

function usage(): never {
  console.log(`Usage: sprite-tool watch <sprite-name> [task-id] [poll-interval]

Arguments:
  sprite-name     Name of the sprite to watch
  task-id         Beads task ID to track (default: auto-detect first open critical task)
  poll-interval   Seconds between polls (default: 30)

Examples:
  sprite-tool watch ember-red-hawk
  sprite-tool watch ember-red-hawk CRM-1
  sprite-tool watch ember-red-hawk CRM-1 60`);
  Deno.exit(1);
}

function delay(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

export async function watch(args: string[]): Promise<void> {
  if (args.length < 1) {
    usage();
  }

  const sprite = args[0];
  let taskId = args[1] || "";
  const pollInterval = parseInt(args[2] || "30", 10);

  // Auto-detect tracker task if not specified
  if (!taskId) {
    console.log("Detecting tracker task...");

    // Try critical tasks first
    taskId = (
      await sxAsync(
        sprite,
        "cd /home/sprite && bd list --priority critical 2>/dev/null | head -1 | awk '{print $1}'",
        { ignoreError: true },
      )
    ).trim();

    if (!taskId) {
      console.log("No critical task found. Falling back to first open task...");
      taskId = (
        await sxAsync(
          sprite,
          "cd /home/sprite && bd list 2>/dev/null | head -1 | awk '{print $1}'",
          { ignoreError: true },
        )
      ).trim();
    }

    if (!taskId) {
      console.error(`ERROR: No beads tasks found on sprite '${sprite}'`);
      console.error(
        `Specify a task ID manually: sprite-tool watch ${sprite} <task-id>`,
      );
      Deno.exit(1);
    }

    console.log(`Tracking task: ${taskId}`);
  }

  console.log(
    `Watching sprite '${sprite}' task '${taskId}' (every ${pollInterval}s)`,
  );
  console.log("Press Ctrl+C to stop");
  console.log("");

  while (true) {
    // Clear screen and move cursor to top-left
    console.log("\x1b[2J\x1b[H");

    const now = new Date();
    const ts = now.toTimeString().slice(0, 8);
    console.log(`=== sprite-watch: ${sprite} / ${taskId} === ${ts} ===`);
    console.log("");

    // Show task status
    const taskOutput = await sxAsync(
      sprite,
      `cd /home/sprite && bd show ${taskId} 2>/dev/null`,
      { ignoreError: true },
    );
    console.log(taskOutput || "(could not read task)");
    console.log("");

    // Show recent comments
    console.log("--- Recent updates ---");
    const comments = await sxAsync(
      sprite,
      `cd /home/sprite && bd comments ${taskId} 2>/dev/null | tail -8`,
      { ignoreError: true },
    );
    console.log(comments || "(no comments)");
    console.log("");

    // Check if done
    const status = await sxAsync(
      sprite,
      `cd /home/sprite && bd show ${taskId} 2>/dev/null | grep -i status`,
      { ignoreError: true },
    );

    if (/closed|done|completed/i.test(status)) {
      console.log("==========================================");
      console.log("PROJECT COMPLETE");
      console.log("==========================================");
      break;
    }

    await delay(pollInterval * 1000);
  }
}
