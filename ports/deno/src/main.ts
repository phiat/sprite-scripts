/**
 * main.ts — Entry point and command dispatch for sprite-tool
 *
 * Usage: sprite-tool <command> [args...]
 *
 * Commands:
 *   launch   Create and configure a sprite with coding agent, git, beads
 *   push     Push local file or directory to a sprite
 *   pull     Pull file or directory from a sprite
 *   watch    Poll a sprite's beads tracker task for progress
 */

import { launch } from "./commands/launch.ts";
import { push } from "./commands/push.ts";
import { pull } from "./commands/pull.ts";
import { watch } from "./commands/watch.ts";

function usage(): never {
  console.log(`sprite-tool — manage sprites.dev cloud containers

Usage: sprite-tool <command> [args...]

Commands:
  launch   Create and configure a sprite with coding agent, git, beads
  push     Push local file or directory to a sprite
  pull     Pull file or directory from a sprite
  watch    Poll a sprite's beads tracker task for progress

Run 'sprite-tool <command> --help' for command-specific usage.`);
  Deno.exit(1);
}

async function main(): Promise<void> {
  const args = Deno.args;

  if (args.length === 0) {
    usage();
  }

  const command = args[0];
  const subArgs = args.slice(1);

  switch (command) {
    case "launch":
      await launch(subArgs);
      break;
    case "push":
      await push(subArgs);
      break;
    case "pull":
      await pull(subArgs);
      break;
    case "watch":
      await watch(subArgs);
      break;
    case "--help":
    case "-h":
      usage();
      break;
    default:
      console.error(`Unknown command: ${command}`);
      usage();
  }
}

main();
