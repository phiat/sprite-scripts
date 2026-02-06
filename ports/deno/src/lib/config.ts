/**
 * config.ts — .env loading and configuration defaults
 */

export interface Config {
  spriteToken: string;
  agent: string;
  claudeAuth: string;
  anthropicApiKey: string;
  model: string;
  checkpointInterval: number;
  envFile: string;
}

/**
 * Load a .env file: read lines, skip blanks and comments,
 * parse KEY=VALUE, strip surrounding quotes, set into Deno.env.
 */
export function loadEnvFile(path: string): void {
  let content: string;
  try {
    content = Deno.readTextFileSync(path);
  } catch {
    // File doesn't exist — silently skip
    return;
  }

  for (const raw of content.split("\n")) {
    const line = raw.trim();
    if (line === "" || line.startsWith("#")) continue;

    const eqIdx = line.indexOf("=");
    if (eqIdx === -1) continue;

    const key = line.slice(0, eqIdx).trim();
    let value = line.slice(eqIdx + 1).trim();

    // Strip matched surrounding quotes (single or double)
    if (
      (value.startsWith('"') && value.endsWith('"')) ||
      (value.startsWith("'") && value.endsWith("'"))
    ) {
      value = value.slice(1, -1);
    }

    Deno.env.set(key, value);
  }
}

/**
 * Build the Config struct from environment variables,
 * applying defaults and SPRITES_TOKEN fallback.
 */
export function loadConfig(): Config {
  const envFile = Deno.env.get("ENV_FILE") || "./.env";

  // Load .env first so its values are available
  loadEnvFile(envFile);

  // SPRITES_TOKEN from .env is the fallback for SPRITE_TOKEN
  const spriteToken =
    Deno.env.get("SPRITE_TOKEN") || Deno.env.get("SPRITES_TOKEN") || "";

  return {
    spriteToken,
    agent: Deno.env.get("AGENT") || "opencode",
    claudeAuth: Deno.env.get("CLAUDE_AUTH") || "subscription",
    anthropicApiKey: Deno.env.get("ANTHROPIC_API_KEY") || "",
    model: Deno.env.get("MODEL") || "",
    checkpointInterval: parseInt(
      Deno.env.get("CHECKPOINT_INTERVAL") || "300",
      10,
    ),
    envFile,
  };
}
