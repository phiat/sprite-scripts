#include "launch.h"
#include "config.h"
#include "sprite.h"

#include <atomic>
#include <chrono>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <string>
#include <thread>
#include <vector>

namespace fs = std::filesystem;

// Wrapper to suppress GCC's warn_unused_result on std::system().
static inline void run(const char* cmd) { if (std::system(cmd)) {} }
static inline void run(const std::string& cmd) { run(cmd.c_str()); }

static void print_usage() {
    std::cout <<
R"(Usage: sprite-tool launch [options] <sprite-name> [plan-file]

Options:
  --dry-run              Show what would happen without executing
  --no-checkpoint        Disable auto-checkpointing
  --upload <dir>         Upload a local directory to /home/sprite/<dirname>
                         (repeatable: --upload ./data --upload ./tests)

Environment variables:
  ENV_FILE               Path to .env file (default: ./.env)
  SPRITE_TOKEN           sprites.dev API token (or SPRITES_TOKEN in .env)
  AGENT                  "opencode" (default) or "claude"
  CLAUDE_AUTH            "subscription" (default) or "apikey"
  MODEL                  Model override (see below)
  CHECKPOINT_INTERVAL    Seconds between checkpoints (default: 300 = 5min)

Model examples:
  OpenCode: MODEL=opencode/big-pickle  (free, default)
            MODEL=groq/llama-3.3-70b-versatile  (free w/ GROQ_API_KEY)
            MODEL=openai/gpt-4o
            MODEL=google/gemini-2.5-pro
  Claude:   MODEL=opus, MODEL=sonnet, MODEL=haiku

Examples:
  sprite-tool launch my-project plan.md
  sprite-tool launch --upload ./data my-project plan.md
  sprite-tool launch --upload ./data --upload ./tests my-project plan.md
  sprite-tool launch --dry-run my-project plan.md
)";
}

int cmd_launch(bool dry_run, bool no_checkpoint,
               const std::vector<std::string>& upload_dirs,
               const std::string& sprite_name,
               const std::optional<std::string>& plan_file) {

    Config cfg = load_config();
    bool checkpointing = !no_checkpoint;

    // ============================================================
    // 1. Check/install sprite CLI
    // ============================================================
    std::string which_sprite = capture("command -v sprite 2>/dev/null");
    if (which_sprite.empty()) {
        if (dry_run) {
            std::cout << "  [dry-run] Would install sprite CLI\n";
        } else {
            std::cout << "Installing sprite CLI...\n";
            run("curl -fsSL https://sprites.dev/install.sh | sh");
            // Add to PATH
            std::string home = env_or("HOME", "");
            if (!home.empty()) {
                std::string new_path = home + "/.local/bin:" + env_or("PATH", "");
                setenv("PATH", new_path.c_str(), 1);
            }
        }
    }

    // ============================================================
    // 2. Auth sprite
    // ============================================================
    if (!cfg.sprite_token.empty()) {
        std::cout << "Authenticating sprite with token...\n";
        if (!dry_run) {
            std::string cmd = "sprite auth setup --token '" + shell_escape(cfg.sprite_token) + "'";
            run(cmd.c_str());
        }
    } else {
        std::cout << "No SPRITE_TOKEN set. Running interactive login...\n";
        if (!dry_run) {
            run("sprite login");
        }
    }

    // ============================================================
    // 3. Create sprite (or use existing)
    // ============================================================
    if (dry_run) {
        std::cout << "  [dry-run] Would create or reuse sprite '" << sprite_name << "'\n";
    } else {
        std::string ls_output = capture("sprite ls 2>/dev/null");
        if (ls_output.find(sprite_name) != std::string::npos) {
            std::cout << "Sprite '" << sprite_name << "' already exists, using it.\n";
        } else {
            std::cout << "Creating sprite: " << sprite_name << "\n";
            std::string cmd = "sprite create -skip-console '" + shell_escape(sprite_name) + "'";
            run(cmd.c_str());
        }
    }

    // ============================================================
    // 4. Push .env to sprite
    // ============================================================
    if (fs::exists(cfg.env_file)) {
        std::cout << "Pushing " << cfg.env_file << "...\n";
        push_file(sprite_name, cfg.env_file, "/home/sprite/.env", dry_run);
    }

    // ============================================================
    // 5. Push plan file if provided
    // ============================================================
    if (plan_file.has_value() && !plan_file->empty() && fs::exists(*plan_file)) {
        std::cout << "Pushing " << *plan_file << "...\n";
        push_file(sprite_name, *plan_file, "/home/sprite/plan.md", dry_run);
    }

    // ============================================================
    // 6. Upload directories if provided
    // ============================================================
    for (const auto& dir : upload_dirs) {
        if (fs::is_directory(dir)) {
            std::string dirname = fs::path(dir).filename().string();
            std::string remote = "/home/sprite/" + dirname;
            std::cout << "Uploading directory: " << dir << " -> " << remote << "\n";
            push_dir(sprite_name, dir, remote, dry_run);
        } else {
            std::cout << "WARNING: --upload dir '" << dir << "' not found, skipping.\n";
        }
    }

    // ============================================================
    // 7. Setup git + beads
    // ============================================================
    std::cout << "Initializing git...\n";
    sx(sprite_name, "cd /home/sprite && git init -b main 2>/dev/null || true", dry_run);

    std::cout << "Installing beads...\n";
    sx(sprite_name, "curl -fsSL https://raw.githubusercontent.com/steveyegge/beads/main/scripts/install.sh | bash", dry_run);

    // ============================================================
    // 8. Install and auth coding agent
    // ============================================================
    if (cfg.agent == "claude") {
        std::cout << "Setting up claude...\n";
        sx(sprite_name, "command -v claude >/dev/null 2>&1 || npm install -g @anthropic-ai/claude-code", dry_run);

        if (cfg.claude_auth == "subscription") {
            std::string home = env_or("HOME", "");
            std::string creds_path = home + "/.claude/.credentials.json";
            if (fs::exists(creds_path)) {
                std::cout << "Copying claude subscription credentials...\n";
                push_file(sprite_name, creds_path, "/home/sprite/.claude/.credentials.json", dry_run);
                sx(sprite_name, "chmod 600 ~/.claude/.credentials.json", dry_run);
            } else {
                std::cerr << "ERROR: ~/.claude/.credentials.json not found\n";
                std::cerr << "Run 'claude' locally first to authenticate, then re-run this script.\n";
                return 1;
            }
        } else if (cfg.claude_auth == "apikey" && !cfg.anthropic_api_key.empty()) {
            std::cout << "Setting ANTHROPIC_API_KEY in sprite...\n";
            sx(sprite_name,
               "grep -q ANTHROPIC_API_KEY ~/.bashrc 2>/dev/null || echo 'export ANTHROPIC_API_KEY=\""
               + shell_escape(cfg.anthropic_api_key) + "\"' >> ~/.bashrc",
               dry_run);
        } else {
            std::cerr << "ERROR: No valid claude auth configured\n";
            std::cerr << "Set CLAUDE_AUTH=subscription (default) or CLAUDE_AUTH=apikey with ANTHROPIC_API_KEY\n";
            return 1;
        }

    } else if (cfg.agent == "opencode") {
        std::cout << "Setting up opencode...\n";
        sx(sprite_name, "[ -x ~/.opencode/bin/opencode ] || curl -fsSL https://opencode.ai/install | bash", dry_run);
        sx(sprite_name,
           "grep -q 'source.*\\.env' ~/.bashrc 2>/dev/null || echo '[ -f /home/sprite/.env ] && set -a && source /home/sprite/.env && set +a' >> ~/.bashrc",
           dry_run);
    } else {
        std::cerr << "ERROR: Unknown AGENT '" << cfg.agent << "'. Use 'claude' or 'opencode'.\n";
        return 1;
    }

    // ============================================================
    // 9. Launch agent with plan (or open console)
    // ============================================================
    std::cout << "\n";
    std::cout << "==========================================\n";
    std::cout << "Sprite '" << sprite_name << "' is ready!\n";
    std::cout << "Agent: " << cfg.agent;
    if (!cfg.model.empty()) {
        std::cout << " (model: " << cfg.model << ")";
    }
    std::cout << "\n";
    if (checkpointing) {
        std::cout << "Checkpointing: every " << cfg.checkpoint_interval << "s\n";
    }
    std::cout << "==========================================\n";

    if (dry_run) {
        std::cout << "\n[dry-run] Would launch " << cfg.agent << " with plan. No changes were made.\n";
        return 0;
    }

    if (plan_file.has_value() && !plan_file->empty()) {
        // Start auto-checkpointing before agent runs
        std::atomic<bool> stop_flag{false};
        std::thread checkpoint_thread;

        if (checkpointing) {
            checkpoint_thread = std::thread([&]() {
                while (!stop_flag.load()) {
                    for (int i = 0; i < cfg.checkpoint_interval && !stop_flag.load(); ++i) {
                        std::this_thread::sleep_for(std::chrono::seconds(1));
                    }
                    if (stop_flag.load()) break;

                    std::cout << "[checkpoint] Creating checkpoint...\n";
                    std::string cmd = "sprite checkpoint create -s '" + shell_escape(sprite_name) + "' 2>/dev/null";
                    int ret = std::system(cmd.c_str());
                    if (ret == 0) {
                        std::cout << "[checkpoint] Done.\n";
                    } else {
                        std::cout << "[checkpoint] Failed (non-fatal).\n";
                    }
                }
            });
            std::cout << "Auto-checkpointing every " << cfg.checkpoint_interval << "s\n";
        }

        std::cout << "Launching " << cfg.agent << " with plan...\n";

        if (cfg.agent == "claude") {
            std::string model_flag;
            if (!cfg.model.empty()) {
                model_flag = "--model " + cfg.model + " ";
            }
            sx(sprite_name,
               "cd /home/sprite && claude " + model_flag + "-p 'read plan.md and complete the plan please'",
               false);
        } else if (cfg.agent == "opencode") {
            std::string oc_model = cfg.model.empty() ? "opencode/big-pickle" : cfg.model;
            sx(sprite_name,
               "set -a && source /home/sprite/.env 2>/dev/null && set +a && cd /home/sprite && ~/.opencode/bin/opencode run -m "
               + oc_model + " 'read plan.md and complete the plan please'",
               false);
        }

        // Stop checkpointing
        stop_flag.store(true);
        if (checkpoint_thread.joinable()) {
            checkpoint_thread.join();
        }

        // Final checkpoint
        std::cout << "Creating final checkpoint...\n";
        std::string final_cmd = "sprite checkpoint create -s '" + shell_escape(sprite_name) + "' 2>/dev/null";
        int ret = std::system(final_cmd.c_str());
        if (ret == 0) {
            std::cout << "Final checkpoint saved.\n";
        } else {
            std::cout << "Final checkpoint failed (non-fatal).\n";
        }
    } else {
        std::cout << "Opening console...\n";
        std::string cmd = "sprite console -s '" + shell_escape(sprite_name) + "'";
        run(cmd.c_str());
    }

    return 0;
}
