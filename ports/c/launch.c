#include "launch.h"
#include "config.h"
#include "sprite.h"

#include <getopt.h>
#include <libgen.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

/* Checkpoint child PID -- file-scope for signal handler */
static volatile pid_t g_checkpoint_pid = 0;

static void cleanup_handler(int sig)
{
    (void)sig;
    if (g_checkpoint_pid > 0) {
        kill(g_checkpoint_pid, SIGTERM);
        waitpid(g_checkpoint_pid, NULL, 0);
        g_checkpoint_pid = 0;
    }
}

static void launch_usage(void)
{
    fprintf(stderr,
        "Usage: sprite-tool launch [options] <sprite-name> [plan-file]\n"
        "\n"
        "Options:\n"
        "  --dry-run              Show what would happen without executing\n"
        "  --no-checkpoint        Disable auto-checkpointing\n"
        "  --upload <dir>         Upload a local directory to /home/sprite/<dirname>\n"
        "                         (repeatable: --upload ./data --upload ./tests)\n"
        "  --help                 Show this help\n"
        "\n"
        "Environment variables:\n"
        "  ENV_FILE               Path to .env file (default: ./.env)\n"
        "  SPRITE_TOKEN           sprites.dev API token (or SPRITES_TOKEN in .env)\n"
        "  AGENT                  \"opencode\" (default) or \"claude\"\n"
        "  CLAUDE_AUTH            \"subscription\" (default) or \"apikey\"\n"
        "  MODEL                  Model override\n"
        "  CHECKPOINT_INTERVAL    Seconds between checkpoints (default: 300)\n"
        "\n"
        "Examples:\n"
        "  sprite-tool launch my-project plan.md\n"
        "  sprite-tool launch --upload ./data my-project plan.md\n"
        "  sprite-tool launch --dry-run my-project plan.md\n");
}

static void start_checkpointing(const char *sprite, int interval)
{
    pid_t pid = fork();
    if (pid < 0) {
        perror("fork (checkpointing)");
        return;
    }

    if (pid == 0) {
        /* Child: checkpoint loop */
        char cmd[MAX_CMD];
        for (;;) {
            sleep((unsigned)interval);

            time_t now = time(NULL);
            struct tm *tm = localtime(&now);
            char timebuf[16];
            strftime(timebuf, sizeof(timebuf), "%H:%M:%S", tm);

            fprintf(stderr, "[checkpoint] Creating checkpoint at %s...\n",
                    timebuf);
            snprintf(cmd, sizeof(cmd),
                     "sprite checkpoint create -s '%s' 2>/dev/null", sprite);
            int rc = system(cmd);
            if (rc == 0)
                fprintf(stderr, "[checkpoint] Done.\n");
            else
                fprintf(stderr, "[checkpoint] Failed (non-fatal).\n");
        }
        _exit(0); /* unreachable */
    }

    g_checkpoint_pid = pid;
    printf("Auto-checkpointing every %ds (pid: %d)\n", interval, (int)pid);
}

static void stop_checkpointing(void)
{
    if (g_checkpoint_pid > 0) {
        kill(g_checkpoint_pid, SIGTERM);
        waitpid(g_checkpoint_pid, NULL, 0);
        g_checkpoint_pid = 0;
    }
}

int cmd_launch(int argc, char **argv)
{
    /* Reset optind for getopt_long */
    optind = 1;

    bool dry_run = false;
    bool checkpointing = true;
    char *upload_dirs[MAX_UPLOADS];
    int upload_count = 0;

    static struct option long_opts[] = {
        {"dry-run",       no_argument,       NULL, 'd'},
        {"no-checkpoint", no_argument,       NULL, 'n'},
        {"upload",        required_argument, NULL, 'u'},
        {"help",          no_argument,       NULL, 'h'},
        {NULL, 0, NULL, 0}
    };

    int opt;
    while ((opt = getopt_long(argc, argv, "h", long_opts, NULL)) != -1) {
        switch (opt) {
        case 'd':
            dry_run = true;
            break;
        case 'n':
            checkpointing = false;
            break;
        case 'u':
            if (upload_count >= MAX_UPLOADS) {
                fprintf(stderr, "Error: too many --upload dirs (max %d)\n",
                        MAX_UPLOADS);
                return 1;
            }
            upload_dirs[upload_count++] = optarg;
            break;
        case 'h':
            launch_usage();
            return 0;
        default:
            launch_usage();
            return 1;
        }
    }

    if (optind >= argc) {
        fprintf(stderr, "Error: sprite-name is required\n");
        launch_usage();
        return 1;
    }

    const char *sprite_name = argv[optind];
    const char *plan_file   = (optind + 1 < argc) ? argv[optind + 1] : NULL;

    /* ---- Load config ---- */
    config_t cfg;
    config_init(&cfg);         /* get defaults from current env */
    config_load_env(cfg.env_file); /* load .env, won't overwrite existing */
    config_init(&cfg);         /* re-read now that .env vars are loaded */

    /* ---- 1. Check/install sprite CLI ---- */
    if (system("command -v sprite >/dev/null 2>&1") != 0) {
        if (dry_run) {
            printf("  [dry-run] Would install sprite CLI\n");
        } else {
            printf("Installing sprite CLI...\n");
            int rc = system("curl -fsSL https://sprites.dev/install.sh | sh");
            if (rc != 0) {
                fprintf(stderr, "Error: sprite CLI installation failed\n");
                return 1;
            }
            /* Add to PATH */
            char *home = getenv("HOME");
            if (home) {
                char newpath[MAX_PATH_LEN];
                snprintf(newpath, sizeof(newpath), "%s/.local/bin:%s",
                         home, getenv("PATH") ? getenv("PATH") : "");
                setenv("PATH", newpath, 1);
            }
        }
    }

    /* ---- 2. Auth sprite ---- */
    if (cfg.sprite_token[0]) {
        printf("Authenticating sprite with token...\n");
        if (!dry_run) {
            char cmd[MAX_CMD];
            snprintf(cmd, sizeof(cmd),
                     "sprite auth setup --token '%s'", cfg.sprite_token);
            system(cmd);
        }
    } else {
        printf("No SPRITE_TOKEN set. Running interactive login...\n");
        if (!dry_run) {
            system("sprite login");
        }
    }

    /* ---- 3. Create sprite (or reuse existing) ---- */
    if (dry_run) {
        printf("  [dry-run] Would create or reuse sprite '%s'\n", sprite_name);
    } else {
        char check_cmd[MAX_CMD];
        snprintf(check_cmd, sizeof(check_cmd),
                 "sprite ls 2>/dev/null | grep -qw '%s'", sprite_name);
        if (system(check_cmd) == 0) {
            printf("Sprite '%s' already exists, using it.\n", sprite_name);
        } else {
            printf("Creating sprite: %s\n", sprite_name);
            char create_cmd[MAX_CMD];
            snprintf(create_cmd, sizeof(create_cmd),
                     "sprite create -skip-console '%s'", sprite_name);
            int rc = system(create_cmd);
            if (rc != 0) {
                fprintf(stderr, "Error: failed to create sprite\n");
                return 1;
            }
        }
    }

    /* ---- 4. Push .env ---- */
    {
        struct stat st;
        if (stat(cfg.env_file, &st) == 0) {
            printf("Pushing %s...\n", cfg.env_file);
            push_file(sprite_name, cfg.env_file, "/home/sprite/.env", dry_run);
        }
    }

    /* ---- 5. Push plan file ---- */
    if (plan_file) {
        struct stat st;
        if (stat(plan_file, &st) == 0) {
            printf("Pushing %s...\n", plan_file);
            push_file(sprite_name, plan_file, "/home/sprite/plan.md", dry_run);
        } else {
            fprintf(stderr, "Warning: plan file '%s' not found\n", plan_file);
        }
    }

    /* ---- 6. Upload directories ---- */
    for (int i = 0; i < upload_count; i++) {
        struct stat st;
        if (stat(upload_dirs[i], &st) == 0 && S_ISDIR(st.st_mode)) {
            /* Get basename for remote path */
            char tmp[MAX_PATH_LEN];
            snprintf(tmp, sizeof(tmp), "%s", upload_dirs[i]);
            char *bname = basename(tmp);

            char remote[MAX_PATH_LEN];
            snprintf(remote, sizeof(remote), "/home/sprite/%s", bname);

            printf("Uploading directory: %s -> %s\n", upload_dirs[i], remote);
            push_dir(sprite_name, upload_dirs[i], remote, dry_run);
        } else {
            fprintf(stderr, "WARNING: --upload dir '%s' not found, skipping.\n",
                    upload_dirs[i]);
        }
    }

    /* ---- 7. Setup git + beads ---- */
    printf("Initializing git...\n");
    sx(sprite_name, "cd /home/sprite && git init -b main 2>/dev/null || true",
       dry_run);

    printf("Installing beads...\n");
    sx(sprite_name,
       "curl -fsSL https://raw.githubusercontent.com/steveyegge/beads/main/scripts/install.sh | bash",
       dry_run);

    /* ---- 8. Install and auth coding agent ---- */
    if (strcmp(cfg.agent, "claude") == 0) {
        printf("Setting up claude...\n");
        sx(sprite_name,
           "command -v claude >/dev/null 2>&1 || npm install -g @anthropic-ai/claude-code",
           dry_run);

        if (strcmp(cfg.claude_auth, "subscription") == 0) {
            char cred_path[MAX_PATH_LEN];
            char *home = getenv("HOME");
            if (!home) home = "/root";
            snprintf(cred_path, sizeof(cred_path),
                     "%s/.claude/.credentials.json", home);

            struct stat st;
            if (stat(cred_path, &st) == 0) {
                printf("Copying claude subscription credentials...\n");
                push_file(sprite_name, cred_path,
                          "/home/sprite/.claude/.credentials.json", dry_run);
                sx(sprite_name, "chmod 600 ~/.claude/.credentials.json",
                   dry_run);
            } else {
                fprintf(stderr,
                    "ERROR: ~/.claude/.credentials.json not found\n"
                    "Run 'claude' locally first to authenticate, "
                    "then re-run this script.\n");
                return 1;
            }
        } else if (strcmp(cfg.claude_auth, "apikey") == 0 &&
                   cfg.anthropic_api_key[0]) {
            printf("Setting ANTHROPIC_API_KEY in sprite...\n");
            char cmd[MAX_CMD];
            snprintf(cmd, sizeof(cmd),
                "grep -q ANTHROPIC_API_KEY ~/.bashrc 2>/dev/null || "
                "echo '\"'\"'export ANTHROPIC_API_KEY=\"%s\"'\"'\"' >> ~/.bashrc",
                cfg.anthropic_api_key);
            sx(sprite_name, cmd, dry_run);
        } else {
            fprintf(stderr,
                "ERROR: No valid claude auth configured\n"
                "Set CLAUDE_AUTH=subscription (default) or "
                "CLAUDE_AUTH=apikey with ANTHROPIC_API_KEY\n");
            return 1;
        }
    } else if (strcmp(cfg.agent, "opencode") == 0) {
        printf("Setting up opencode...\n");
        sx(sprite_name,
           "[ -x ~/.opencode/bin/opencode ] || curl -fsSL https://opencode.ai/install | bash",
           dry_run);
        sx(sprite_name,
           "grep -q 'source.*\\.env' ~/.bashrc 2>/dev/null || "
           "echo '[ -f /home/sprite/.env ] && set -a && source /home/sprite/.env && set +a' >> ~/.bashrc",
           dry_run);
    } else {
        fprintf(stderr, "ERROR: Unknown AGENT '%s'. Use 'claude' or 'opencode'.\n",
                cfg.agent);
        return 1;
    }

    /* ---- 9. Launch agent ---- */
    printf("\n");
    printf("==========================================\n");
    printf("Sprite '%s' is ready!\n", sprite_name);
    printf("Agent: %s", cfg.agent);
    if (cfg.model[0])
        printf(" (model: %s)", cfg.model);
    printf("\n");
    if (checkpointing)
        printf("Checkpointing: every %ds\n", cfg.checkpoint_interval);
    printf("==========================================\n");

    if (dry_run) {
        printf("\n[dry-run] Would launch %s with plan. No changes were made.\n",
               cfg.agent);
        return 0;
    }

    if (plan_file) {
        /* Install signal handler for cleanup */
        struct sigaction sa;
        memset(&sa, 0, sizeof(sa));
        sa.sa_handler = cleanup_handler;
        sigaction(SIGINT, &sa, NULL);
        sigaction(SIGTERM, &sa, NULL);

        /* Start auto-checkpointing */
        if (checkpointing)
            start_checkpointing(sprite_name, cfg.checkpoint_interval);

        printf("Launching %s with plan...\n", cfg.agent);

        if (strcmp(cfg.agent, "claude") == 0) {
            char cmd[MAX_CMD];
            if (cfg.model[0]) {
                snprintf(cmd, sizeof(cmd),
                    "cd /home/sprite && claude --model %s -p "
                    "'\"'\"'read plan.md and complete the plan please'\"'\"'",
                    cfg.model);
            } else {
                snprintf(cmd, sizeof(cmd),
                    "cd /home/sprite && claude -p "
                    "'\"'\"'read plan.md and complete the plan please'\"'\"'");
            }
            sx(sprite_name, cmd, false);
        } else if (strcmp(cfg.agent, "opencode") == 0) {
            char oc_model[256];
            if (cfg.model[0])
                snprintf(oc_model, sizeof(oc_model), "%s", cfg.model);
            else
                snprintf(oc_model, sizeof(oc_model), "opencode/big-pickle");

            char cmd[MAX_CMD];
            snprintf(cmd, sizeof(cmd),
                "set -a && source /home/sprite/.env 2>/dev/null && set +a && "
                "cd /home/sprite && ~/.opencode/bin/opencode run -m %s "
                "'\"'\"'read plan.md and complete the plan please'\"'\"'",
                oc_model);
            sx(sprite_name, cmd, false);
        }

        /* Final checkpoint */
        stop_checkpointing();
        printf("Creating final checkpoint...\n");
        char final_cmd[MAX_CMD];
        snprintf(final_cmd, sizeof(final_cmd),
                 "sprite checkpoint create -s '%s' 2>/dev/null", sprite_name);
        if (system(final_cmd) == 0)
            printf("Final checkpoint saved.\n");
        else
            printf("Final checkpoint failed (non-fatal).\n");
    } else {
        printf("Opening console...\n");
        char cmd[MAX_CMD];
        snprintf(cmd, sizeof(cmd), "sprite console -s '%s'", sprite_name);
        system(cmd);
    }

    return 0;
}
