#ifndef CONFIG_H
#define CONFIG_H

#include <stdbool.h>

#define MAX_UPLOADS 32
#define MAX_LINE    4096
#define MAX_CMD     8192
#define MAX_PATH_LEN 4096

/* Configuration loaded from environment and .env file */
typedef struct {
    char sprite_token[MAX_LINE];
    char agent[64];            /* "opencode" or "claude" */
    char claude_auth[64];      /* "subscription" or "apikey" */
    char anthropic_api_key[MAX_LINE];
    char model[256];
    int  checkpoint_interval;  /* seconds, default 300 */
    char env_file[MAX_PATH_LEN];
} config_t;

/*
 * Load .env file: parse KEY=VALUE lines, skip comments/blanks,
 * strip surrounding quotes, call setenv().
 * Returns 0 on success, -1 if file not found (non-fatal).
 */
int config_load_env(const char *path);

/*
 * Populate config_t from current environment variables.
 * Call after config_load_env() so .env values are visible.
 */
void config_init(config_t *cfg);

#endif /* CONFIG_H */
