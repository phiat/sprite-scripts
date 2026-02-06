#include "config.h"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Strip leading and trailing whitespace in-place. Returns pointer into buf. */
static char *strip(char *buf)
{
    while (*buf && isspace((unsigned char)*buf))
        buf++;
    if (*buf == '\0')
        return buf;
    char *end = buf + strlen(buf) - 1;
    while (end > buf && isspace((unsigned char)*end))
        *end-- = '\0';
    return buf;
}

/* Strip matching surrounding quotes (' or ") from a value in-place. */
static void strip_quotes(char *val)
{
    size_t len = strlen(val);
    if (len >= 2) {
        char first = val[0];
        char last  = val[len - 1];
        if ((first == '"' && last == '"') || (first == '\'' && last == '\'')) {
            memmove(val, val + 1, len - 2);
            val[len - 2] = '\0';
        }
    }
}

int config_load_env(const char *path)
{
    FILE *fp = fopen(path, "r");
    if (!fp)
        return -1;

    char line[MAX_LINE];
    while (fgets(line, sizeof(line), fp)) {
        /* Remove trailing newline */
        size_t len = strlen(line);
        if (len > 0 && line[len - 1] == '\n')
            line[len - 1] = '\0';

        char *p = strip(line);

        /* Skip empty lines and comments */
        if (*p == '\0' || *p == '#')
            continue;

        /* Find the '=' separator */
        char *eq = strchr(p, '=');
        if (!eq)
            continue;

        /* Split into key and value */
        *eq = '\0';
        char *key = strip(p);
        char *val = strip(eq + 1);

        if (*key == '\0')
            continue;

        strip_quotes(val);

        /* Set in environment (don't overwrite existing) */
        setenv(key, val, 0);
    }

    fclose(fp);
    return 0;
}

static const char *getenv_or(const char *name, const char *fallback)
{
    const char *v = getenv(name);
    return (v && *v) ? v : fallback;
}

void config_init(config_t *cfg)
{
    memset(cfg, 0, sizeof(*cfg));

    /* SPRITE_TOKEN with fallback to SPRITES_TOKEN */
    const char *tok = getenv("SPRITE_TOKEN");
    if (!tok || !*tok)
        tok = getenv("SPRITES_TOKEN");
    if (tok)
        snprintf(cfg->sprite_token, sizeof(cfg->sprite_token), "%s", tok);

    snprintf(cfg->agent, sizeof(cfg->agent), "%s",
             getenv_or("AGENT", "opencode"));
    snprintf(cfg->claude_auth, sizeof(cfg->claude_auth), "%s",
             getenv_or("CLAUDE_AUTH", "subscription"));

    const char *key = getenv("ANTHROPIC_API_KEY");
    if (key)
        snprintf(cfg->anthropic_api_key, sizeof(cfg->anthropic_api_key),
                 "%s", key);

    const char *model = getenv("MODEL");
    if (model)
        snprintf(cfg->model, sizeof(cfg->model), "%s", model);

    const char *ci = getenv("CHECKPOINT_INTERVAL");
    cfg->checkpoint_interval = ci ? atoi(ci) : 300;
    if (cfg->checkpoint_interval <= 0)
        cfg->checkpoint_interval = 300;

    snprintf(cfg->env_file, sizeof(cfg->env_file), "%s",
             getenv_or("ENV_FILE", "./.env"));
}
