#include "config.h"

#include <cstdlib>
#include <fstream>
#include <iostream>
#include <string>

std::string env_or(const char* key, const std::string& fallback) {
    const char* val = std::getenv(key);
    if (val && val[0] != '\0') {
        return std::string(val);
    }
    return fallback;
}

static std::string trim(const std::string& s) {
    size_t start = s.find_first_not_of(" \t\r\n");
    if (start == std::string::npos) return "";
    size_t end = s.find_last_not_of(" \t\r\n");
    return s.substr(start, end - start + 1);
}

static std::string strip_quotes(const std::string& s) {
    if (s.size() >= 2) {
        char first = s.front();
        char last = s.back();
        if ((first == '"' && last == '"') || (first == '\'' && last == '\'')) {
            return s.substr(1, s.size() - 2);
        }
    }
    return s;
}

void load_env_file(const std::string& path) {
    std::ifstream file(path);
    if (!file.is_open()) {
        return;
    }

    std::string line;
    while (std::getline(file, line)) {
        std::string trimmed = trim(line);

        // Skip blank lines and comments
        if (trimmed.empty() || trimmed[0] == '#') {
            continue;
        }

        // Find first '='
        auto eq = trimmed.find('=');
        if (eq == std::string::npos) {
            continue;
        }

        std::string key = trim(trimmed.substr(0, eq));
        std::string value = trim(trimmed.substr(eq + 1));

        // Strip optional "export " prefix
        if (key.rfind("export ", 0) == 0) {
            key = trim(key.substr(7));
        }

        // Strip surrounding quotes from value
        value = strip_quotes(value);

        if (!key.empty()) {
            setenv(key.c_str(), value.c_str(), 1);
        }
    }
}

Config load_config() {
    // Source .env if present
    std::string env_file = env_or("ENV_FILE", "./.env");
    load_env_file(env_file);

    // SPRITES_TOKEN from .env is fallback for SPRITE_TOKEN
    std::string sprite_token = env_or("SPRITE_TOKEN", "");
    if (sprite_token.empty()) {
        sprite_token = env_or("SPRITES_TOKEN", "");
    }

    Config cfg;
    cfg.sprite_token = sprite_token;
    cfg.agent = env_or("AGENT", "opencode");
    cfg.claude_auth = env_or("CLAUDE_AUTH", "subscription");
    cfg.anthropic_api_key = env_or("ANTHROPIC_API_KEY", "");
    cfg.model = env_or("MODEL", "");
    cfg.env_file = env_file;

    std::string interval_str = env_or("CHECKPOINT_INTERVAL", "300");
    try {
        cfg.checkpoint_interval = std::stoi(interval_str);
    } catch (...) {
        cfg.checkpoint_interval = 300;
    }

    return cfg;
}
