#pragma once

#include <string>
#include <optional>

struct Config {
    std::string sprite_token;
    std::string agent;         // "opencode" or "claude"
    std::string claude_auth;   // "subscription" or "apikey"
    std::string anthropic_api_key;
    std::string model;
    int checkpoint_interval;   // seconds
    std::string env_file;
};

// Load .env file and populate environment, then build Config from env vars.
Config load_config();

// Load a .env file: parse KEY=VALUE lines, skip comments/blanks, strip quotes, setenv.
void load_env_file(const std::string& path);

// Get an environment variable with a default fallback.
std::string env_or(const char* key, const std::string& fallback);
