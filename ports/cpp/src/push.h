#pragma once

#include <string>
#include <optional>

int cmd_push(const std::string& local_path,
             const std::string& remote_path,
             const std::optional<std::string>& sprite_name);
