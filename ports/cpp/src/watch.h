#pragma once

#include <string>
#include <optional>

int cmd_watch(const std::string& sprite_name,
              const std::optional<std::string>& task_id,
              int poll_interval);
