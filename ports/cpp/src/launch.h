#pragma once

#include <string>
#include <vector>
#include <optional>

int cmd_launch(bool dry_run, bool no_checkpoint,
               const std::vector<std::string>& upload_dirs,
               const std::string& sprite_name,
               const std::optional<std::string>& plan_file);
