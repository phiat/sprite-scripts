#include "pull.h"
#include "sprite.h"

#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <string>

namespace fs = std::filesystem;

static void print_usage() {
    std::cout <<
R"(Usage: sprite-tool pull <remote-path> <local-path> [sprite-name]

Examples:
  sprite-tool pull /home/sprite/file.txt ./file.txt
  sprite-tool pull /home/sprite/mydir ./mydir
  sprite-tool pull /tmp/file.txt ./file.txt ember-red-hawk
)";
}

int cmd_pull(const std::string& remote_path,
             const std::string& local_path,
             const std::optional<std::string>& sprite_name) {

    std::string sargs = sprite_args(sprite_name);

    // Check if remote path is a directory
    std::string check_cmd = "sprite exec" + sargs
                            + " bash -c '[ -d '\\''"+shell_escape(remote_path)+"'\\'' ] && echo dir || echo file'";
    std::string is_dir = capture(check_cmd);

    if (is_dir == "dir") {
        std::cout << "Pulling directory: " << remote_path << " -> " << local_path << "\n";

        // Create local directory
        fs::create_directories(local_path);

        // sprite exec [-s sprite] tar czf - -C <remote> . | tar xzf - -C <local>
        std::string cmd = "sprite exec" + sargs
                          + " tar czf - -C '" + shell_escape(remote_path) + "' ."
                          + " | tar xzf - -C '" + shell_escape(local_path) + "'";
        int ret = std::system(cmd.c_str());
        if (ret != 0) {
            std::cerr << "Error: pull directory failed\n";
            return 1;
        }
    } else {
        std::cout << "Pulling file: " << remote_path << " -> " << local_path << "\n";

        // Create local parent directory
        fs::path lp(local_path);
        if (lp.has_parent_path()) {
            fs::create_directories(lp.parent_path());
        }

        // sprite exec [-s sprite] cat <remote> > <local>
        std::string cmd = "sprite exec" + sargs
                          + " cat '" + shell_escape(remote_path) + "'"
                          + " > '" + shell_escape(local_path) + "'";
        int ret = std::system(cmd.c_str());
        if (ret != 0) {
            std::cerr << "Error: pull file failed\n";
            return 1;
        }
    }

    std::cout << "Done.\n";
    return 0;
}
