#include "push.h"
#include "sprite.h"

#include <cstdlib>
#include <filesystem>
#include <iostream>
#include <string>

namespace fs = std::filesystem;

static void print_usage() {
    std::cout <<
R"(Usage: sprite-tool push <local-path> <remote-path> [sprite-name]

Examples:
  sprite-tool push ./file.txt /home/sprite/file.txt
  sprite-tool push ./mydir /home/sprite/mydir
  sprite-tool push ./file.txt /tmp/file.txt ember-red-hawk
)";
}

int cmd_push(const std::string& local_path,
             const std::string& remote_path,
             const std::optional<std::string>& sprite_name) {

    if (!fs::exists(local_path)) {
        std::cerr << "Error: " << local_path << " does not exist\n";
        return 1;
    }

    std::string sargs = sprite_args(sprite_name);

    if (fs::is_directory(local_path)) {
        std::cout << "Pushing directory: " << local_path << " -> " << remote_path << "\n";

        // Determine dirname and basename of local_path
        fs::path p(local_path);
        std::string parent = p.parent_path().string();
        if (parent.empty()) parent = ".";
        std::string base = p.filename().string();

        // tar czf - -C <parent> <base> | sprite exec [-s sprite] bash -c 'mkdir -p <remote> && tar xzf - -C <remote> --strip-components=1'
        std::string cmd = "tar czf - -C '" + shell_escape(parent) + "' '" + shell_escape(base)
                          + "' | sprite exec" + sargs
                          + " bash -c 'mkdir -p '\\''"+shell_escape(remote_path)
                          +"'\\'' && tar xzf - -C '\\''"+shell_escape(remote_path)+"'\\'' --strip-components=1'";
        int ret = std::system(cmd.c_str());
        if (ret != 0) {
            std::cerr << "Error: push directory failed\n";
            return 1;
        }
    } else {
        std::cout << "Pushing file: " << local_path << " -> " << remote_path << "\n";

        // Compute dirname of remote_path
        fs::path rp(remote_path);
        std::string remote_dir = rp.parent_path().string();
        if (remote_dir.empty()) remote_dir = "/";

        // sprite exec [-s sprite] bash -c 'mkdir -p <remote_dir> && cat > <remote_path>' < local_path
        std::string cmd = "sprite exec" + sargs
                          + " bash -c 'mkdir -p '\\''"+shell_escape(remote_dir)
                          +"'\\'' && cat > '\\''"+shell_escape(remote_path)+"'\\'''";
        cmd += " < '" + shell_escape(local_path) + "'";
        int ret = std::system(cmd.c_str());
        if (ret != 0) {
            std::cerr << "Error: push file failed\n";
            return 1;
        }
    }

    std::cout << "Done.\n";
    return 0;
}
