#include "launch.h"
#include "push.h"
#include "pull.h"
#include "watch.h"

#include <cstring>
#include <iostream>
#include <optional>
#include <string>
#include <vector>

static void print_usage() {
    std::cout <<
R"(sprite-tool: CLI tool for managing sprites with coding agents

Usage: sprite-tool <command> [options]

Commands:
  launch    Create and configure a sprite with coding agent, git, beads
  push      Push local file or directory to a sprite
  pull      Pull file or directory from a sprite
  watch     Poll beads task for progress

Run 'sprite-tool <command> --help' for more information.
)";
}

static int parse_launch(int argc, char* argv[]) {
    bool dry_run = false;
    bool no_checkpoint = false;
    std::vector<std::string> upload_dirs;
    std::vector<std::string> positional;

    for (int i = 0; i < argc; ++i) {
        std::string arg = argv[i];

        if (arg == "--dry-run") {
            dry_run = true;
        } else if (arg == "--no-checkpoint") {
            no_checkpoint = true;
        } else if (arg == "--upload") {
            if (i + 1 >= argc) {
                std::cerr << "Error: --upload requires an argument\n";
                return 1;
            }
            ++i;
            upload_dirs.push_back(argv[i]);
        } else if (arg == "--help" || arg == "-h") {
            // Trigger usage by calling with no sprite name; cmd_launch won't be reached
            std::cout <<
R"(Usage: sprite-tool launch [options] <sprite-name> [plan-file]

Options:
  --dry-run              Show what would happen without executing
  --no-checkpoint        Disable auto-checkpointing
  --upload <dir>         Upload a local directory to /home/sprite/<dirname>
                         (repeatable: --upload ./data --upload ./tests)
  --help, -h             Show this help
)";
            return 0;
        } else if (arg.rfind("--", 0) == 0) {
            std::cerr << "Unknown option: " << arg << "\n";
            return 1;
        } else {
            positional.push_back(arg);
        }
    }

    if (positional.empty()) {
        std::cerr << "Error: sprite-name is required\n";
        std::cerr << "Usage: sprite-tool launch [options] <sprite-name> [plan-file]\n";
        return 1;
    }

    std::string sprite_name = positional[0];
    std::optional<std::string> plan_file;
    if (positional.size() >= 2) {
        plan_file = positional[1];
    }

    return cmd_launch(dry_run, no_checkpoint, upload_dirs, sprite_name, plan_file);
}

static int parse_push(int argc, char* argv[]) {
    // Check for help
    for (int i = 0; i < argc; ++i) {
        std::string arg = argv[i];
        if (arg == "--help" || arg == "-h") {
            std::cout <<
R"(Usage: sprite-tool push <local-path> <remote-path> [sprite-name]

Examples:
  sprite-tool push ./file.txt /home/sprite/file.txt
  sprite-tool push ./mydir /home/sprite/mydir
  sprite-tool push ./file.txt /tmp/file.txt ember-red-hawk
)";
            return 0;
        }
    }

    if (argc < 2) {
        std::cerr << "Error: local-path and remote-path are required\n";
        std::cerr << "Usage: sprite-tool push <local-path> <remote-path> [sprite-name]\n";
        return 1;
    }

    std::string local_path = argv[0];
    std::string remote_path = argv[1];
    std::optional<std::string> sprite_name;
    if (argc >= 3) {
        sprite_name = std::string(argv[2]);
    }

    return cmd_push(local_path, remote_path, sprite_name);
}

static int parse_pull(int argc, char* argv[]) {
    // Check for help
    for (int i = 0; i < argc; ++i) {
        std::string arg = argv[i];
        if (arg == "--help" || arg == "-h") {
            std::cout <<
R"(Usage: sprite-tool pull <remote-path> <local-path> [sprite-name]

Examples:
  sprite-tool pull /home/sprite/file.txt ./file.txt
  sprite-tool pull /home/sprite/mydir ./mydir
  sprite-tool pull /tmp/file.txt ./file.txt ember-red-hawk
)";
            return 0;
        }
    }

    if (argc < 2) {
        std::cerr << "Error: remote-path and local-path are required\n";
        std::cerr << "Usage: sprite-tool pull <remote-path> <local-path> [sprite-name]\n";
        return 1;
    }

    std::string remote_path = argv[0];
    std::string local_path = argv[1];
    std::optional<std::string> sprite_name;
    if (argc >= 3) {
        sprite_name = std::string(argv[2]);
    }

    return cmd_pull(remote_path, local_path, sprite_name);
}

static int parse_watch(int argc, char* argv[]) {
    // Check for help
    for (int i = 0; i < argc; ++i) {
        std::string arg = argv[i];
        if (arg == "--help" || arg == "-h") {
            std::cout <<
R"(Usage: sprite-tool watch <sprite-name> [task-id] [poll-interval]

Arguments:
  sprite-name     Name of the sprite to watch
  task-id         Beads task ID to track (default: auto-detect first open critical task)
  poll-interval   Seconds between polls (default: 30)

Examples:
  sprite-tool watch ember-red-hawk
  sprite-tool watch ember-red-hawk CRM-1
  sprite-tool watch ember-red-hawk CRM-1 60
)";
            return 0;
        }
    }

    if (argc < 1) {
        std::cerr << "Error: sprite-name is required\n";
        std::cerr << "Usage: sprite-tool watch <sprite-name> [task-id] [poll-interval]\n";
        return 1;
    }

    std::string sprite_name = argv[0];
    std::optional<std::string> task_id;
    int poll_interval = 30;

    if (argc >= 2) {
        task_id = std::string(argv[1]);
    }
    if (argc >= 3) {
        try {
            poll_interval = std::stoi(argv[2]);
        } catch (...) {
            std::cerr << "Error: invalid poll-interval '" << argv[2] << "'\n";
            return 1;
        }
    }

    return cmd_watch(sprite_name, task_id, poll_interval);
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        print_usage();
        return 1;
    }

    std::string command = argv[1];

    if (command == "--help" || command == "-h" || command == "help") {
        print_usage();
        return 0;
    }

    // Remaining args after the subcommand
    int sub_argc = argc - 2;
    char** sub_argv = argv + 2;

    if (command == "launch") {
        return parse_launch(sub_argc, sub_argv);
    } else if (command == "push") {
        return parse_push(sub_argc, sub_argv);
    } else if (command == "pull") {
        return parse_pull(sub_argc, sub_argv);
    } else if (command == "watch") {
        return parse_watch(sub_argc, sub_argv);
    } else {
        std::cerr << "Unknown command: " << command << "\n";
        print_usage();
        return 1;
    }
}
