#include "watch.h"
#include "sprite.h"

#include <chrono>
#include <cstdlib>
#include <iostream>
#include <string>
#include <thread>

static void print_usage() {
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
}

// Run a command in the sprite and capture output
static std::string sx_capture(const std::string& sprite, const std::string& cmd) {
    std::string full_cmd = "sprite exec -s '" + shell_escape(sprite)
                           + "' bash -c '" + shell_escape(cmd) + "' 2>/dev/null";
    return capture(full_cmd);
}

int cmd_watch(const std::string& sprite_name,
              const std::optional<std::string>& task_id_opt,
              int poll_interval) {

    std::string task_id;

    // Auto-detect tracker task if not specified
    if (task_id_opt.has_value() && !task_id_opt->empty()) {
        task_id = *task_id_opt;
    } else {
        std::cout << "Detecting tracker task...\n";

        task_id = sx_capture(sprite_name,
            "cd /home/sprite && bd list --priority critical 2>/dev/null | head -1 | awk '{print $1}'");

        if (task_id.empty()) {
            std::cout << "No critical task found. Falling back to first open task...\n";
            task_id = sx_capture(sprite_name,
                "cd /home/sprite && bd list 2>/dev/null | head -1 | awk '{print $1}'");
        }

        if (task_id.empty()) {
            std::cerr << "ERROR: No beads tasks found on sprite '" << sprite_name << "'\n";
            std::cerr << "Specify a task ID manually: sprite-tool watch " << sprite_name << " <task-id>\n";
            return 1;
        }

        std::cout << "Tracking task: " << task_id << "\n";
    }

    std::cout << "Watching sprite '" << sprite_name << "' task '" << task_id
              << "' (every " << poll_interval << "s)\n";
    std::cout << "Press Ctrl+C to stop\n\n";

    while (true) {
        // Clear screen and home cursor
        std::cout << "\033[2J\033[H";
        std::cout << std::flush;

        // Get current time
        auto now = std::chrono::system_clock::now();
        std::time_t now_t = std::chrono::system_clock::to_time_t(now);
        char time_buf[16];
        std::strftime(time_buf, sizeof(time_buf), "%H:%M:%S", std::localtime(&now_t));

        std::cout << "=== sprite-watch: " << sprite_name << " / " << task_id
                  << " === " << time_buf << " ===\n\n";

        // Show task status
        std::string task_output = sx_capture(sprite_name,
            "cd /home/sprite && bd show " + task_id + " 2>/dev/null");
        if (task_output.empty()) {
            std::cout << "(could not read task)\n";
        } else {
            std::cout << task_output << "\n";
        }
        std::cout << "\n";

        // Show recent comments
        std::cout << "--- Recent updates ---\n";
        std::string comments = sx_capture(sprite_name,
            "cd /home/sprite && bd comments " + task_id + " 2>/dev/null | tail -8");
        if (comments.empty()) {
            std::cout << "(no comments)\n";
        } else {
            std::cout << comments << "\n";
        }
        std::cout << "\n";

        // Check if done
        std::string status = sx_capture(sprite_name,
            "cd /home/sprite && bd show " + task_id + " 2>/dev/null | grep -i status");

        // Check for closed/done/completed (case insensitive)
        std::string status_lower = status;
        for (auto& c : status_lower) {
            c = static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
        }
        if (status_lower.find("closed") != std::string::npos ||
            status_lower.find("done") != std::string::npos ||
            status_lower.find("completed") != std::string::npos) {
            std::cout << "==========================================\n";
            std::cout << "PROJECT COMPLETE\n";
            std::cout << "==========================================\n";
            break;
        }

        std::this_thread::sleep_for(std::chrono::seconds(poll_interval));
    }

    return 0;
}
