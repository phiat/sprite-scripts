#include "sprite.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <array>

#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

std::string shell_escape(const std::string& s) {
    // Replace each single quote with: '\''
    std::string result;
    result.reserve(s.size() + 16);
    for (char c : s) {
        if (c == '\'') {
            result += "'\\''";
        } else {
            result += c;
        }
    }
    return result;
}

std::string sprite_args(const std::optional<std::string>& sprite_name) {
    if (sprite_name.has_value() && !sprite_name->empty()) {
        return " -s '" + shell_escape(*sprite_name) + "'";
    }
    return "";
}

int sx(const std::string& sprite, const std::string& cmd, bool dry_run) {
    std::string full_cmd = "sprite exec -s '" + shell_escape(sprite)
                           + "' bash -c '" + shell_escape(cmd) + "'";
    if (dry_run) {
        std::cout << "  [dry-run] " << full_cmd << "\n";
        return 0;
    }
    return std::system(full_cmd.c_str());
}

std::string capture(const std::string& cmd) {
    std::array<char, 4096> buffer;
    std::string result;
    FILE* pipe = popen(cmd.c_str(), "r");
    if (!pipe) {
        return "";
    }
    while (fgets(buffer.data(), static_cast<int>(buffer.size()), pipe) != nullptr) {
        result += buffer.data();
    }
    pclose(pipe);

    // Trim trailing whitespace
    while (!result.empty() && (result.back() == '\n' || result.back() == '\r'
                               || result.back() == ' ' || result.back() == '\t')) {
        result.pop_back();
    }
    return result;
}

int push_file(const std::string& sprite, const std::string& src, const std::string& dest, bool dry_run) {
    if (dry_run) {
        std::cout << "  [dry-run] push " << src << " -> sprite:" << dest << "\n";
        return 0;
    }

    // Compute dirname of dest
    std::string dest_dir = dest;
    auto slash = dest_dir.rfind('/');
    if (slash != std::string::npos) {
        dest_dir = dest_dir.substr(0, slash);
    }

    // Create remote directory
    sx(sprite, "mkdir -p '" + shell_escape(dest_dir) + "'", false);

    // Pipe file content via popen
    std::string cmd = "sprite exec -s '" + shell_escape(sprite)
                      + "' bash -c 'cat > '\\''"+shell_escape(dest)+"'\\'''";

    // Use shell redirection: cmd < src
    std::string full_cmd = cmd + " < '" + shell_escape(src) + "'";
    int ret = std::system(full_cmd.c_str());
    return ret;
}

int push_dir(const std::string& sprite, const std::string& src, const std::string& dest, bool dry_run) {
    if (dry_run) {
        std::cout << "  [dry-run] push dir " << src << " -> sprite:" << dest << "\n";
        return 0;
    }

    // Create remote directory
    sx(sprite, "mkdir -p '" + shell_escape(dest) + "'", false);

    // Determine dirname and basename of src
    std::string src_copy = src;
    // Remove trailing slashes
    while (src_copy.size() > 1 && src_copy.back() == '/') {
        src_copy.pop_back();
    }

    std::string src_dir;
    std::string src_base;
    auto slash = src_copy.rfind('/');
    if (slash != std::string::npos) {
        src_dir = src_copy.substr(0, slash);
        src_base = src_copy.substr(slash + 1);
    } else {
        src_dir = ".";
        src_base = src_copy;
    }

    // Compute dirname of dest for tar extraction
    std::string dest_dir = dest;
    while (dest_dir.size() > 1 && dest_dir.back() == '/') {
        dest_dir.pop_back();
    }
    auto dslash = dest_dir.rfind('/');
    std::string extract_dir;
    if (dslash != std::string::npos) {
        extract_dir = dest_dir.substr(0, dslash);
    } else {
        extract_dir = "/";
    }

    // tar czf - -C <srcdir> <srcbase> | sprite exec -s <sprite> bash -c 'tar xzf - -C <destdir>'
    std::string full_cmd = "tar czf - -C '" + shell_escape(src_dir) + "' '" + shell_escape(src_base)
                           + "' | sprite exec -s '" + shell_escape(sprite)
                           + "' bash -c 'tar xzf - -C '\\''"+shell_escape(extract_dir)+"'\\'''";
    int ret = std::system(full_cmd.c_str());
    return ret;
}
