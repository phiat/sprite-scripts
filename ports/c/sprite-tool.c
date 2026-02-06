#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "launch.h"
#include "push.h"
#include "pull.h"
#include "watch.h"

static void usage(void)
{
    fprintf(stderr,
        "Usage: sprite-tool <command> [options] [args...]\n"
        "\n"
        "Commands:\n"
        "  launch    Create and configure a sprite with a coding agent\n"
        "  push      Push a local file or directory to a sprite\n"
        "  pull      Pull a file or directory from a sprite\n"
        "  watch     Watch a sprite's beads tracker task for progress\n"
        "\n"
        "Run 'sprite-tool <command> --help' for command-specific usage.\n");
}

int main(int argc, char **argv)
{
    if (argc < 2) {
        usage();
        return 1;
    }

    const char *cmd = argv[1];

    if (strcmp(cmd, "--help") == 0 || strcmp(cmd, "-h") == 0) {
        usage();
        return 0;
    }

    /* Shift argv past the subcommand name:
     * argv[0] = program name, argv[1] = subcommand
     * Pass argv+2 with argc-2 to the subcommand handler.
     */
    int sub_argc = argc - 2;
    char **sub_argv = argv + 2;

    if (strcmp(cmd, "launch") == 0) {
        return cmd_launch(sub_argc, sub_argv);
    } else if (strcmp(cmd, "push") == 0) {
        return cmd_push(sub_argc, sub_argv);
    } else if (strcmp(cmd, "pull") == 0) {
        return cmd_pull(sub_argc, sub_argv);
    } else if (strcmp(cmd, "watch") == 0) {
        return cmd_watch(sub_argc, sub_argv);
    } else {
        fprintf(stderr, "Unknown command: %s\n\n", cmd);
        usage();
        return 1;
    }
}
