#include "pull.h"
#include "config.h"
#include "sprite.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

static void pull_usage(void)
{
    fprintf(stderr,
        "Usage: sprite-tool pull <remote-path> <local-path> [sprite-name]\n"
        "\n"
        "Examples:\n"
        "  sprite-tool pull /home/sprite/file.txt ./file.txt\n"
        "  sprite-tool pull /home/sprite/mydir ./mydir\n"
        "  sprite-tool pull /tmp/file.txt ./file.txt ember-red-hawk\n");
}

int cmd_pull(int argc, char **argv)
{
    if (argc < 2) {
        pull_usage();
        return 1;
    }

    const char *remote_path = argv[0];
    const char *local_path  = argv[1];
    const char *sprite      = (argc >= 3) ? argv[2] : NULL;

    /* Detect whether remote path is a directory or file */
    int is_dir;

    if (sprite) {
        is_dir = remote_is_dir(sprite, remote_path);
    } else {
        /* No sprite name: use sprite exec without -s */
        char cmd[MAX_CMD];
        snprintf(cmd, sizeof(cmd),
            "sprite exec bash -c '[ -d \"%s\" ] && echo dir || echo file' 2>/dev/null",
            remote_path);

        FILE *fp = popen(cmd, "r");
        if (!fp) {
            fprintf(stderr, "Error: cannot detect remote path type\n");
            return 1;
        }

        char result[64] = {0};
        if (fgets(result, sizeof(result), fp) != NULL) {
            /* Strip trailing newline */
            size_t len = strlen(result);
            while (len > 0 && (result[len - 1] == '\n' || result[len - 1] == '\r'))
                result[--len] = '\0';
        }
        pclose(fp);

        if (strcmp(result, "dir") == 0)
            is_dir = 1;
        else
            is_dir = 0;
    }

    if (is_dir == 1) {
        printf("Pulling directory: %s -> %s\n", remote_path, local_path);

        if (sprite) {
            if (pull_dir(sprite, remote_path, local_path) != 0) {
                fprintf(stderr, "Error: pull_dir failed\n");
                return 1;
            }
        } else {
            /* mkdir -p local_path, then pipe */
            char mkdir_cmd[MAX_CMD];
            snprintf(mkdir_cmd, sizeof(mkdir_cmd), "mkdir -p '%s'", local_path);
            system(mkdir_cmd);

            char cmd[MAX_CMD];
            snprintf(cmd, sizeof(cmd),
                "sprite exec tar czf - -C '%s' . | tar xzf - -C '%s'",
                remote_path, local_path);
            int rc = system(cmd);
            if (rc != 0) {
                fprintf(stderr, "Error: directory pull failed\n");
                return 1;
            }
        }
    } else {
        printf("Pulling file: %s -> %s\n", remote_path, local_path);

        if (sprite) {
            if (pull_file(sprite, remote_path, local_path) != 0) {
                fprintf(stderr, "Error: pull_file failed\n");
                return 1;
            }
        } else {
            /* Ensure local parent dir exists */
            char tmp[MAX_PATH_LEN];
            snprintf(tmp, sizeof(tmp), "%s", local_path);
            char *p = strrchr(tmp, '/');
            if (p) {
                *p = '\0';
                char mkdir_cmd[MAX_CMD];
                snprintf(mkdir_cmd, sizeof(mkdir_cmd), "mkdir -p '%s'", tmp);
                system(mkdir_cmd);
            }

            char cmd[MAX_CMD];
            snprintf(cmd, sizeof(cmd),
                "sprite exec cat '%s' > '%s'",
                remote_path, local_path);
            int rc = system(cmd);
            if (rc != 0) {
                fprintf(stderr, "Error: file pull failed\n");
                return 1;
            }
        }
    }

    printf("Done.\n");
    return 0;
}
