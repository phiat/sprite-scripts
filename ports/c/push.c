#include "push.h"
#include "config.h"
#include "sprite.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

static void push_usage(void)
{
    fprintf(stderr,
        "Usage: sprite-tool push <local-path> <remote-path> [sprite-name]\n"
        "\n"
        "Examples:\n"
        "  sprite-tool push ./file.txt /home/sprite/file.txt\n"
        "  sprite-tool push ./mydir /home/sprite/mydir\n"
        "  sprite-tool push ./file.txt /tmp/file.txt ember-red-hawk\n");
}

int cmd_push(int argc, char **argv)
{
    if (argc < 2) {
        push_usage();
        return 1;
    }

    const char *local_path  = argv[0];
    const char *remote_path = argv[1];
    const char *sprite      = (argc >= 3) ? argv[2] : NULL;

    /* Build sprite args: if no sprite name, call without -s */
    struct stat st;
    if (stat(local_path, &st) != 0) {
        fprintf(stderr, "Error: %s does not exist\n", local_path);
        return 1;
    }

    if (S_ISDIR(st.st_mode)) {
        printf("Pushing directory: %s -> %s\n", local_path, remote_path);

        if (sprite) {
            if (push_dir(sprite, local_path, remote_path, false) != 0) {
                fprintf(stderr, "Error: push_dir failed\n");
                return 1;
            }
        } else {
            /* No sprite name -- use sprite exec without -s.
             * We handle this by building the command directly. */
            char cmd[MAX_CMD];
            snprintf(cmd, sizeof(cmd),
                "sprite exec bash -c \"mkdir -p '%s' && tar xzf - -C '%s' --strip-components=1\"",
                remote_path, remote_path);

            /* We need to pipe tar output to this command */
            char parent[MAX_PATH_LEN], base_name[MAX_PATH_LEN];
            char tmp1[MAX_PATH_LEN], tmp2[MAX_PATH_LEN];
            snprintf(tmp1, sizeof(tmp1), "%s", local_path);
            snprintf(tmp2, sizeof(tmp2), "%s", local_path);

            /* dirname and basename */
            {
                char *p = strrchr(tmp1, '/');
                if (p) {
                    *p = '\0';
                    snprintf(parent, sizeof(parent), "%s", tmp1);
                } else {
                    snprintf(parent, sizeof(parent), ".");
                }
            }
            {
                char *p = strrchr(tmp2, '/');
                if (p)
                    snprintf(base_name, sizeof(base_name), "%s", p + 1);
                else
                    snprintf(base_name, sizeof(base_name), "%s", tmp2);
            }

            char full_cmd[MAX_CMD + MAX_PATH_LEN * 2 + 64];
            snprintf(full_cmd, sizeof(full_cmd),
                "tar czf - -C '%s' '%s' | %s", parent, base_name, cmd);
            int rc = system(full_cmd);
            if (rc != 0) {
                fprintf(stderr, "Error: directory push failed\n");
                return 1;
            }
        }
    } else {
        printf("Pushing file: %s -> %s\n", local_path, remote_path);

        if (sprite) {
            if (push_file(sprite, local_path, remote_path, false) != 0) {
                fprintf(stderr, "Error: push_file failed\n");
                return 1;
            }
        } else {
            /* No sprite name -- use sprite exec without -s */
            char rdir[MAX_PATH_LEN];
            char tmp[MAX_PATH_LEN];
            snprintf(tmp, sizeof(tmp), "%s", remote_path);
            /* dirname */
            char *p = strrchr(tmp, '/');
            if (p) {
                *p = '\0';
                snprintf(rdir, sizeof(rdir), "%s", tmp);
            } else {
                snprintf(rdir, sizeof(rdir), ".");
            }

            char cmd[MAX_CMD];
            snprintf(cmd, sizeof(cmd),
                "sprite exec bash -c \"mkdir -p '%s' && cat > '%s'\" < '%s'",
                rdir, remote_path, local_path);
            int rc = system(cmd);
            if (rc != 0) {
                fprintf(stderr, "Error: file push failed\n");
                return 1;
            }
        }
    }

    printf("Done.\n");
    return 0;
}
