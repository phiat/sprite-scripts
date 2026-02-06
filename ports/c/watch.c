#include "watch.h"
#include "config.h"
#include "sprite.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

static void watch_usage(void)
{
    fprintf(stderr,
        "Usage: sprite-tool watch <sprite-name> [task-id] [poll-interval]\n"
        "\n"
        "Arguments:\n"
        "  sprite-name     Name of the sprite to watch\n"
        "  task-id         Beads task ID to track (default: auto-detect)\n"
        "  poll-interval   Seconds between polls (default: 30)\n"
        "\n"
        "Examples:\n"
        "  sprite-tool watch ember-red-hawk\n"
        "  sprite-tool watch ember-red-hawk CRM-1\n"
        "  sprite-tool watch ember-red-hawk CRM-1 60\n");
}

int cmd_watch(int argc, char **argv)
{
    /* argv[0] is the subcommand name; real args start at argv[1] */
    if (argc < 2) {
        watch_usage();
        return 1;
    }

    const char *sprite = argv[1];
    char task_id[256] = {0};
    int poll_interval = 30;

    if (argc >= 3)
        snprintf(task_id, sizeof(task_id), "%s", argv[2]);
    if (argc >= 4) {
        poll_interval = atoi(argv[3]);
        if (poll_interval <= 0)
            poll_interval = 30;
    }

    /* Auto-detect task ID if not specified */
    if (task_id[0] == '\0') {
        printf("Detecting tracker task...\n");

        int rc = sx_capture(sprite,
            "cd /home/sprite && bd list --priority critical 2>/dev/null | head -1 | awk '{print $1}'",
            task_id, sizeof(task_id));

        if (rc != 0 || task_id[0] == '\0') {
            printf("No critical task found. Falling back to first open task...\n");
            rc = sx_capture(sprite,
                "cd /home/sprite && bd list 2>/dev/null | head -1 | awk '{print $1}'",
                task_id, sizeof(task_id));
        }

        if (rc != 0 || task_id[0] == '\0') {
            fprintf(stderr,
                "ERROR: No beads tasks found on sprite '%s'\n"
                "Specify a task ID manually: sprite-tool watch %s <task-id>\n",
                sprite, sprite);
            return 1;
        }

        printf("Tracking task: %s\n", task_id);
    }

    printf("Watching sprite '%s' task '%s' (every %ds)\n",
           sprite, task_id, poll_interval);
    printf("Press Ctrl+C to stop\n\n");

    for (;;) {
        /* Clear screen and move cursor to top-left */
        printf("\033[2J\033[H");

        time_t now = time(NULL);
        struct tm *tm = localtime(&now);
        char timebuf[16];
        strftime(timebuf, sizeof(timebuf), "%H:%M:%S", tm);

        printf("=== sprite-watch: %s / %s === %s ===\n\n",
               sprite, task_id, timebuf);

        /* Show task status */
        char show_cmd[MAX_CMD];
        snprintf(show_cmd, sizeof(show_cmd),
                 "cd /home/sprite && bd show %s 2>/dev/null", task_id);
        if (sx(sprite, show_cmd, false) != 0)
            printf("(could not read task)\n");
        printf("\n");

        /* Show recent comments */
        printf("--- Recent updates ---\n");
        char comments_cmd[MAX_CMD];
        snprintf(comments_cmd, sizeof(comments_cmd),
                 "cd /home/sprite && bd comments %s 2>/dev/null | tail -8",
                 task_id);
        if (sx(sprite, comments_cmd, false) != 0)
            printf("(no comments)\n");
        printf("\n");

        /* Check if done */
        char status_buf[MAX_LINE];
        char status_cmd[MAX_CMD];
        snprintf(status_cmd, sizeof(status_cmd),
                 "cd /home/sprite && bd show %s 2>/dev/null | grep -i status",
                 task_id);

        if (sx_capture(sprite, status_cmd, status_buf, sizeof(status_buf)) == 0) {
            /* Case-insensitive check for completion keywords */
            /* Convert to lowercase for comparison */
            char lower[MAX_LINE];
            size_t i;
            for (i = 0; status_buf[i] && i < sizeof(lower) - 1; i++)
                lower[i] = (char)(status_buf[i] >= 'A' && status_buf[i] <= 'Z'
                            ? status_buf[i] + 32 : status_buf[i]);
            lower[i] = '\0';

            if (strstr(lower, "closed") || strstr(lower, "done") ||
                strstr(lower, "completed")) {
                printf("==========================================\n");
                printf("PROJECT COMPLETE\n");
                printf("==========================================\n");
                break;
            }
        }

        sleep((unsigned)poll_interval);
    }

    return 0;
}
