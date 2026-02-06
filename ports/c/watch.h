#ifndef WATCH_H
#define WATCH_H

/*
 * sprite-tool watch sprite-name [task-id] [poll-interval]
 *
 * argc/argv should be the remaining args after "watch" is consumed.
 * Returns 0 on success, non-zero on failure.
 */
int cmd_watch(int argc, char **argv);

#endif /* WATCH_H */
