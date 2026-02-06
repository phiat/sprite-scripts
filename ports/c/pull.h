#ifndef PULL_H
#define PULL_H

/*
 * sprite-tool pull remote-path local-path [sprite-name]
 *
 * argc/argv should be the remaining args after "pull" is consumed.
 * Returns 0 on success, non-zero on failure.
 */
int cmd_pull(int argc, char **argv);

#endif /* PULL_H */
