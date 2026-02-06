#ifndef PUSH_H
#define PUSH_H

/*
 * sprite-tool push local-path remote-path [sprite-name]
 *
 * argc/argv should be the remaining args after "push" is consumed.
 * Returns 0 on success, non-zero on failure.
 */
int cmd_push(int argc, char **argv);

#endif /* PUSH_H */
