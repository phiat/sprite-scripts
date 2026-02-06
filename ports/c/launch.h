#ifndef LAUNCH_H
#define LAUNCH_H

/*
 * sprite-tool launch [--dry-run] [--no-checkpoint] [--upload dir] sprite-name [plan-file]
 *
 * argc/argv should be the remaining args after "launch" is consumed.
 * Returns 0 on success, non-zero on failure.
 */
int cmd_launch(int argc, char **argv);

#endif /* LAUNCH_H */
