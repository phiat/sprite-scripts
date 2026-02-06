#include "sprite.h"
#include "config.h"

#include <errno.h>
#include <libgen.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>

/* ------------------------------------------------------------------ */
/* sx: run command inside sprite via system()                          */
/* ------------------------------------------------------------------ */
int sx(const char *sprite, const char *cmd, bool dry_run)
{
    char buf[MAX_CMD];

    if (dry_run) {
        printf("  [dry-run] sprite exec -s %s bash -c \"%s\"\n", sprite, cmd);
        return 0;
    }

    snprintf(buf, sizeof(buf),
             "sprite exec -s '%s' bash -c '%s'", sprite, cmd);
    return system(buf);
}

/* ------------------------------------------------------------------ */
/* sx_capture: run command in sprite, capture stdout into buf          */
/* ------------------------------------------------------------------ */
int sx_capture(const char *sprite, const char *cmd, char *buf, size_t bufsz)
{
    char cmdline[MAX_CMD];
    snprintf(cmdline, sizeof(cmdline),
             "sprite exec -s '%s' bash -c '%s' 2>/dev/null", sprite, cmd);

    FILE *fp = popen(cmdline, "r");
    if (!fp)
        return -1;

    size_t total = 0;
    while (total < bufsz - 1) {
        size_t n = fread(buf + total, 1, bufsz - 1 - total, fp);
        if (n == 0)
            break;
        total += n;
    }
    buf[total] = '\0';

    /* Strip trailing whitespace */
    while (total > 0 && (buf[total - 1] == '\n' || buf[total - 1] == '\r' ||
                         buf[total - 1] == ' '  || buf[total - 1] == '\t')) {
        buf[--total] = '\0';
    }

    int status = pclose(fp);
    return WIFEXITED(status) && WEXITSTATUS(status) == 0 ? 0 : -1;
}

/* ------------------------------------------------------------------ */
/* Helper: safe dirname/basename that don't modify the original        */
/* ------------------------------------------------------------------ */
static void path_dirname(const char *path, char *out, size_t outsz)
{
    char tmp[MAX_PATH_LEN];
    snprintf(tmp, sizeof(tmp), "%s", path);
    snprintf(out, outsz, "%s", dirname(tmp));
}

static void path_basename(const char *path, char *out, size_t outsz)
{
    char tmp[MAX_PATH_LEN];
    snprintf(tmp, sizeof(tmp), "%s", path);
    snprintf(out, outsz, "%s", basename(tmp));
}

/* ------------------------------------------------------------------ */
/* push_file: copy local file into sprite                              */
/* ------------------------------------------------------------------ */
int push_file(const char *sprite, const char *local_path,
              const char *remote_path, bool dry_run)
{
    if (dry_run) {
        printf("  [dry-run] push %s -> sprite:%s\n", local_path, remote_path);
        return 0;
    }

    /* Ensure remote parent directory exists */
    char rdir[MAX_PATH_LEN];
    path_dirname(remote_path, rdir, sizeof(rdir));

    char mkdir_cmd[MAX_CMD];
    snprintf(mkdir_cmd, sizeof(mkdir_cmd), "mkdir -p '%s'", rdir);
    sx(sprite, mkdir_cmd, false);

    /* Pipe file contents via popen */
    char cmdline[MAX_CMD];
    snprintf(cmdline, sizeof(cmdline),
             "sprite exec -s '%s' bash -c \"cat > '%s'\"",
             sprite, remote_path);

    FILE *proc = popen(cmdline, "w");
    if (!proc) {
        fprintf(stderr, "Error: failed to popen for push_file\n");
        return -1;
    }

    FILE *fp = fopen(local_path, "rb");
    if (!fp) {
        fprintf(stderr, "Error: cannot open %s: %s\n",
                local_path, strerror(errno));
        pclose(proc);
        return -1;
    }

    char buf[8192];
    size_t n;
    while ((n = fread(buf, 1, sizeof(buf), fp)) > 0) {
        if (fwrite(buf, 1, n, proc) != n) {
            fprintf(stderr, "Error: write to sprite failed\n");
            fclose(fp);
            pclose(proc);
            return -1;
        }
    }

    fclose(fp);
    int status = pclose(proc);
    return WIFEXITED(status) && WEXITSTATUS(status) == 0 ? 0 : -1;
}

/* ------------------------------------------------------------------ */
/* push_dir: tar | sprite exec tar                                     */
/* ------------------------------------------------------------------ */
int push_dir(const char *sprite, const char *local_dir,
             const char *remote_path, bool dry_run)
{
    if (dry_run) {
        printf("  [dry-run] push dir %s -> sprite:%s\n",
               local_dir, remote_path);
        return 0;
    }

    /* Ensure remote directory exists */
    char mkdir_cmd[MAX_CMD];
    snprintf(mkdir_cmd, sizeof(mkdir_cmd), "mkdir -p '%s'", remote_path);
    sx(sprite, mkdir_cmd, false);

    char parent[MAX_PATH_LEN];
    char base[MAX_PATH_LEN];
    path_dirname(local_dir, parent, sizeof(parent));
    path_basename(local_dir, base, sizeof(base));

    char rparent[MAX_PATH_LEN];
    path_dirname(remote_path, rparent, sizeof(rparent));

    int pipefd[2];
    if (pipe(pipefd) < 0) {
        perror("pipe");
        return -1;
    }

    pid_t pid = fork();
    if (pid < 0) {
        perror("fork");
        close(pipefd[0]);
        close(pipefd[1]);
        return -1;
    }

    if (pid == 0) {
        /* Child: tar czf - -C parent base, stdout -> pipe write end */
        close(pipefd[0]);
        if (dup2(pipefd[1], STDOUT_FILENO) < 0) {
            perror("dup2");
            _exit(1);
        }
        close(pipefd[1]);
        execlp("tar", "tar", "czf", "-", "-C", parent, base, (char *)NULL);
        perror("execlp tar");
        _exit(1);
    }

    /* Parent: read from pipe, feed to sprite exec tar */
    close(pipefd[1]);

    char cmdline[MAX_CMD];
    snprintf(cmdline, sizeof(cmdline),
             "sprite exec -s '%s' bash -c \"tar xzf - -C '%s'\"",
             sprite, rparent);

    /* We need to fork again for the sprite exec side, reading from pipefd[0] */
    pid_t pid2 = fork();
    if (pid2 < 0) {
        perror("fork");
        close(pipefd[0]);
        return -1;
    }

    if (pid2 == 0) {
        /* Child 2: sprite exec with stdin from pipe */
        if (dup2(pipefd[0], STDIN_FILENO) < 0) {
            perror("dup2");
            _exit(1);
        }
        close(pipefd[0]);
        execl("/bin/sh", "sh", "-c", cmdline, (char *)NULL);
        perror("execl");
        _exit(1);
    }

    close(pipefd[0]);

    int status1, status2;
    waitpid(pid, &status1, 0);
    waitpid(pid2, &status2, 0);

    if (WIFEXITED(status1) && WEXITSTATUS(status1) != 0)
        return -1;
    if (WIFEXITED(status2) && WEXITSTATUS(status2) != 0)
        return -1;
    return 0;
}

/* ------------------------------------------------------------------ */
/* pull_file: sprite exec cat > local file                             */
/* ------------------------------------------------------------------ */
int pull_file(const char *sprite, const char *remote_path,
              const char *local_path)
{
    /* Ensure local parent directory exists */
    char ldir[MAX_PATH_LEN];
    path_dirname(local_path, ldir, sizeof(ldir));

    char mkdir_cmd[MAX_CMD];
    snprintf(mkdir_cmd, sizeof(mkdir_cmd), "mkdir -p '%s'", ldir);
    system(mkdir_cmd);

    char cmdline[MAX_CMD];
    snprintf(cmdline, sizeof(cmdline),
             "sprite exec -s '%s' cat '%s'", sprite, remote_path);

    FILE *proc = popen(cmdline, "r");
    if (!proc) {
        fprintf(stderr, "Error: failed to popen for pull_file\n");
        return -1;
    }

    FILE *fp = fopen(local_path, "wb");
    if (!fp) {
        fprintf(stderr, "Error: cannot open %s for writing: %s\n",
                local_path, strerror(errno));
        pclose(proc);
        return -1;
    }

    char buf[8192];
    size_t n;
    while ((n = fread(buf, 1, sizeof(buf), proc)) > 0) {
        if (fwrite(buf, 1, n, fp) != n) {
            fprintf(stderr, "Error: write to local file failed\n");
            fclose(fp);
            pclose(proc);
            return -1;
        }
    }

    fclose(fp);
    int status = pclose(proc);
    return WIFEXITED(status) && WEXITSTATUS(status) == 0 ? 0 : -1;
}

/* ------------------------------------------------------------------ */
/* pull_dir: sprite exec tar | local tar                               */
/* ------------------------------------------------------------------ */
int pull_dir(const char *sprite, const char *remote_path,
             const char *local_path)
{
    /* Ensure local directory exists */
    char mkdir_cmd[MAX_CMD];
    snprintf(mkdir_cmd, sizeof(mkdir_cmd), "mkdir -p '%s'", local_path);
    system(mkdir_cmd);

    int pipefd[2];
    if (pipe(pipefd) < 0) {
        perror("pipe");
        return -1;
    }

    pid_t pid = fork();
    if (pid < 0) {
        perror("fork");
        close(pipefd[0]);
        close(pipefd[1]);
        return -1;
    }

    if (pid == 0) {
        /* Child: sprite exec tar czf - , stdout -> pipe write end */
        close(pipefd[0]);
        if (dup2(pipefd[1], STDOUT_FILENO) < 0) {
            perror("dup2");
            _exit(1);
        }
        close(pipefd[1]);

        char cmdline[MAX_CMD];
        snprintf(cmdline, sizeof(cmdline),
                 "sprite exec -s '%s' tar czf - -C '%s' .",
                 sprite, remote_path);
        execl("/bin/sh", "sh", "-c", cmdline, (char *)NULL);
        perror("execl");
        _exit(1);
    }

    /* Parent: tar xzf - -C local_path, stdin from pipe read end */
    close(pipefd[1]);

    pid_t pid2 = fork();
    if (pid2 < 0) {
        perror("fork");
        close(pipefd[0]);
        return -1;
    }

    if (pid2 == 0) {
        if (dup2(pipefd[0], STDIN_FILENO) < 0) {
            perror("dup2");
            _exit(1);
        }
        close(pipefd[0]);
        execlp("tar", "tar", "xzf", "-", "-C", local_path, (char *)NULL);
        perror("execlp tar");
        _exit(1);
    }

    close(pipefd[0]);

    int status1, status2;
    waitpid(pid, &status1, 0);
    waitpid(pid2, &status2, 0);

    if (WIFEXITED(status1) && WEXITSTATUS(status1) != 0)
        return -1;
    if (WIFEXITED(status2) && WEXITSTATUS(status2) != 0)
        return -1;
    return 0;
}

/* ------------------------------------------------------------------ */
/* remote_is_dir: test if remote path is a directory                   */
/* ------------------------------------------------------------------ */
int remote_is_dir(const char *sprite, const char *remote_path)
{
    char result[64];
    char cmd[MAX_CMD];
    snprintf(cmd, sizeof(cmd),
             "[ -d '\\''%s'\\'' ] && echo dir || echo file", remote_path);

    /* Build the actual command ourselves to handle quoting properly */
    char cmdline[MAX_CMD];
    snprintf(cmdline, sizeof(cmdline),
             "sprite exec -s '%s' bash -c '[ -d \"%s\" ] && echo dir || echo file' 2>/dev/null",
             sprite, remote_path);

    FILE *fp = popen(cmdline, "r");
    if (!fp)
        return -1;

    result[0] = '\0';
    if (fgets(result, sizeof(result), fp) == NULL) {
        pclose(fp);
        return -1;
    }
    pclose(fp);

    /* Strip trailing whitespace */
    size_t len = strlen(result);
    while (len > 0 && (result[len - 1] == '\n' || result[len - 1] == '\r'))
        result[--len] = '\0';

    if (strcmp(result, "dir") == 0)
        return 1;
    if (strcmp(result, "file") == 0)
        return 0;
    return -1;
}
