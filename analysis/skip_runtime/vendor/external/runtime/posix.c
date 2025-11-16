#include <fcntl.h>
#include <poll.h>
#include <signal.h>
#include <spawn.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/errno.h>
#include <sys/wait.h>
#include <unistd.h>

#include "runtime.h"

int64_t SKIP_posix_open(char* path, int64_t oflag, int64_t mode) {
  sk_string_check_c_safe(path);
  int fd = open(path, oflag, mode);

  if (fd == -1) {
    perror("open");
    fprintf(stderr, "Could not open file: %s\n", path);
    exit(EXIT_FAILURE);
  }

  return (int64_t)fd;
}

int64_t SKIP_posix_open_flags(int64_t read, int64_t write, int64_t append,
                              int64_t truncate, int64_t create,
                              int64_t create_new) {
  int res = 0;
  if (read && write) {
    res = O_RDWR;
  } else if (read) {
    res = O_RDONLY;
  } else if (write) {
    res = O_WRONLY;
  } else {
    return -1;
  }

  if (append) {
    res |= O_APPEND;
  }
  if (truncate) {
    res |= O_TRUNC;
  }
  if (create) {
    res |= O_CREAT;
  }
  if (create_new) {
    res |= O_EXCL;
  }

  return (int64_t)res;
}

void SKIP_posix_close(int64_t fd) {
  int rv = close((int)fd);
  if (rv != 0) {
    perror("close");
    exit(EXIT_FAILURE);
  }
}

int64_t SKIP_posix_write(int64_t fd, char* buf, int64_t len) {
  int rv = write((int)fd, buf, (size_t)len);
  if (rv == -1) {
    return (int64_t)(-errno);
  }
  return (int64_t)rv;
}

int64_t SKIP_posix_read(int64_t fd, char* buf, int64_t len) {
  ssize_t rv = read((int)fd, buf, (size_t)len);
  if (rv == -1) {
    return (int64_t)(-errno);
  }
  return (int64_t)rv;
}

int64_t SKIP_posix_lseek(int64_t fd, int64_t offset, int64_t whence) {
  int rv = lseek((int)fd, (int)offset, (int)whence);
  if (rv == -1) {
    perror("lseek");
    exit(EXIT_FAILURE);
  }
  return rv;
}

char* sk_create_posix_pipe(int64_t output_fd, int64_t input_fd);

char* SKIP_posix_pipe() {
  int fds[] = {-1, -1};
  if (pipe(fds) == -1) {
    perror("pipe");
    exit(EXIT_FAILURE);
  }
  return sk_create_posix_pipe((int64_t)fds[0], (int64_t)fds[1]);
}

int64_t SKIP_posix_dup(int64_t fd) {
  int rv = -1;
  while ((rv = dup((int)fd)) == -1) {
    if (errno == EINTR) continue;
    perror("dup");
    exit(EXIT_FAILURE);
  }

  return (int64_t)rv;
}

void SKIP_posix_dup2(int64_t oldfd, int64_t newfd) {
  while (dup2((int)oldfd, (int)newfd) == -1) {
    if (errno == EINTR) continue;
    perror("dup2");
    exit(EXIT_FAILURE);
  }
}

void SKIP_posix_kill(int64_t pid, int64_t sig) {
  int rv = kill((int)pid, (int)sig);
  if (rv != 0) {
    perror("kill");
    exit(EXIT_FAILURE);
  }
}

int64_t SKIP_posix_waitpid(int64_t pid, char nohang) {
  int stat_loc;
  int opts = 0;
  if (nohang) {
    opts |= WNOHANG;
  }
  int rv = waitpid((int)pid, &stat_loc, opts);
  if (rv == -1) {
    perror("waitpid");
    exit(EXIT_FAILURE);
  }
  if (nohang && rv == 0) {
    return (int64_t)(-1);
  }
  return (int64_t)stat_loc;
}

char SKIP_posix_wifexited(int64_t stat_loc) {
  return (char)WIFEXITED(stat_loc);
}

char SKIP_posix_wifsignaled(int64_t stat_loc) {
  return (char)WIFSIGNALED(stat_loc);
}

char SKIP_posix_wifstopped(int64_t stat_loc) {
  return (char)WIFSTOPPED(stat_loc);
}

int64_t SKIP_posix_wexitstatus(int64_t stat_loc) {
  return (int64_t)WEXITSTATUS(stat_loc);
}

int64_t SKIP_posix_wtermsig(int64_t stat_loc) {
  return (int64_t)WTERMSIG(stat_loc);
}

int64_t SKIP_posix_wstopsig(int64_t stat_loc) {
  return (int64_t)WSTOPSIG(stat_loc);
}

int64_t SKIP_posix_poll(char* pollfds) {
  unsigned int nfds = skip_array_len(pollfds);

  for (;;) {
    int rv = poll((struct pollfd*)pollfds, nfds, -1);
    if (rv == -1) {
      if (errno == EINTR) {
        continue;
      } else {
        perror("poll");
        exit(EXIT_FAILURE);
      }
    }
    return (int64_t)rv;
  }
}

void* SKIP_posix_spawn_file_actions_init() {
  posix_spawn_file_actions_t* file_actionsp =
      malloc(sizeof(posix_spawn_file_actions_t));
  if (file_actionsp == NULL) {
    perror("malloc");
    exit(EXIT_FAILURE);
  }
  int rv = posix_spawn_file_actions_init(file_actionsp);
  if (rv != 0) {
    errno = rv;
    perror("posix_spawn_file_actions_init");
    exit(EXIT_FAILURE);
  }

  return file_actionsp;
}

void SKIP_posix_spawn_file_actions_adddup2(void* file_actionsp, int64_t oldfd,
                                           int64_t newfd) {
  int rv = (int)posix_spawn_file_actions_adddup2(
      (posix_spawn_file_actions_t*)file_actionsp, (int)oldfd, (int)newfd);
  if (rv != 0) {
    errno = rv;
    perror("posix_spawn_file_actions_adddup2");
    exit(EXIT_FAILURE);
  }
}

void SKIP_posix_spawn_file_actions_addclose(void* file_actionsp, int64_t fd) {
  int rv = (int)posix_spawn_file_actions_addclose(
      (posix_spawn_file_actions_t*)file_actionsp, (int)fd);
  if (rv != 0) {
    errno = rv;
    perror("posix_spawn_file_actions_addclose");
    exit(EXIT_FAILURE);
  }
}

void SKIP_posix_spawn_file_actions_destroy(void* file_actionsp) {
  int rv = (int)posix_spawn_file_actions_destroy(
      (posix_spawn_file_actions_t*)file_actionsp);
  if (rv != 0) {
    errno = rv;
    perror("posix_spawn_file_actions_destroy");
    exit(EXIT_FAILURE);
  }

  free(file_actionsp);
}

// create a NULL-terminated array of pointers to C strings from Array<String>
char** create_argv(char* skargv) {
  size_t sz = skip_array_len(skargv);
  char** argv = (char**)malloc(sizeof(char*) * (sz + 1));
  if (argv == NULL) {
    perror("malloc");
    exit(EXIT_FAILURE);
  }
  for (size_t i = 0; i < sz; ++i) {
    char* arg = ((char**)skargv)[i];
    sk_string_check_c_safe(arg);
    argv[i] = arg;
  }
  argv[sz] = NULL;
  return argv;
}

int64_t SKIP_posix_spawnp(char* skargv, char* skenvp, char* file_actionsp) {
  char** argv = create_argv(skargv);
  char** envp = create_argv(skenvp);

  pid_t pid = -1;

  int rv =
      posix_spawnp(&pid, argv[0], (posix_spawn_file_actions_t*)file_actionsp,
                   NULL, argv, envp);
  free(argv);
  free(envp);

  if (rv != 0) {
    return (int64_t)(-rv);
  }

  return (int64_t)pid;
}

void SKIP_posix_execvp(char* skargv) {
  char** argv = create_argv(skargv);

  // TODO: Optional envp with execvpe.
  execvp(argv[0], argv);

  free(argv);
  perror("execvp");
  exit(EXIT_FAILURE);
}

int64_t SKIP_posix_isatty(int64_t fd) {
  int rv = isatty((int)fd);
  if (rv == 0 && errno != ENOTTY) {
    return 0; /* treat this call as best effort and assume not a tty */
  }
  return (char)rv;
}

int64_t SKIP_posix_mkstemp(char* template) {
  sk_string_check_c_safe(template);
  size_t memsize = SKIP_String_byteSize(template) + 1;
  char* copy = (char*)malloc(memsize);
  if (copy == NULL) {
    perror("malloc");
    exit(1);
  }
  memcpy(copy, template, memsize);

  int rv = mkstemp(copy);
  if (rv == -1) {
    perror("mkstemp");
    exit(EXIT_FAILURE);
  }

  free(copy);
  return rv;
}
