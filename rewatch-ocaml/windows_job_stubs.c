#define CAML_INTERNALS

#ifdef _WIN32
#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0600
#endif
#endif

#include <stdio.h>

/* This owner extends the MIT-licensed Spawn 0.17 Windows process setup with
   suspended Job Object assignment, which must happen before user code runs. */

#ifdef _WIN32
#include <caml/osdeps.h>
#endif

#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

#ifdef _WIN32

#include <windows.h>

CAMLprim value rewatch_windows_directory_identity(value path_value)
{
  CAMLparam1(path_value);
  CAMLlocal1(identity_value);
  WCHAR *path = caml_stat_strdup_to_utf16(String_val(path_value));
  BY_HANDLE_FILE_INFORMATION information;
  char identity[26];
  HANDLE handle = CreateFileW(path, 0,
                              FILE_SHARE_READ | FILE_SHARE_WRITE |
                                FILE_SHARE_DELETE,
                              NULL, OPEN_EXISTING,
                              FILE_FLAG_BACKUP_SEMANTICS, NULL);
  caml_stat_free(path);
  if (handle == INVALID_HANDLE_VALUE) {
    caml_win32_maperr(GetLastError());
    uerror("CreateFile", path_value);
  }
  if (!GetFileInformationByHandle(handle, &information)) {
    DWORD error = GetLastError();
    CloseHandle(handle);
    caml_win32_maperr(error);
    uerror("GetFileInformationByHandle", path_value);
  }
  CloseHandle(handle);
  snprintf(identity, sizeof(identity), "%08lx:%08lx%08lx",
           (unsigned long)information.dwVolumeSerialNumber,
           (unsigned long)information.nFileIndexHigh,
           (unsigned long)information.nFileIndexLow);
  identity_value = caml_copy_string(identity);
  CAMLreturn(identity_value);
}

struct rewatch_windows_job {
  HANDLE handle;
  HANDLE stdin_handle;
  HANDLE stdout_handle;
  HANDLE stderr_handle;
  WCHAR *program;
  WCHAR *command_line;
  WCHAR *environment;
  WCHAR *cwd;
};

#define Job_val(v) ((struct rewatch_windows_job *)Data_custom_val(v))

static void rewatch_windows_release_launch_resources(
  struct rewatch_windows_job *job)
{
  if (job->stdin_handle != NULL) CloseHandle(job->stdin_handle);
  if (job->stdout_handle != NULL) CloseHandle(job->stdout_handle);
  if (job->stderr_handle != NULL) CloseHandle(job->stderr_handle);
  if (job->program != NULL) caml_stat_free(job->program);
  if (job->command_line != NULL) caml_stat_free(job->command_line);
  if (job->environment != NULL) caml_stat_free(job->environment);
  if (job->cwd != NULL) caml_stat_free(job->cwd);
  job->stdin_handle = NULL;
  job->stdout_handle = NULL;
  job->stderr_handle = NULL;
  job->program = NULL;
  job->command_line = NULL;
  job->environment = NULL;
  job->cwd = NULL;
}

static void rewatch_windows_finalize_job(value job_value)
{
  struct rewatch_windows_job *job = Job_val(job_value);
  rewatch_windows_release_launch_resources(job);
  if (job->handle != NULL) {
    CloseHandle(job->handle);
    job->handle = NULL;
  }
}

static const struct custom_operations rewatch_windows_job_operations = {
  "rescript.rewatch.windows_job",
  rewatch_windows_finalize_job,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static BOOL duplicate_standard_handle(value descriptor, HANDLE *result)
{
  return DuplicateHandle(GetCurrentProcess(), Handle_val(descriptor),
                         GetCurrentProcess(), result, 0L, TRUE,
                         DUPLICATE_SAME_ACCESS);
}

static BOOL terminate_unowned_process(PROCESS_INFORMATION *process,
                                      DWORD *termination_error)
{
  BOOL terminated = TerminateProcess(process->hProcess, 1);
  if (!terminated) *termination_error = GetLastError();
  CloseHandle(process->hThread);
  CloseHandle(process->hProcess);
  return terminated;
}

CAMLprim value rewatch_windows_spawn_owned(value env_value,
                                            value cwd_value,
                                            value program_value,
                                            value command_line_value,
                                            value stdin_value,
                                            value stdout_value,
                                            value stderr_value)
{
  CAMLparam5(env_value, cwd_value, program_value, command_line_value,
             stdin_value);
  CAMLxparam2(stdout_value, stderr_value);
  CAMLlocal2(job_value, process_value);
  STARTUPINFOEXW startup;
  PROCESS_INFORMATION process;
  struct rewatch_windows_job *job;
  HANDLE job_handle;
  HANDLE inherited_handles[3];
  SIZE_T attribute_list_size = 0;
  DWORD error;

  job_value = caml_alloc_custom(&rewatch_windows_job_operations,
                                sizeof(struct rewatch_windows_job), 0, 1);
  job = Job_val(job_value);
  job->handle = NULL;
  job->stdin_handle = NULL;
  job->stdout_handle = NULL;
  job->stderr_handle = NULL;
  job->program = NULL;
  job->command_line = NULL;
  job->environment = NULL;
  job->cwd = NULL;
  process_value = caml_alloc_small(2, 0);
  Store_field(process_value, 0, Val_long(0));
  Store_field(process_value, 1, job_value);
  job = Job_val(job_value);

  ZeroMemory(&startup, sizeof(startup));
  ZeroMemory(&process, sizeof(process));
  startup.StartupInfo.cb = sizeof(startup);
  startup.StartupInfo.dwFlags = STARTF_USESTDHANDLES;

  if (!duplicate_standard_handle(stdin_value, &job->stdin_handle) ||
      !duplicate_standard_handle(stdout_value, &job->stdout_handle) ||
      !duplicate_standard_handle(stderr_value, &job->stderr_handle)) {
    error = GetLastError();
    caml_win32_maperr(error);
    uerror("DuplicateHandle", Nothing);
  }
  startup.StartupInfo.hStdInput = job->stdin_handle;
  startup.StartupInfo.hStdOutput = job->stdout_handle;
  startup.StartupInfo.hStdError = job->stderr_handle;
  inherited_handles[0] = job->stdin_handle;
  inherited_handles[1] = job->stdout_handle;
  inherited_handles[2] = job->stderr_handle;

  InitializeProcThreadAttributeList(NULL, 1, 0, &attribute_list_size);
  startup.lpAttributeList =
    HeapAlloc(GetProcessHeap(), 0, attribute_list_size);
  if (startup.lpAttributeList == NULL) {
    rewatch_windows_release_launch_resources(job);
    caml_win32_maperr(ERROR_NOT_ENOUGH_MEMORY);
    uerror("HeapAlloc", Nothing);
  }
  if (!InitializeProcThreadAttributeList(startup.lpAttributeList, 1, 0,
                                         &attribute_list_size)) {
    error = GetLastError();
    HeapFree(GetProcessHeap(), 0, startup.lpAttributeList);
    startup.lpAttributeList = NULL;
    rewatch_windows_release_launch_resources(job);
    caml_win32_maperr(error);
    uerror("InitializeProcThreadAttributeList", Nothing);
  }
  if (!UpdateProcThreadAttribute(startup.lpAttributeList, 0,
                                 PROC_THREAD_ATTRIBUTE_HANDLE_LIST,
                                 inherited_handles,
                                 sizeof(inherited_handles), NULL, NULL)) {
    error = GetLastError();
    DeleteProcThreadAttributeList(startup.lpAttributeList);
    HeapFree(GetProcessHeap(), 0, startup.lpAttributeList);
    startup.lpAttributeList = NULL;
    rewatch_windows_release_launch_resources(job);
    caml_win32_maperr(error);
    uerror("UpdateProcThreadAttribute", Nothing);
  }

  job->program = caml_stat_strdup_to_utf16(String_val(program_value));
  job->command_line =
    caml_stat_strdup_to_utf16(String_val(command_line_value));
  job->cwd = caml_stat_strdup_to_utf16(String_val(cwd_value));
  if (Is_block(env_value)) {
    value contents = Field(env_value, 0);
    mlsize_t length = caml_string_length(contents);
    int size = caml_win32_multi_byte_to_wide_char(
      String_val(contents), length, NULL, 0);
    job->environment = caml_stat_alloc(size * sizeof(WCHAR));
    caml_win32_multi_byte_to_wide_char(String_val(contents), length,
                                       job->environment, size);
  }

  job_handle = CreateJobObjectW(NULL, NULL);
  if (job_handle == NULL) {
    error = GetLastError();
    DeleteProcThreadAttributeList(startup.lpAttributeList);
    HeapFree(GetProcessHeap(), 0, startup.lpAttributeList);
    rewatch_windows_release_launch_resources(job);
    caml_win32_maperr(error);
    uerror("CreateJobObject", Nothing);
  }
  job->handle = job_handle;

  if (!CreateProcessW(job->program, job->command_line, NULL, NULL, TRUE,
                      CREATE_SUSPENDED | CREATE_UNICODE_ENVIRONMENT |
                        EXTENDED_STARTUPINFO_PRESENT,
                      job->environment, job->cwd, &startup.StartupInfo,
                      &process)) {
    error = GetLastError();
    DeleteProcThreadAttributeList(startup.lpAttributeList);
    HeapFree(GetProcessHeap(), 0, startup.lpAttributeList);
    rewatch_windows_release_launch_resources(job);
    CloseHandle(job->handle);
    job->handle = NULL;
    caml_win32_maperr(error);
    uerror("CreateProcess", Nothing);
  }

  DeleteProcThreadAttributeList(startup.lpAttributeList);
  HeapFree(GetProcessHeap(), 0, startup.lpAttributeList);
  rewatch_windows_release_launch_resources(job);

  if (!AssignProcessToJobObject(job->handle, process.hProcess)) {
    error = GetLastError();
    DWORD termination_error;
    BOOL terminated = terminate_unowned_process(&process, &termination_error);
    CloseHandle(job->handle);
    job->handle = NULL;
    if (!terminated) {
      caml_win32_maperr(termination_error);
      uerror("TerminateProcess", Nothing);
    }
    caml_win32_maperr(error);
    uerror("AssignProcessToJobObject", Nothing);
  }

  if (ResumeThread(process.hThread) == (DWORD)-1) {
    error = GetLastError();
    if (!TerminateJobObject(job->handle, 1)) {
      DWORD termination_error;
      if (!TerminateProcess(process.hProcess, 1)) {
        termination_error = GetLastError();
        CloseHandle(process.hThread);
        CloseHandle(process.hProcess);
        CloseHandle(job->handle);
        job->handle = NULL;
        caml_win32_maperr(termination_error);
        uerror("TerminateProcess", Nothing);
      }
    }
    CloseHandle(process.hThread);
    CloseHandle(process.hProcess);
    CloseHandle(job->handle);
    job->handle = NULL;
    caml_win32_maperr(error);
    uerror("ResumeThread", Nothing);
  }

  CloseHandle(process.hThread);
  Store_field(process_value, 0, Val_long(process.hProcess));
  CAMLreturn(process_value);
}

CAMLprim value rewatch_windows_spawn_owned_byte(value *arguments,
                                                 int argument_count)
{
  (void)argument_count;
  return rewatch_windows_spawn_owned(arguments[0], arguments[1], arguments[2],
                                     arguments[3], arguments[4], arguments[5],
                                     arguments[6]);
}

CAMLprim value rewatch_windows_terminate_process_job(value job_value)
{
  CAMLparam1(job_value);
  struct rewatch_windows_job *job = Job_val(job_value);
  JOBOBJECT_EXTENDED_LIMIT_INFORMATION limits;
  HANDLE handle;

  if (job->handle == NULL) CAMLreturn(Val_true);
  if (TerminateJobObject(job->handle, 1)) CAMLreturn(Val_true);

  /* Closing a job normally must not affect a successfully detached helper.
     Only the cancellation fallback enables kill-on-close before releasing the
     last stable process-tree identity. */
  ZeroMemory(&limits, sizeof(limits));
  limits.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
  handle = job->handle;
  if (SetInformationJobObject(handle, JobObjectExtendedLimitInformation,
                              &limits, sizeof(limits))) {
    if (CloseHandle(handle)) {
      job->handle = NULL;
      CAMLreturn(Val_true);
    }
  }
  CAMLreturn(Val_false);
}

CAMLprim value rewatch_windows_close_process_job(value job_value)
{
  CAMLparam1(job_value);
  struct rewatch_windows_job *job = Job_val(job_value);
  HANDLE handle = job->handle;

  if (handle != NULL) {
    job->handle = NULL;
    if (!CloseHandle(handle)) {
      caml_win32_maperr(GetLastError());
      uerror("CloseHandle", Nothing);
    }
  }
  CAMLreturn(Val_unit);
}

#else

CAMLprim value rewatch_windows_directory_identity(value path_value)
{
  (void)path_value;
  caml_invalid_argument("Windows directory identities are unavailable");
}

CAMLprim value rewatch_windows_spawn_owned(value env_value,
                                            value cwd_value,
                                            value program_value,
                                            value command_line_value,
                                            value stdin_value,
                                            value stdout_value,
                                            value stderr_value)
{
  (void)env_value;
  (void)cwd_value;
  (void)program_value;
  (void)command_line_value;
  (void)stdin_value;
  (void)stdout_value;
  (void)stderr_value;
  caml_invalid_argument("Windows process jobs are unavailable");
}

CAMLprim value rewatch_windows_spawn_owned_byte(value *arguments,
                                                 int argument_count)
{
  (void)argument_count;
  return rewatch_windows_spawn_owned(arguments[0], arguments[1], arguments[2],
                                     arguments[3], arguments[4], arguments[5],
                                     arguments[6]);
}

CAMLprim value rewatch_windows_terminate_process_job(value job_value)
{
  (void)job_value;
  caml_invalid_argument("Windows process jobs are unavailable");
}

CAMLprim value rewatch_windows_close_process_job(value job_value)
{
  (void)job_value;
  caml_invalid_argument("Windows process jobs are unavailable");
}

#endif
