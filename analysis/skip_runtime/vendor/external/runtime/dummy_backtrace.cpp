extern "C" {

struct backtrace_state {};

backtrace_state* backtrace_create_state(const char* filename, int threaded,
                                        void* (*error_callback)(void*, const char*, int),
                                        void* data) {
    return nullptr;
}

int backtrace_full(backtrace_state* state, int skip,
                   int (*callback)(void* data, void* pc, const char* filename, int lineno, const char* function),
                   void* error_callback, void* data) {
    return 0;
}

}
