OCAML_LIB_DIR := $(shell ocamlc -where)
OCAML_VERSION := $(shell ocamlc -version)
OCAML_MAJOR := $(shell echo $(OCAML_VERSION) | cut -d. -f1)

SKIP_LL := external/skip.ll
SKIP_OBJ := build/skip.o
C_OBJS := build/memory.o build/pack.o build/interop.o build/util.o build/fork.o\
	build/page_table.o

CPP_OBJS := build/map.o

# Runtime source and object files
RUNTIME_SRC_DIR := external/runtime
RUNTIME_C_SRCS := $(filter-out $(RUNTIME_SRC_DIR)/runtime32_specific.c, $(wildcard $(RUNTIME_SRC_DIR)/*.c))
RUNTIME_CPP_SRCS := $(wildcard $(RUNTIME_SRC_DIR)/*.cpp)
RUNTIME_OBJS := $(patsubst $(RUNTIME_SRC_DIR)/%.c, build/runtime_%.o, $(RUNTIME_C_SRCS)) \
                $(patsubst $(RUNTIME_SRC_DIR)/%.cpp, build/runtime_%.o, $(RUNTIME_CPP_SRCS))
RUNTIME_LIB := build/libskip_runtime.a

STATIC_LIB := build/libskip_reactive.a
CMXA := build/reactive.cmxa
TARGET := build/main

# Conditionally add -DOCAML5
CFLAGS := -O2 -g -I$(OCAML_LIB_DIR) -Isrc/c -Iexternal/runtime -DSKIP64 -Wno-c2x-extensions -Wno-extern-c-compat -DRELEASE -DSKIP_NO_MAIN
ifeq ($(shell [ $(OCAML_MAJOR) -ge 5 ] && echo yes),yes)
CFLAGS += -DOCAML5
endif

.PHONY: all clean

all: $(TARGET)

# Ensure build directory exists
$(shell mkdir -p build)

# Final binary: link cmxa, static lib, and explicitly -lstdc++
# Platform-specific linker flags
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
  # macOS: skip position-independent code restrictions (may affect caching)
  LINK_FLAGS := -cclib -lstdc++ -I +unix
else
  # Linux: use fixed text segment for code pointer stability
  LINK_FLAGS := -cclib -lstdc++ -I +unix -ccopt -no-pie -ccopt -Wl,-Ttext=0x8000000
endif

$(TARGET): $(CMXA) $(STATIC_LIB) src/main.ml
	ocamlopt -g -o $@ -I build unix.cmxa reactive.cmxa src/main.ml\
            $(STATIC_LIB) $(LINK_FLAGS)

# OCaml static archive
$(CMXA): build/reactive.cmx
	ocamlopt -a -o $@ $^

# Create unified static lib
$(STATIC_LIB): $(C_OBJS) $(CPP_OBJS) $(RUNTIME_OBJS) $(SKIP_OBJ)
	ar rcs $@ $(C_OBJS) $(CPP_OBJS) $(RUNTIME_OBJS) $(SKIP_OBJ)

build/reactive.cmi: src/ocaml/reactive.mli
	ocamlopt -g -c -o $@ $<

# OCaml object
build/reactive.cmx: src/ocaml/reactive.ml build/reactive.cmi
	ocamlopt -I build/ -I +unix -g -c -o $@ $<

# Compile C sources
build/%.o: src/c/%.c
	clang -g -c $(CFLAGS) $< -o $@

# Compile C++ sources
build/%.o: src/c/%.cpp
	clang++ -g -c $(CFLAGS) $< -o $@

# Special rule for stripping 'main' from runtime64_specific.o
build/runtime_runtime64_specific.o: $(RUNTIME_SRC_DIR)/runtime64_specific.cpp
	clang++ -g -c $(CFLAGS) $< -o $@
	@if command -v objcopy >/dev/null 2>&1; then \
		objcopy --strip-symbol=main $@; \
	elif command -v llvm-objcopy >/dev/null 2>&1; then \
		llvm-objcopy --strip-symbol=main $@; \
	else \
		echo "Warning: objcopy not found, skipping symbol stripping"; \
	fi

# Compile LLVM to object
$(SKIP_OBJ): $(SKIP_LL)
	clang++ -g -c $< -o $@

# Compile runtime .c sources
build/runtime_%.o: $(RUNTIME_SRC_DIR)/%.c
	clang -g -c $(CFLAGS) $< -o $@

# Compile runtime .cpp sources
build/runtime_%.o: $(RUNTIME_SRC_DIR)/%.cpp
	clang++ -g -c $(CFLAGS) $< -o $@



TEST_SRC_DIR:=src/test
TESTS:=$(basename $(notdir $(wildcard $(TEST_SRC_DIR)/*.ml)))
TEST_BINS:=$(addprefix build/,$(TESTS))

.PHONY: tests run_tests

tests: $(TEST_BINS)

build/%: $(TEST_SRC_DIR)/%.ml $(CMXA) $(STATIC_LIB)
	ocamlopt -g -o $@ -I build unix.cmxa reactive.cmxa $<\
		$(STATIC_LIB) $(LINK_FLAGS)\

clean:
	rm -Rf build/
