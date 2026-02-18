# Refactinator

A tool for refactoring LLVM libc math functions into header-only implementations.

## What does it do?

When working on LLVM's libc, math functions live in `.cpp` files under `libc/src/math/generic/`. The goal of this tool is to move those implementations into header-only support files under `libc/src/__support/math/`, generate shared wrappers, and update all the build system files (CMake and Bazel) accordingly.

In short, given a function name like `fabsf`, the tool will:

1. Extract the implementation from `libc/src/math/generic/fabsf.cpp` into `libc/src/__support/math/fabsf.h`
2. Rewrite the original `.cpp` to just call the new header-only version
3. Create a shared wrapper at `libc/shared/math/fabsf.h`
4. Add the include to `libc/shared/math.h`
5. Update `libc/src/__support/math/CMakeLists.txt` with the new header library target
6. Update `libc/src/math/generic/CMakeLists.txt` to depend on the new support target
7. Update the Bazel `BUILD.bazel` file with the support library and math function deps
8. Add a test case to `libc/test/shared/shared_math_test.cpp`

## Building

```bash
cargo build --release
```

The binary ends up at `target/release/refactinator`.

## Usage

### Refactoring a single function

```bash
cargo run -- --func <function_name> --root <path_to_llvm_project>
```

For example:

```bash
cargo run -- --func fabsf --root ~/llvm-project
```

If you omit `--root`, it defaults to the current directory.

### The `--no-constexpr` flag

Some functions can't be `constexpr` (e.g., they touch errno or use runtime-only intrinsics). Pass `-n` or `--no-constexpr` to skip adding the `constexpr` qualifier to the extracted functions:

```bash
cargo run -- --func lgammaf --root ~/llvm-project -n
```

### CLI flags summary

| Flag | Short | Description |
|------|-------|-------------|
| `--func` | `-f` | Name of the math function to refactor (required) |
| `--root` | `-r` | Path to the LLVM project root (default: `.`) |
| `--no-constexpr` | `-n` | Don't add `constexpr` to the extracted implementation |

## Batch refactoring with the shell script

There's a `refactor.sh` script that handles the full workflow: syncing with upstream, creating a branch, running the tool on a list of functions, formatting, and committing.

### Functions file format

Create a text file with one function name per line. Lines starting with `#` are comments, empty lines are ignored.

```
# trig family
sinf
cosf
tanf
```

### Running a batch refactor

```bash
./refactor.sh functions.txt [family_name]
```

- `functions.txt` - path to your functions file
- `family_name` - optional; used for the branch name and commit message. If omitted, it's inferred from the filename (e.g., `trig.txt` → family `trig`)

This will:
1. Sync your local `main` with `upstream/main`
2. Create (or reuse) a branch called `refactor_<family>_family`
3. Run the refactinator on each function in the file
4. Format the code with `clang-format` and `buildifier`
5. Commit everything

### Running just the tests

If you've already done the refactor and just want to re-run the tests:

```bash
./refactor.sh -t
```

This runs the shared math test suite against both GCC and LLVM builds.

### Publishing (creating an issue and PR)

Once you're happy with the refactor:

```bash
./refactor.sh functions.txt -p
```

or if you're already on the right branch and just want to publish:

```bash
./refactor.sh -p family_name
```

This will:
1. Format and commit any remaining changes
2. Create a tracking issue on `llvm/llvm-project`
3. Force-push your branch to your fork
4. Open a pull request against `llvm/llvm-project`

Requires the [GitHub CLI (`gh`)](https://cli.github.com/) to be installed and authenticated.

### Script flags

| Flag | Description |
|------|-------------|
| `-n` | Pass `--no-constexpr` to the refactinator |
| `-t` / `--tests` | Only run tests, skip everything else |
| `-p` / `--publish` | Commit + create GitHub issue + open PR |

## How the transformation works

### Implementation extraction

The tool parses the C++ source using tree-sitter. It looks for `LLVM_LIBC_FUNCTION(ret_type, name, (args))` macros and converts them into `LIBC_INLINE constexpr ret_type name(args)` signatures. Other helper functions in the file get `LIBC_INLINE constexpr` added to them as well.

The extracted code is wrapped in the appropriate include guards, license header, and `namespace math {}` block.

### Float16 / Float128 detection

The tool checks if the original `.cpp` file uses `float16` or `float128` types and automatically:
- Adds the right macro includes (`float16-macros.h`, `float128.h`)
- Wraps the code in `#ifdef LIBC_TYPES_HAS_FLOAT16` / `#ifdef LIBC_TYPES_HAS_FLOAT128` guards
- Adds the corresponding CMake and Bazel dependencies

### Build system updates

**CMake:** The tool parses the existing `add_entrypoint_object` block to extract dependencies, filters out the ones relevant for the support header, and creates a new `add_header_library` target. It also updates the original entrypoint to depend on the new support library.

**Bazel:** Similar logic - it creates a `libc_support_library` target and updates the `libc_math_function` entry to include it in `additional_deps`. It tries to match CMake dependency names to existing Bazel target names by searching the BUILD file for matching header paths.

### Test generation

The tool appends a test invocation to the appropriate `TEST(LlvmLibcSharedMathTest, ...)` block in `shared_math_test.cpp`. It infers the right literal types (e.g., `0.0f` for float, `0.0` for double, `0x0p+0f16` for float16) based on the function name suffix.

## Configuration

The shell script has two paths hardcoded at the top:

```bash
LLVM_PROJECT_DIR="$HOME/Documents/llvm-project"
REFACTINATOR_DIR="$HOME/refactinator"
```

Change these to match your setup before using the script.

## Caveats

- The tool works well for straightforward math functions. If a function has unusual structure (multiple `LLVM_LIBC_FUNCTION` macros, complex preprocessor conditionals, etc.), you might need to manually fix up the output.
- Always review the generated code before committing. The tool does its best, but C++ is C++.
- The Bazel dependency resolution is heuristic-based - it searches for header paths in the BUILD file to find target names. If a target doesn't exist yet, it falls back to a naming convention.
- Run `clang-format` after the tool (the script does this automatically). The tool doesn't try to perfectly format the output.
