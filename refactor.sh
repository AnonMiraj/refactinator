#!/bin/bash
set -e
set -o pipefail

# --- Configuration ---
LLVM_PROJECT_DIR="$HOME/Documents/llvm-project"
REFACTINATOR_DIR="$HOME/refactinator"

# --- Argument Parsing ---
REFACTINATOR_ARGS=""
ONLY_TESTS=false
PUBLISH=false
POSITIONAL_ARGS=()

# Parse all arguments
while [[ $# -gt 0 ]]; do
  case "$1" in
  -n)
    REFACTINATOR_ARGS="-n"
    echo "removed constexpr"
    shift
    ;;
  -t | --tests)
    ONLY_TESTS=true
    echo "🧪 Test-only mode enabled"
    shift
    ;;
  -p | --publish)
    PUBLISH=true
    echo "🌐 GitHub Publish mode enabled (Commit + Issue + PR)"
    shift
    ;;
  -*)
    echo "❌ Unknown option: $1"
    exit 1
    ;;
  *)
    POSITIONAL_ARGS+=("$1")
    shift
    ;;
  esac
done

# --- Variables & Inference ---

FUNCTIONS_FILE_RAW="${POSITIONAL_ARGS[0]}"

if [ -n "$FUNCTIONS_FILE_RAW" ] && [ -f "$FUNCTIONS_FILE_RAW" ]; then
  FUNCTIONS_FILE=$(realpath "$FUNCTIONS_FILE_RAW")
  FAMILY="${POSITIONAL_ARGS[1]}"
  if [ -z "$FAMILY" ]; then
    FAMILY=$(basename "$FUNCTIONS_FILE_RAW" | cut -d. -f1)
    echo "ℹ️ Inferred family '$FAMILY' from filename"
  fi
else
  FUNCTIONS_FILE=""
  FAMILY="${POSITIONAL_ARGS[0]}"
fi

if [ -z "$FAMILY" ]; then
  cd "$LLVM_PROJECT_DIR"
  CURRENT_BRANCH=$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "")
  if [[ $CURRENT_BRANCH =~ refactor_(.*)_family ]]; then
    FAMILY="${BASH_REMATCH[1]}"
    echo "ℹ️ Inferred family '$FAMILY' from branch '$CURRENT_BRANCH'"
  fi
fi

BRANCH_NAME="refactor_${FAMILY}_family"

# --- Functions ---

get_function_list_formatted() {
  if [ -f "$FUNCTIONS_FILE" ]; then
    grep -v '^#' "$FUNCTIONS_FILE" | grep -v '^$' | sed 's/^/  - /'
  else
    echo "  - (Batch refactor for $FAMILY)"
  fi
}

sync_upstream() {
  echo "🔄 Syncing with upstream..."
  cd "$LLVM_PROJECT_DIR"
  git checkout main
  git fetch upstream
  git rebase upstream/main
}

setup_branch() {
  echo "🌿 Setting up branch: $BRANCH_NAME..."
  cd "$LLVM_PROJECT_DIR"
  if git show-ref --verify --quiet "refs/heads/$BRANCH_NAME"; then
    echo "    ♻️ Branch exists. Reusing: $BRANCH_NAME"
    git checkout "$BRANCH_NAME"
    echo "    🧨 Discarding local changes..."
    git restore --staged libc utils || true
    git restore libc utils || true
    git clean -fd libc utils || true
  else
    echo "    ✨ Creating new branch"
    git checkout -b "$BRANCH_NAME"
  fi
}

run_refactinator() {
  if [ -z "$FUNCTIONS_FILE" ]; then
    echo "❌ Error: Functions file is required for refactoring."
    exit 1
  fi
  echo "🛠️ Running refactinator..."
  cd "$REFACTINATOR_DIR"
  while IFS= read -r FUNC || [ -n "$FUNC" ]; do
    [[ -z "$FUNC" ]] && continue
    [[ "$FUNC" =~ ^#.* ]] && continue
    echo "    → Refactoring: $FUNC"
    cargo run --quiet -- --func "$FUNC" --root "$LLVM_PROJECT_DIR" $REFACTINATOR_ARGS
  done <"$FUNCTIONS_FILE"
  cd "$LLVM_PROJECT_DIR"
}

run_tests() {
  echo "🧪 Running Tests..."
  cd "$LLVM_PROJECT_DIR"
  echo "    • Running GCC tests..."
  ninja -C build-libc-gcc/runtimes/runtimes-bins/ libc.test.shared.shared_math_test.__unit__ || echo "⚠️ GCC tests failed"
  echo "    • Running LLVM tests..."
  ninja -C build-libc/runtimes/runtimes-bins/ libc.test.shared.shared_math_test.__unit__ || echo "⚠️ LLVM tests failed"
}

format_code() {
  echo "🧹 Formatting..."
  cd "$LLVM_PROJECT_DIR"
  git add libc/ utils/ 2>/dev/null || true
  git clang-format --force
  git add libc/ utils/ 2>/dev/null || true

  if command -v buildifier &>/dev/null; then
    if [ -f "./utils/bazel/llvm-project-overlay/libc/BUILD.bazel" ]; then
      echo "    🏗️ Running buildifier..."
      buildifier ./utils/bazel/llvm-project-overlay/libc/BUILD.bazel
      git add ./utils/bazel/llvm-project-overlay/libc/BUILD.bazel
    fi
  fi
}

commit_changes() {
  cd "$LLVM_PROJECT_DIR"
  if git diff --cached --quiet; then
    echo "ℹ️ No changes to commit (already clean)."
    return 0
  fi

  echo "📦 Committing changes..."
  local FUNC_LIST=$(get_function_list_formatted)
  local COMMIT_MSG="[libc][math] Refactor ${FAMILY} family to header-only

Refactored functions:
$FUNC_LIST"

  git commit -m "$COMMIT_MSG"
}

publish_to_github() {
  echo "🐙 Publishing to GitHub..."
  cd "$LLVM_PROJECT_DIR"

  if ! command -v gh &>/dev/null; then
    echo "❌ Error: GitHub CLI (gh) not found."
    return 1
  fi

  if [ -z "$FAMILY" ]; then
    echo "❌ Error: Family name is required to publish."
    exit 1
  fi

  local TARGET_REPO="llvm/llvm-project"
  local GITHUB_USER=$(gh api user -q .login)
  local FUNC_LIST=$(get_function_list_formatted)

  local ISSUE_TITLE="[libc][math] Tracking Issue - Refactor ${FAMILY} Math Functions to Header Only"
  local ISSUE_BODY="This is tracking issue to refactor ${FAMILY} math functions currently implemented in LLVM libc to be header-only.

part of: #147386
@bassiounix

Target Functions:
$FUNC_LIST"

  echo "    📝 Creating tracking issue on $TARGET_REPO..."
  local ISSUE_URL=$(gh issue create --repo "$TARGET_REPO" --title "$ISSUE_TITLE" --body "$ISSUE_BODY")
  echo "    ✅ Issue created: $ISSUE_URL"

  echo "    📤 Pushing branch $BRANCH_NAME to origin..."
  git push -u origin "$BRANCH_NAME" --force

  local PR_TITLE="[libc][math] Refactor ${FAMILY} family to header-only"
  local PR_BODY="Refactors the ${FAMILY} math family to be header-only.

Closes $ISSUE_URL

Target Functions:
$FUNC_LIST"

  echo "    🎁 Creating Pull Request on $TARGET_REPO..."
  gh pr create \
    --repo "$TARGET_REPO" \
    --title "$PR_TITLE" \
    --body "$PR_BODY" \
    --base main \
    --head "${GITHUB_USER}:${BRANCH_NAME}"

  echo "    ✅ PR created!"
}

# --- Main Execution ---

if [ "$ONLY_TESTS" = true ]; then
  run_tests
  exit 0
fi

if [ "$PUBLISH" = true ]; then
  if [ -z "$FAMILY" ]; then
    echo "❌ Error: Could not determine family name for publishing."
    exit 1
  fi

  echo "🚀 Starting exclusive Publish flow for family: $FAMILY"
  cd "$LLVM_PROJECT_DIR"

  format_code
  commit_changes
  publish_to_github
  exit 0
fi

if [ -z "$FUNCTIONS_FILE" ]; then
  echo "❌ Error: Functions file required for refactoring."
  echo "Usage (Refactor): $0 <functions_file> [-n]"
  echo "Usage (Publish only): $0 <functions_file> -p"
  exit 1
fi

echo "🚀 Starting refactor flow for family: $FAMILY"
sync_upstream
setup_branch
run_refactinator
format_code
commit_changes
echo "✅ Refactoring complete! Use -p to publish when ready."
