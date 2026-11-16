#!/bin/sh
# SessionStart hook: emit every AGENTS.d/*.md so the harness loads
# project-specific agent rules deterministically at session start. Wired into
# both Claude Code (.claude/settings.json) and Codex (.codex/hooks.json); both
# inject a hook's plain stdout as context. Missing/empty AGENTS.d -> no output.
#
# Canonical source: package/tools/load-agents-d.sh in mlr3infrastructure; copied
# into each package as tools/load-agents-d.sh by the /r-package-setup skill.
set -eu

# Project root is the parent of the tools/ dir this script sits in, resolved
# from $0 so the hook works regardless of the caller's cwd.
root="$(CDPATH= cd "$(dirname "$0")/.." && pwd)"
dir="$root/AGENTS.d"
[ -d "$dir" ] || exit 0

emitted=0
for f in "$dir"/*.md; do
  [ -e "$f" ] || continue  # no matches: glob stays literal, skip it
  if [ "$emitted" -eq 0 ]; then
    printf 'Project-specific agent rules from AGENTS.d/ (loaded via SessionStart hook):\n'
    emitted=1
  fi
  printf '\n\n===== AGENTS.d/%s =====\n\n' "$(basename "$f")"
  cat "$f"
done
