#!/usr/bin/env bash
# check-commit-msg.sh - Verify commit message formatting and line lengths.
#
# Usage:
#   check-commit-msg.sh [OPTIONS] [REV...]
#
# Options:
#   -m, --max-width N   Maximum line width (default: 72)
#   -v, --vcs VCS       Force VCS: 'jj' or 'git' (auto-detected if omitted)
#   --stdin             Read commit message from standard input
#   -h, --help          Show this help message
#
# REV:
#   One or more revisions or jj revsets (default: '@' for jj, 'HEAD' for git).
#
# Checks:
#   - Non-empty subject on line 1
#   - Blank line 2 when a body is present
#   - Every line within the character width limit
#
# Exit codes:
#   0  All checked commit messages pass.
#   1  One or more checks failed.
#   2  Usage or runtime error.

set -euo pipefail

MAX_WIDTH=72
FORCE_VCS=""
READ_STDIN=0
REVS=()

usage() {
	sed -n '2,/^$/s/^# \{0,1\}//p' "$0"
}

die_usage() {
	echo "Error: $1" >&2
	exit 2
}

while [[ $# -gt 0 ]]; do
	case "$1" in
	-m | --max-width)
		[[ $# -ge 2 ]] || die_usage "Missing value for $1"
		MAX_WIDTH="$2"
		shift 2
		;;
	-v | --vcs)
		[[ $# -ge 2 ]] || die_usage "Missing value for $1"
		FORCE_VCS="$2"
		shift 2
		;;
	--stdin)
		READ_STDIN=1
		shift
		;;
	-h | --help)
		usage
		exit 0
		;;
	--)
		shift
		REVS+=("$@")
		break
		;;
	-*)
		die_usage "Unknown option: $1"
		;;
	*)
		REVS+=("$1")
		shift
		;;
	esac
done

if [[ ! $MAX_WIDTH =~ ^[1-9][0-9]*$ ]]; then
	die_usage "Invalid max width: $MAX_WIDTH (must be a positive integer)"
fi

if [[ -n $FORCE_VCS && $FORCE_VCS != "jj" && $FORCE_VCS != "git" ]]; then
	die_usage "Invalid VCS: $FORCE_VCS (expected 'jj' or 'git')"
fi

detect_vcs() {
	if [[ -n $FORCE_VCS ]]; then
		echo "$FORCE_VCS"
		return
	fi
	if command -v jj &>/dev/null && jj root &>/dev/null; then
		echo "jj"
	elif command -v git &>/dev/null && git rev-parse --is-inside-work-tree &>/dev/null; then
		echo "git"
	else
		die_usage "No supported VCS found (jj or git)"
	fi
}

check_message() {
	local label="$1"
	local msg="$2"
	local fail=0
	local lineno=0
	local line len

	# Strip trailing blank lines so VCS trailing newlines do not look like a body.
	while [[ $msg == *$'\n' ]]; do
		msg="${msg%$'\n'}"
	done

	if [[ -z ${msg//[[:space:]]/} ]]; then
		echo "EMPTY MESSAGE [$label]: commit message is empty"
		return 1
	fi

	while IFS= read -r line; do
		lineno=$((lineno + 1))
		if ((lineno == 1)) && [[ -z ${line//[[:space:]]/} ]]; then
			echo "EMPTY SUBJECT [$label, line 1]: first line must not be blank"
			fail=1
		fi
		if ((lineno == 2)) && [[ -n $line ]]; then
			echo "MISSING BLANK LINE [$label, line 2]: separate subject and body with a blank line"
			fail=1
		fi
		len=${#line}
		if ((len > MAX_WIDTH)); then
			echo "LINE TOO LONG ($len/$MAX_WIDTH) [$label, line $lineno]: $line"
			fail=1
		fi
	done <<<"$msg"

	if ((fail)); then
		echo "FAIL [$label]: $lineno line(s) checked."
		return 1
	fi

	echo "OK [$label]: $lineno line(s) within $MAX_WIDTH characters."
	return 0
}

if ((READ_STDIN)); then
	if ((${#REVS[@]} > 0)); then
		die_usage "Cannot combine --stdin with revision arguments"
	fi
	MSG=$(cat)
	check_message "stdin" "$MSG"
	exit $?
fi

VCS=$(detect_vcs)

if ((${#REVS[@]} == 0)); then
	if [[ $VCS == "git" ]]; then
		REVS=("HEAD")
	else
		REVS=("@")
	fi
fi

OVERALL_FAIL=0

if [[ $VCS == "jj" ]]; then
	for rev in "${REVS[@]}"; do
		commit_ids_out=$(jj log -r "$rev" --no-graph -T 'commit_id ++ "\n"' 2>/dev/null) || die_usage "Failed to resolve revision '$rev' via jj"
		commit_ids=()
		while IFS= read -r cid; do
			[[ -n $cid ]] && commit_ids+=("$cid")
		done <<<"$commit_ids_out"

		if ((${#commit_ids[@]} == 0)); then
			die_usage "Revision '$rev' matched no commits"
		fi

		for cid in "${commit_ids[@]}"; do
			short_id=$(jj log -r "$cid" --no-graph -T 'change_id.short()' 2>/dev/null) || short_id="$cid"
			msg=$(jj log -r "$cid" --no-graph -T 'description' 2>/dev/null) || die_usage "Failed to read commit message for '$cid' via jj"
			if ! check_message "$short_id" "$msg"; then
				OVERALL_FAIL=1
			fi
		done
	done
else
	for rev in "${REVS[@]}"; do
		msg=$(git log -1 --format='%B' "$rev" 2>/dev/null) || die_usage "Failed to get commit message for '$rev' via git"
		if ! check_message "$rev" "$msg"; then
			OVERALL_FAIL=1
		fi
	done
fi

exit "$OVERALL_FAIL"
