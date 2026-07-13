#!/usr/bin/env bash

set -euo pipefail

readonly program_name="${0##*/}"
readonly status_expression='(let* ((root (or (getenv "EMACS_PROJECT_ROOT") "")) (digest (secure-hash '\''sha256 (encode-coding-string root '\''utf-8 t)))) (concat digest ":" (or (getenv "EMACS_PROJECT_DAEMON_NAME") "") ":" (or (getenv "EMACS_PROJECT_DAEMON") "")))'

usage() {
  cat <<EOF
Usage:
  $program_name root [--root PATH]
  $program_name name [--root PATH]
  $program_name status [--root PATH] [--quiet]
  $program_name start [--root PATH]
  $program_name stop [--root PATH] [--force]
EOF
}

fail() {
  printf '%s: %s\n' "$program_name" "$*" >&2
  exit 1
}

canonicalize_directory() {
  local path=$1

  [[ -d $path ]] || return 1
  realpath -- "$path"
}

detect_root() {
  local explicit_root=${1-}
  local current git_root parent

  if [[ -n $explicit_root ]]; then
    canonicalize_directory "$explicit_root"
    return
  fi

  current=$(canonicalize_directory "$PWD") || return 1
  git_root=$(git -C "$current" rev-parse --show-toplevel 2>/dev/null) || return 1
  git_root=$(canonicalize_directory "$git_root") || return 1

  case "$current/" in
    "$git_root/"*) ;;
    *) return 1 ;;
  esac

  while :; do
    if [[ -f $current/.dir-locals.el ]]; then
      printf '%s\n' "$current"
      return
    fi

    [[ $current == "$git_root" ]] && break
    parent=$(dirname -- "$current")
    [[ $parent != "$current" ]] || return 1
    current=$parent
  done

  printf '%s\n' "$git_root"
}

root_digest() {
  printf '%s' "$1" | sha256sum | cut -d ' ' -f 1
}

daemon_name() {
  local digest
  digest=$(root_digest "$1")
  printf 'emacs-p-%s\n' "${digest:0:16}"
}

daemon_probe() {
  local root=$1
  local digest name expected actual

  digest=$(root_digest "$root")
  name=$(daemon_name "$root")
  expected="\"$digest:$name:1\""
  if ! actual=$(ALTERNATE_EDITOR=false emacsclient \
      --timeout=1 \
      --socket-name "$name" \
      --eval "$status_expression" 2>/dev/null); then
    return 1
  fi

  [[ $actual == "$expected" ]] && return 0
  return 2
}

with_daemon_lock() {
  local root=$1
  shift

  local name lock_dir lock_file
  name=$(daemon_name "$root")
  lock_dir=${HOME:?HOME is not set}/.local/state/emacs/daemon-locks
  lock_file=$lock_dir/$name.lock

  umask 077
  mkdir -p -- "$lock_dir"
  exec {lock_fd}>"$lock_file"
  flock --exclusive --timeout 10 "$lock_fd" || fail "timed out waiting for daemon lock: $name"
  "$@" "$root"
}

start_locked() {
  local root=$1
  local name output

  if daemon_probe "$root"; then
    printf 'running %s\n' "$(daemon_name "$root")"
    return
  elif [[ $? -eq 2 ]]; then
    fail "daemon identity mismatch: $(daemon_name "$root")"
  fi

  name=$(daemon_name "$root")
  if ! output=$(
      EMACS_PROJECT_DAEMON=1 \
      EMACS_PROJECT_ROOT="$root" \
      EMACS_PROJECT_DAEMON_NAME="$name" \
      emacs --chdir="$root" --daemon="$name" {lock_fd}>&- 2>&1
  ); then
    if daemon_probe "$root"; then
      printf 'running %s\n' "$name"
      return
    fi
    printf '%s\n' "$output" >&2
    fail "failed to start daemon: $name"
  fi

  for _ in $(seq 1 100); do
    if daemon_probe "$root"; then
      printf 'started %s\n' "$name"
      return
    fi
    sleep 0.1
  done

  [[ -z $output ]] || printf '%s\n' "$output" >&2
  fail "daemon did not become ready: $name"
}

stop_locked() {
  local root=$1
  local digest identity name force=$stop_force expression output

  digest=$(root_digest "$root")
  name=$(daemon_name "$root")
  identity="$digest:$name:1"
  if daemon_probe "$root"; then
    :
  else
    case $? in
      1) fail "daemon is not running or not responding: $name" ;;
      2) fail "daemon identity mismatch: $name" ;;
    esac
  fi

  if [[ $force == true ]]; then
    expression="(let ((identity $status_expression)) (unless (equal identity \"$identity\") (error \"Project daemon identity mismatch\")) (run-at-time 0.1 nil #'kill-emacs 0) \"stopping\")"
  else
    expression="(let ((identity $status_expression) (modified (seq-filter (lambda (buffer) (with-current-buffer buffer (and buffer-file-name (buffer-modified-p)))) (buffer-list)))) (unless (equal identity \"$identity\") (error \"Project daemon identity mismatch\")) (if modified (error \"Refusing to stop: modified file buffers: %s\" (mapconcat (lambda (buffer) (buffer-name buffer)) modified \", \")) (run-at-time 0.1 nil #'kill-emacs 0) \"stopping\"))"
  fi

  if ! output=$(ALTERNATE_EDITOR=false emacsclient \
    --timeout=2 \
    --socket-name "$name" \
      --eval "$expression" 2>&1); then
    printf '%s\n' "$output" >&2
    fail "failed to stop daemon: $name"
  fi

  for _ in $(seq 1 50); do
    if daemon_probe "$root"; then
      sleep 0.1
      continue
    else
      case $? in
        1)
          printf 'stopped %s\n' "$name"
          return
          ;;
        2) fail "daemon identity mismatch after stop request: $name" ;;
      esac
    fi
  done

  fail "daemon did not stop: $name"
}

[[ $# -gt 0 ]] || {
  usage >&2
  exit 2
}

command=$1
shift

explicit_root=
quiet=false
stop_force=false

while [[ $# -gt 0 ]]; do
  case $1 in
    --root)
      [[ $# -ge 2 ]] || fail "--root requires a path"
      explicit_root=$2
      shift 2
      ;;
    --quiet)
      quiet=true
      shift
      ;;
    --force)
      stop_force=true
      shift
      ;;
    -h | --help)
      usage
      exit 0
      ;;
    *) fail "unknown option: $1" ;;
  esac
done

case $command in
  root | name | status | start | stop) ;;
  *)
    usage >&2
    exit 2
    ;;
esac

[[ $quiet == false || $command == status ]] || fail "--quiet is only valid with status"
[[ $stop_force == false || $command == stop ]] || fail "--force is only valid with stop"

if ! root=$(detect_root "$explicit_root"); then
  [[ $quiet == true ]] || printf '%s: project root not found\n' "$program_name" >&2
  exit 1
fi

case $command in
  root)
    printf '%s\n' "$root"
    ;;
  name)
    daemon_name "$root"
    ;;
  status)
    if daemon_probe "$root"; then
      [[ $quiet == true ]] || printf 'running %s\n' "$(daemon_name "$root")"
    else
      case $? in
        1)
          [[ $quiet == true ]] || printf 'not running or not responding %s\n' "$(daemon_name "$root")"
          exit 1
          ;;
        2)
          [[ $quiet == true ]] || printf '%s: daemon identity mismatch: %s\n' "$program_name" "$(daemon_name "$root")" >&2
          exit 2
          ;;
      esac
    fi
    ;;
  start)
    if daemon_probe "$root"; then
      printf 'running %s\n' "$(daemon_name "$root")"
    else
      case $? in
        1) with_daemon_lock "$root" start_locked ;;
        2) fail "daemon identity mismatch: $(daemon_name "$root")" ;;
      esac
    fi
    ;;
  stop)
    with_daemon_lock "$root" stop_locked
    ;;
esac
