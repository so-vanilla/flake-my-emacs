#!/usr/bin/env bash

set -euo pipefail

readonly program_name="${0##*/}"

fail() {
  printf '%s: %s\n' "$program_name" "$*" >&2
  exit 1
}

reject_reserved_long_option() {
  local argument=$1
  local option=${argument%%=*}
  local reserved

  [[ $option == --?* ]] || return 0

  for reserved in --socket-name --server-file --alternate-editor; do
    if [[ $reserved == "$option"* || $option == "$reserved" ]]; then
      fail "$option is reserved; select the server with --global or --project"
    fi
  done

  return 0
}

mode=auto
passthrough=true
args=()

while [[ $# -gt 0 ]]; do
  if [[ $passthrough == false ]]; then
    args+=("$1")
    shift
    continue
  fi

  case $1 in
    --)
      passthrough=false
      args+=("$1")
      shift
      ;;
    --global)
      [[ $mode == auto ]] || fail "--global and --project cannot be combined or repeated"
      mode=global
      shift
      ;;
    --project)
      [[ $mode == auto ]] || fail "--global and --project cannot be combined or repeated"
      mode=project
      shift
      ;;
    -s | -s?* | -f | -f?* | -a | -a?*)
      fail "$1 is reserved; select the server with --global or --project"
      ;;
    *)
      reject_reserved_long_option "$1"
      args+=("$1")
      shift
      ;;
  esac
done

run_global() {
  unset EMACS_SOCKET_NAME EMACS_SERVER_FILE
  exec emacsclient --alternate-editor= "${args[@]}"
}

if [[ $mode == global ]]; then
  run_global
fi

if ! root=$(emacs-project-daemon root 2>/dev/null); then
  [[ $mode == project ]] && fail "project root not found"
  run_global
fi

name=$(emacs-project-daemon name --root "$root")
if emacs-project-daemon status --root "$root" --quiet; then
  ALTERNATE_EDITOR=false exec emacsclient --socket-name "$name" "${args[@]}"
else
  status_result=$?
fi

[[ $status_result -ne 2 ]] || fail "project daemon identity mismatch: $name"
[[ $mode == project ]] && fail "project daemon is not running: $name"
run_global
