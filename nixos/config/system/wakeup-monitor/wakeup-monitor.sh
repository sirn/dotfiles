set -euo pipefail

state_dir=/var/lib/wakeup-monitor
events_log="$state_dir/events.log"

mkdir -p "$state_dir"

log() {
  local message="$*"
  printf '%s %s\n' "$(date --iso-8601=seconds)" "$message" | tee -a "$events_log" | systemd-cat -t wakeup-monitor -p info
}

safe_cat() {
  local path="$1"
  if [[ -r $path ]]; then
    cat "$path" 2>/dev/null || true
  fi
}

dump_acpi_interrupts() {
  local output="$1"
  : >"$output"
  for path in /sys/firmware/acpi/interrupts/*; do
    [[ -r $path ]] || continue
    local name
    name="$(basename "$path")"
    printf '%s %s\n' "$name" "$(safe_cat "$path")" >>"$output"
  done
}

dump_wakeup_sources() {
  local output="$1"
  if [[ -r /sys/kernel/debug/wakeup_sources ]]; then
    cat /sys/kernel/debug/wakeup_sources >"$output" 2>/dev/null || : >"$output"
  else
    : >"$output"
  fi
}

dump_power_wakeup() {
  local output="$1"
  : >"$output"
  find /sys/devices -path '*/power/wakeup' -type f -readable -print 2>/dev/null |
    while read -r path; do
      local value
      value="$(safe_cat "$path")"
      [[ -n $value ]] || continue
      printf '%s %s\n' "${path%/power/wakeup}" "$value"
    done |
    sort >"$output"
}

dump_misc() {
  local output="$1"
  {
    printf 'timestamp=%s\n' "$(date --iso-8601=ns)"
    printf 'wakeup_count=%s\n' "$(safe_cat /sys/power/wakeup_count)"
    printf 'mem_sleep=%s\n' "$(safe_cat /sys/power/mem_sleep)"
    printf 'pm_debug_messages=%s\n' "$(safe_cat /sys/power/pm_debug_messages)"
    printf 'pm_print_times=%s\n' "$(safe_cat /sys/power/pm_print_times)"

    for path in /proc/acpi/button/lid/*/state; do
      [[ -r $path ]] || continue
      printf 'lid:%s=%s\n' "$path" "$(safe_cat "$path")"
    done

    for path in /sys/class/rtc/rtc*/wakealarm; do
      [[ -e $path ]] || continue
      printf 'rtc:%s=%s\n' "$path" "$(safe_cat "$path")"
    done

    for path in /sys/power/suspend_stats/*; do
      [[ -r $path ]] || continue
      printf 'suspend_stats:%s=%s\n' "$(basename "$path")" "$(safe_cat "$path")"
    done

    if [[ -r /proc/acpi/wakeup ]]; then
      sed 's/^/acpi_wakeup:/' /proc/acpi/wakeup
    fi
  } >"$output"
}

snapshot() {
  local prefix="$1"
  dump_acpi_interrupts "$state_dir/acpi.$prefix"
  dump_wakeup_sources "$state_dir/wakeup_sources.$prefix"
  dump_power_wakeup "$state_dir/power_wakeup.$prefix"
  dump_misc "$state_dir/misc.$prefix"
}

log_acpi_deltas() {
  local before="$state_dir/acpi.pre"
  local after="$state_dir/acpi.post"
  [[ -s $before && -s $after ]] || return 0

  local deltas
  deltas="$(
    awk '
      NR == FNR {
        before[$1] = $2 + 0
        next
      }
      $1 in before {
        delta = ($2 + 0) - before[$1]
        if (delta > 0) {
          print delta, $0
        }
      }
    ' "$before" "$after" | sort -rn | head -n 10
  )"

  if [[ -n $deltas ]]; then
    log "ACPI interrupt deltas since suspend: ${deltas//$'\n'/; }"
  else
    log "ACPI interrupt deltas since suspend: none"
  fi
}

log_wakeup_source_deltas() {
  local before="$state_dir/wakeup_sources.pre"
  local after="$state_dir/wakeup_sources.post"
  [[ -s $before && -s $after ]] || {
    log "kernel wakeup_sources unavailable; debugfs may not be mounted"
    return 0
  }

  local deltas
  deltas="$(
    awk '
      NR == FNR && FNR > 1 {
        event_before[$1] = $3 + 0
        wake_before[$1] = $4 + 0
        expire_before[$1] = $5 + 0
        next
      }
      FNR > 1 && ($1 in event_before) {
        event_delta = ($3 + 0) - event_before[$1]
        wake_delta = ($4 + 0) - wake_before[$1]
        expire_delta = ($5 + 0) - expire_before[$1]
        if (event_delta > 0 || wake_delta > 0 || expire_delta > 0) {
          printf "%d event_delta=%d wakeup_delta=%d expire_delta=%d source=%s\n", event_delta + wake_delta + expire_delta, event_delta, wake_delta, expire_delta, $1
        }
      }
    ' "$before" "$after" | sort -rn | head -n 10
  )"

  if [[ -n $deltas ]]; then
    log "kernel wakeup_source deltas since suspend: ${deltas//$'\n'/; }"
  else
    log "kernel wakeup_source deltas since suspend: none"
  fi
}

enable_kernel_pm_debug() {
  [[ -w /sys/power/pm_debug_messages ]] && echo 1 >/sys/power/pm_debug_messages || true
  [[ -w /sys/power/pm_print_times ]] && echo 1 >/sys/power/pm_print_times || true
}

case "${1:-}" in
pre)
  enable_kernel_pm_debug
  snapshot pre
  log "captured pre-suspend wakeup snapshot"
  ;;
post)
  snapshot post
  log "captured post-resume wakeup snapshot"
  log_wakeup_source_deltas
  log_acpi_deltas
  ;;
debug)
  enable_kernel_pm_debug
  log "enabled kernel PM debug messages"
  ;;
*)
  echo "usage: wakeup-monitor {pre|post|debug}" >&2
  exit 64
  ;;
esac
