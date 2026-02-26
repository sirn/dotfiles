#!/usr/bin/env bash
# synthetic-quota - Check Synthetic API quota
# Requires: curl, jq
# Environment: SYNTHETIC_API_KEY must be set in ~/.config/llm-agent/env or exported

set -euo pipefail

ENV_FILE=${ENV_FILE:-"${HOME}/.config/sops-nix/secrets/agents/env"}
if [[ -f "$ENV_FILE" ]]; then
    # shellcheck source=/dev/null
    source "$ENV_FILE"
fi

if [[ -z "${SYNTHETIC_API_KEY:-}" ]]; then
    echo "SYNTHETIC_API_KEY is not set" >&2
    exit 1
fi

curl -s \
    -H "Authorization: Bearer $SYNTHETIC_API_KEY" \
    https://api.synthetic.new/v2/quotas | jq
