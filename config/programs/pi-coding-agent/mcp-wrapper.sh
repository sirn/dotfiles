# MCP: __MCP_NAME__
# Transport: __MCP_TRANSPORT__

COMMAND=$1

JQ_BIN="__JQ_BIN__"
MCP_REMOTE_BIN="__MCP_REMOTE_BIN__"
TIMEOUT_BIN="__TIMEOUT_BIN__"
SERVER_CMD="__SERVER_CMD__"
SERVER_URL="__SERVER_URL__"

case "$COMMAND" in
  list)
    REQUEST='{"jsonrpc":"2.0","id":1,"method":"tools/list"}'
    ;;
  call)
    TOOL=$2
    ARGS="${3:-'{}'}"
    REQUEST=$($JQ_BIN -n -c \
      --arg tool "$TOOL" \
      --argjson args "$ARGS" \
      '{jsonrpc:"2.0",id:1,method:"tools/call",params:{name:$tool,arguments:$args}}')
    ;;
  *)
    echo "Usage: $0 list"
    echo "       $0 call <tool> '<json-args>'"
    exit 1
    ;;
esac

__TRANSPORT_EXEC__
