# Use a FIFO to maintain persistent stdin for SSE transport
# mcp-remote requires an open connection to receive async responses
FIFO=$(mktemp -u)
mkfifo "$FIFO"
# Open FIFO read-write (doesn't block on Linux) to keep it open via FD 3
exec 3<> "$FIFO"
echo "$REQUEST" > "$FIFO"
__TIMEOUT_BIN__ "${TIMEOUT:-30}" __MCP_REMOTE_BIN__ "__SERVER_URL__" < "$FIFO" 2>/dev/null | __JQ_BIN__ -r '.result | if (.tools) then (.tools[] | .name) else (.content[] | .text) end'
exec 3<&-
rm -f "$FIFO"
