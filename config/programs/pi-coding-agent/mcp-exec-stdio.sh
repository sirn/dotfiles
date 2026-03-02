echo "$REQUEST" | __SERVER_CMD__ | __JQ_BIN__ -r '.result | if (.tools) then (.tools[] | .name) else (.content[] | .text) end'
