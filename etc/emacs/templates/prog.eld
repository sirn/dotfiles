prog-mode

(todo "TODO comment" "TODO: " p " [" (format-time-string "%Y-%m-%d") "]" q)
(fixme "FIXME comment" "FIXME: " p " [" (format-time-string "%Y-%m-%d") "]" q)
(header "File header"
 "/**" n
 " * File: " (file-name-nondirectory (or (buffer-file-name) "untitled")) n
 " *" n
 " * " p n
 " *" n
 " * Created: " (format-time-string "%Y-%m-%d") n
 " */" n q)
