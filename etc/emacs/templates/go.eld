go-mode go-ts-mode

(main "Go main package"
 "package main" n n
 "import (" n>
 "\"fmt\"" q n
 ")" n n
 "func main() {" n>
 "fmt.Println(\"Hello, world!\")" n
 "}")

(test "Go test function"
 "func Test" (p "Name") "(t *testing.T) {" n>
 q n
 "}")

(err "Go error handling"
 "if err != nil {" n>
 "return " (p "err") n
 "}")
