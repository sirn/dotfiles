json-mode json-ts-mode

(pkg "package.json template"
 "{" n>
 "\"name\": \"" (p "project-name") "\"," n>
 "\"version\": \"" (p "0.1.0") "\"," n>
 "\"description\": \"" (p "Description") "\"," n>
 "\"main\": \"" (p "index.js") "\"," n>
 "\"scripts\": {" n>
 "\"test\": \"echo \\\"Error: no test specified\\\" && exit 1\"" n>
 "}," n>
 "\"keywords\": []," n>
 "\"author\": \"\"," n>
 "\"license\": \"MIT\"" n>
 "}" q)
