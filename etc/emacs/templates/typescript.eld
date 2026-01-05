typescript-mode typescript-ts-mode tsx-ts-mode

(rfc "React Functional Component"
 "import React from 'react';" n n
 "interface " (p "ComponentName" name) "Props {" n>
 (p "prop") ": " (p "type") ";" n
 "}" n n
 "export const " (s name) ": React.FC<" (s name) "Props> = ({ " (p "prop") " }) => {" n>
 "return (" n>
 "<div>" n>
 q n>
 "</div>" n>
 ");" n
 "};" n n
 "export default " (s name) ";")
