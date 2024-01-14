# Leviator BNF

```bnf
<syntax> ::= <expression>*

<expression> ::= <alias> | <function> | <comment>

<alias> ::= "alias " <identifierAlias> " " <replacement> ";\n"
<identifierAlias> ::= "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" |
                      "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" |
                      "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "a" |
                      "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" |
                      "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" |
                      "t" | "u" | "v" | "w" | "x" | "y" | "z" | "0" | "1" |
                      "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "_" |
                      "." | "-" | ":" | "!" | "@" | "#" | "$" | "%" | "^" |
                      "&" | "*" | "(" | ")" | "[" | "]" | "{" | "}" | "|" |
                      "\\" | "+" | "=" | ";" | "<" | ">" | "?" | "/" | "`" |
                      "~"
<replacement> ::= <char2>

<comment> ::= "//" <char1>* "\n"

<function> ::= "fn " <identifier> "(" <parameterList>* ") -> " <type> "\n{\n" <instruction>* "}\n"
<identifier> ::= <lowerLetter> <char2>
<parameterList> ::= <parameter> ","
<parameter> ::= <identifier> ": " <type>
<type> ::= <upperLetter> <char2>
<instruction> ::= <instructionIns> ";\n"
<instructionIns> ::= <declaration> | <assignment> | <functionCall> | <return> | <condition>
<declaration> ::= "@" <type> " " <identifier> " = " <value>
<assignment> ::= <identifier> " = " <value>
<functionCall> ::= <identifier> "(" <varParamList>* ")"
<varParamList> ::= <value> ","
<return> ::= "<- " <value>
<value> ::= <functionCall> | <identifier> | <literal>
<literal> ::= <digit> | <character> | <bool> | <stringview>
<condition> ::= <conditionIf> | <conditionIfElse>
<conditionIfElse> ::= <conditionIf> <conditionElse>
<conditionIf> ::= "if (" <value> ")\n{\n" <instruction>* "}\n"
<conditionElse> ::= "else\n{\n" <instruction>* "}\n"

<character> ::= "'" <char1> "'"
<bool> ::= "True" | "False"
<stringview> ::= "\"" <char1>* "\""

<char> ::= <lowerLetter> | <upperLetter> | <digit>
<char1> ::= <char> | "" | " " | <specialAll>
<char2> ::= <lowerLetter> | <upperLetter> | <digit> | <special>
<lowerLetter> ::= "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" |
                  "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" |
                  "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
<upperLetter> ::= "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" |
                  "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" |
                  "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z"
<digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
<special> ::= "_"
<specialAll> ::= <special> | "!" | "@" | "#" | "$" | "%" | "^" | "&" |
                 "*" | "(" | ")" | "[" | "]" | "{" | "}" | "|" | "\\" |
                 "+" | "=" | ";" | "<" | ">" | "?" | "/" | "`" | "~"
```
