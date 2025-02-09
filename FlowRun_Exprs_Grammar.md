
GRAMMAR:
```
expression          -> boolOrComparison ("||"  boolOrComparison)* ;
boolOrComparison    -> boolAndComparison ("&&"  boolAndComparison)* ;
boolAndComparison   -> numComparison (("!=" | "==" ) numComparison)* ;
numComparison       -> term (("<" | ">" | "<=" | ">=") term)* ;
term                -> factor (("+" | "-") factor)* ;
factor              -> unary (("*" |  "/" | "%") unary)* ;
unary               -> ("!" | "-") unary
                    | atom ;
atom                -> NUMBER
                    | STRING
                    | "true"
                    | "false"
                    | ID[intExpression]
                    | ID[intExpression][intExpression]
                    | ID
                    | ID "(" expression? ( "," expression)* ")"
                    | "(" expression ")" ;

ID                  -> [a-zA-Z] [a-zA-Z0-9]*
STRING              -> """ [a-zA-Z]* """
NUMBER              -> [0-9]+ [[0-9]+]?
```

## Precedence
Examples:
- `*` has higher precedence than `+`.  
- `&&` has higher precedence than `||`.  


