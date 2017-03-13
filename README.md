This a pretty much work-in-progress.

The project is aimed at defining dependently typed languages via specifying their inference and reduction rules.

The program then generates our target language's parser and typechecker, so we can play around with the language.

Think of it as a high level yacc+lex.


To launch:

stack install alex happy

stack exec alex src/specLang/parsLex/Lexer.x

stack exec happy alex src/specLang/parsLex/Parser.y

stack repl

And "mainCheck" checks your ".fpl" file (there's also a "mainParse" - in Parser module)

E.g.: mainCheck "examples/langSpecs/depTypedLC.fpl"



---
