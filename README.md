# This pretty much a work-in-progress.

The project is aimed at defining dependently typed languages via specifying their inference and reduction rules.

The program then generates our target language's parser and typechecker, so we can play around with the language.

Think of it as a high level yacc+lex.


# To launch:

- stack install alex happy
- stack exec alex src/specLang/parsLex/Lexer.x
- stack exec happy alex src/specLang/parsLex/Parser.y
- stack repl

And "mainCheck" checks your ".fpl" file (there's also a "mainParse" - in Parser module)

E.g.: mainCheck "examples/langSpecs/depTypedLC.fpl"

# Notes:
- May have depsorts and simplesorts or only depsorts
- May have reductions or/and axioms
- axiom names are alphaNum starting with a numeric, may contain "_", "-", "'"

# Restrictions imposed:
- conclusion of an axiom may not have a ctx (axioms always look like this .... |--- |- funSym())

- right now only funsyms are allowed in axiom conclusions
- only metavars are allowed in funsyms in conclusions
- if variables of metavariables have type of metavars, they may use only metavars that come before them in funsyms (Eg: |--- |- f(A, x.B, z.Y, r.T) -- here z may use only A and B as its' type, x may use only A, while r may use A, B, and Y)

- may subst only into metavars
- subst binds closer than binders (x y.T[z:=ttt] == x y.(T[z:=ttt]))


- only parts of reductions used are these a => b (no context, types or premises are taken into account yet)
- in this reduction a => b all metavars in b must be present in a

---
