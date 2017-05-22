# This is pretty much a work-in-progress.

The project is aimed at defining dependently typed languages via specifying their inference and reduction rules.

The program then generates our target language's parser and typechecker, so we can play around with the language.

Think of it as a high level yacc+lex.

# To launch:

- stack install alex happy
- stack exec alex src/specLang/parsLex/Lexer.x
- stack exec happy alex src/specLang/parsLex/Parser.y
- stack install

~/.local/bin/fpl-exploration-tool-exe "examples/langSpecs/depTypedLC.fpl" > my_src.hs

There are 2 modules: SortCheck and CodeGen, and two functions codeGenIO and sortCheckIO you can use those if you prefer.

# Notes about spec language:
- May have depsorts and simplesorts or only depsorts
- May have reductions or/and axioms
- axiom names are alphaNum starting with a numeric, may contain "_", "-", "'"
- subst binds closer than binders (x y.T[z:=ttt] == x y.(T[z:=ttt]))

# Restrictions imposed:
- conclusion of an axiom/reduction may not have a ctx (axioms always look like this .... |--- |- funSym())

- only funsyms are allowed in axiom conclusions (no equations)
- only metavars are allowed in funsyms in conclusions
- if an axiom conclusion is a term it must have a type (can't just say |--- |- false def, must say |--- |- false : bool)
- no substitutions are allowed for the left hand of a judgement
- may subst only into metavars

- if variables of metavariables (X) have type of metavars, they may use only metavars that come before X in funsym in conclusion (Eg: |--- |- f(A, x.B, z.Y, r.T) -- here z may use only A and B as its' type, x may use only A, while r may use A, B, and Y)
- so if we have f(..., xy.T) we demand a premise looking like this ctx |- T !

- only parts of reductions used are these a => b (context, types or premises are not taken into account yet)
- in reductions a => b all(!) metavars of b must be present in a

- c-stability - reductions are always stable. Others are concatenated with the types on top

---
