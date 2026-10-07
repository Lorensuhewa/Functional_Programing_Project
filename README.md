EC8206 Functional Programming - Project

Folder structure
----------------
src/Expr.hs                  Core expression type and evaluator
app/Main.hs                  Sample demonstrations and output
 eval/eval_imperative.py     Imperative-style Python comparison for Part D
report/EC8206_Project_Report.docx

How to run Haskell
------------------
1. Open a terminal in the project root.
2. Start GHCi:
      ghci -isrc app\\Main.hs
3. Run:
      main

To build an executable:
      ghc -isrc app\\Main.hs -o interpreter.exe

Then run on Windows:
      .\\interpreter.exe


