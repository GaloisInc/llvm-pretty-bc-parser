# Regression tests

This test suite compares the behavior of the `llvm-disasm` executable between
different git revisions. See the commented code for details.

For quick reference, here's the output of `--help` for this tool.
```
Usage: regression-test [OPTIONS] test1.ll .. testn.ll

      --with-llvm-as=FILEPATH  path to/name of llvm-as
      --rev1=REV               first git revision to compare
      --rev2=REV               second git revision to compare
      --ast                    compare generated ASTs, rather than disassembled bitcode
  -h  --help
```
The default is to compare `HEAD` against `HEAD~1`, that is, the latest commit
against the next-to-latest.
