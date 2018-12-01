# Tests from the LLVM project

The tests in this directory are taken from the LLVM project. See [LICENSE](./LICENSE)
(taken from [the LLVM project source](https://raw.githubusercontent.com/llvm-mirror/llvm/master/LICENSE.TXT)).

 - File: `mdnodes-distinct-nodes-first.ll`
   + Path in LLVM source tree: `test/Bitcode/mdnodes-distinct-nodes-first.ll`

   + <!-- Get the short revision number with: git rev-parse --short [hash] -->
     Revision checked out of tree: [e7a2c97](https://github.com/llvm-mirror/llvm/commit/e7a2c97bc25b03f13046949bd767a8207f774cf7)
   + Corresponding LLVM release: 3.9
   + Purpose: `llvm-disasm` doesn't faithfully reproduce the functionality of _any_
     version of `llvm-dis` on this example.