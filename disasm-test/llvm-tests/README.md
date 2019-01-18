# Tests from the LLVM project

The tests in this directory are taken from the LLVM project. See [LICENSE](./LICENSE)
(taken from [the LLVM project source](https://raw.githubusercontent.com/llvm-mirror/llvm/master/LICENSE.TXT)).

Some of these files don't assemble unless used with the corresponding version
of `llvm-as`.

 - File: `mdnodes-distinct-nodes-first.ll`
   + Path in LLVM source tree: `test/Bitcode/mdnodes-distinct-nodes-first.ll`
   + <!-- Get the short revision number with: git rev-parse --short [hash] -->
     Revision checked out of tree: [e7a2c97](https://github.com/llvm-mirror/llvm/commit/e7a2c97bc25b03f13046949bd767a8207f774cf7)
   + Corresponding LLVM release: 3.9
   + Purpose: `llvm-disasm` doesn't faithfully reproduce the functionality of _any_
     version of `llvm-dis` on this example.

 - File: `cfi-eof-prologue.new.ll`
   + Path in LLVM source tree: `test/DebugInfo/AArch64/cfi-eof-prologue.ll`
   + <!-- Get the short revision number with: git rev-parse --short [hash] -->
     Revision checked out of tree: [de74840](https://github.com/llvm-mirror/llvm/blob/de7484036b628b08be6acbfb5feac405d7450300)
   + Corresponding LLVM release: 3.9
   + Purpose: References to `DICompositeType`s changed in 3.9. See
     [llvm-pretty#39](https://github.com/elliottt/llvm-pretty/issues/39).

 - File: `cfi-eof-prologue.old.ll`
   + Path in LLVM source tree: `test/DebugInfo/AArch64/cfi-eof-prologue.ll`
   + Revision checked out of tree: [release_38](https://github.com/llvm-mirror/llvm/blob/release_38/)
   + Corresponding LLVM release: 3.8
   + Purpose: See above. We should also maintain compatibility with 3.8-style
     references.
