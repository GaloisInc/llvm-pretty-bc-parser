# Known Bugs for llvm-pretty-bc-parser

This directory is used to keep files for known llvm-pretty-bc-parser bugs.  The
goal is that this directory is empty (other than this readme), but the reality is
that sometimes there are issues that will take some time to address.

If an entry is made in this directory, corresponding tests will be *expected* to
fail, and thus the entire CI test suite will pass even in the presence of these
bugs.  This is more desirable than either (a) adding tests to demonstrate the bug
but then having to "ignore" test failures manually, and (b) not adding tests to
the test suite for known bugs because it is desireable or required that the CI
passes.  With this mechanism, a test should be added to demonstrate the problem
(and an issue should be filed!) but the CI can regain the previous state until
the bugs can be addressed (because this library does not define original
functionality, but it recapitulates functionality from the LLVM project).  Also
note that any efforts that add *new* capabilities are not expected to create
entries in this directory: they should be fully and successfully implemented if
at all possible before merging into mainline code.

# Format

When adding a new entry to this directory, a single file is created.  By using a
single file, known bugs can be added and removed easily without the necessity of
code changes elsewhere (and keeping the list of known bugs in source code invites
the potential for merge conflicts when adding/resolving bugs in parallel efforts,
whereas these separate files do not).

All files in this directory are processed (including this one!), regardless of
filename or extension.  Only files which contain marker lines (described below)
will be added a s a known bug; this file describes marker lines but is careful to
not contain marker lines (i.e. a line which begins with `"##>"`), so that it will
not be treated as a known bug.

Lines which begin with `"##> rootMatchName: "` should be followed by one or
more words which correspond to the corresponding tasty-sugar field in the
provided `Sweets` structure when generating tests.

Lines which begin with `"##> llvmver: "` should be followed by one or more words,
each of which corresponds to an llvm version (in the format `"llvmN"`) that the
bug is known to be present for.

The above lines are the primary mechanism for matching a particular test with the
expectation that it will fail.  There can be multiple values specified after each
identifier word, and there can be multiple lines starting with each word: all
corresponding tasty-sugar sweets will be identified as failures.  There must be
at least one identifier line present, but any identifier lines not present will
be treated as a wildcard.

One and only one line should start with `"##> summary: "`; the remainder of
that line is displayed along with the (expected) failure message when running the
test.

All other lines in the file are ignored by disasm-test.  It is recommended that
the file contain additional information, including how to reproduce the issue,
how to recognize the issue, links to actual issues (e.g. posted to GitHub) and
any thoughts regarding the potential cause of the issue.  This information can be
helpful to whomever attempts to resolve this known bug (where success will be
indicated by the ability to remove this corresponding known_bugs file).
