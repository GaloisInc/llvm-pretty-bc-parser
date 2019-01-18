# Fuzz tests

See the comment at the top of [fuzz.sh](./fuzz.sh).

To run the tests in Docker, run [fuzz-docker.sh](./fuzz-docker.sh).

## Jenkins

Galois, Inc. hosts a Jenkins fuzzing build of this project.

To add LLVM/Clang version `${version}` (e.g. `version=8.0.0`) to be tested:

 - SSH into the VM
 - Find the URL of the appropriate pre-built binaries here: http://releases.llvm.org/download.html
 - Download the package to `$HOME`
 - `tar xvf clang+llvm-${version}*`
 - `mv clang+llvm-${version}* /opt/llvm-${version}`
 - Go to the build configuration page in Jenkins
 - Add the new version of Clang/LLVM to the `PATH` variable there
 - Add a `-c` flag mentioning the new Clang version
