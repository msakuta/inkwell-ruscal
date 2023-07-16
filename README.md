# inkwell-ruscal

A test repository to support native compile for [Ruscal](https://github.com/msakuta/ruscal) language

It does not reference Ruscal language itself (yet).
It works on a subset of the full language.


## How to run

1. Install Rust.
2. Install LLVM 10. If you are on Ubuntu 20.04, run `sudo apt update && sudo apt install llvm-10-dev`
3. Run `cargo r <scripts/fn_call.rscl` or other example scripts ending `.rscl`

Note that specific LLVM version shouldn't matter.
If you have a different version, just change the inkwell line in [Cargo.toml](Cargo.toml).
