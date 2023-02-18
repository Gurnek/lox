#!/bin/bash

rm -rf /tmp/pgo-data

RUSTFLAGS="-Cprofile-generate=/tmp/pgo-data -Ctarget-cpu=native" \
    cargo build --profile=optimized

./target/optimized/rlox src/test.lox

llvm-profdata merge -o /tmp/pgo-data/merged.profdata /tmp/pgo-data

RUSTFLAGS="-Cprofile-use=/tmp/pgo-data/merged.profdata -Ctarget-cpu=native" \
    cargo build --profile=optimized