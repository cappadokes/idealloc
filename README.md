#   `idealloc`: Futureproof Static Memory Planning

> ***DISCLAIMER:*** This repo is the main outcome of my PhD. It is by no means professional work. Parts of it were written when I was still a (metaphorical) child. The most stable and tested thing to use is the `idealloc` binary, described in the `coreba` crate (you can check a not-yet-peer-reviewed version of our technical report [here](https://arxiv.org/abs/2504.04874)). Lots of the other stuff is undocumented or unaligned with present documentation. The rest of the README is about the `idealloc` binary of the `coreba` crate.

`idealloc` was born in the context of research on dynamic memory allocation (hence the other crates). Nitty-gritty details aside, an allocator solves an *online* version of an old NP-complete combinatorial optimization problem called Dynamic Storage Allocation (DSA). `idealloc` solves DSA **offline**.

Details on the why and the how may be found on [our paper's pre-print](https://arxiv.org/abs/2504.04874).

##  Installation

1. [Install Rust.](https://www.rust-lang.org/tools/install)
2. `cd coreba`
3. `cargo build --release`

Compiled binaries reside in `idealloc/target/release`.

##  Usage

```bash
./target/release/idealloc --input $PATH_TO_CSV $INPUT_FORMAT [-f $MAX_FRAG --start $START_ADDRESS -l $MAX_ITERS]
```

- The input CSV must be structured like the ones in the `/benchmarks` folder.
- Valid values for the input's format are `in-csv` and `ex-csv`. They dictate block lifetime semantics.
- Default value for maximum fragmentation is 1.0, i.e., a perfect solution. 1.1 would accept up to 10% fragmentation, 1.2 up to 20% and so on.
- Default value for maximum iterations is 1.

`idealloc` can be used as a library element via the `coreba::algo::idealloc` function.