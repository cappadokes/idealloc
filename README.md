#   `idealloc`: Tools for Next-Gen Memory Allocation

> ***DISCLAIMER:*** This repo is the main outcome of my PhD. It is by no means professional work. Parts of it were written when I was still a (metaphorical) child. The most stable and tested thing to use is the `idealloc` binary, described in the `coreba` crate (you can check a not-yet-peer-reviewed version of our technical report [here](https://arxiv.org/abs/2504.04874)). Lots of the other stuff is undocumented or unaligned with present documentation. Don't take anything you read here too seriously!

This work reconciles practical and theoretical, i.e., formal, methods for dynamic memory allocation.

##  Installation

1. [Install Rust.](https://www.rust-lang.org/tools/install)
2. `cd idealloc`
3. `cargo build --release`

Compiled binaries reside in `idealloc/target/release`.

##  Tracing Linux applications

`uncore` contains a set of interposing functions for the `malloc` API. Its goal is to emit a compact, binary-encoded file containing all of the application's allocation-related calls, as well as `malloc`'s resulting actions, in sequence.

More specifically, the generated `.trc` file has the following structure:

- `sentinel`: a byte from the set `{0x05, 0x12, 0x26, 0x36, 0x46, 0x56, 0x67}`. Each item's first half (4 bits) encodes the request recorded: `0` for `malloc`, `1` for `free`, `2` for `calloc` etc. The second half encodes how many *machine words* (64 bits during development and testing) related to the forementioned request follow. For example, `malloc` needs 5 additional fields: *requested size*, *returned pointer*, *allocated size*, *thread ID*, *memory mapping*. `free` needs 2: *pointer* and *thread ID*.
- the series of fields, as numbered within the `sentinel` byte.

### Limitations

1. Works only on binaries that do not `fork`/`execve`. Dealing with inherited/cleared memory would unnecessarily complicate both `uncore` and the rest of `idealloc`'s components. Of course that constrains us away from interesting workloads like `gcc`, language runtimes like `CPython`, etc. Nothing comes for free in this world.

2. If one plans to use the produced trace with the `copycat` (see below), the one must make sure that the targeted application is *deterministic w.r.t. its allocation-related calls*: different runs should produce identical logical thread-request couples.

3. Call sites are not of interest to us.

##  Evaluating placements

Placements stem from 2 possible sources: traced Linux applications and ML compiler-generated CSVs. To deal with both uniformly, we transform both in `.plc` format, a custom binary encoding which expresses the bin packing notation on top of which we are building. This is one of the `sanity` crate's two functions, exposed via the `adapt` binary.

The second functionality performs measurements w.r.t. fragmentation, load, makespan etc. It's implemented via the `report` binary of `sanity`, and accepts `.plc` or `.csv` files as its input.

##  Static Memory Planning

`idealloc` was born in the context of research on dynamic memory allocation. Nitty-gritty details aside, an allocator solves an *online* version of an old NP-complete combinatorial optimization problem called Dynamic Storage Allocation (DSA). `idealloc` solves DSA **offline**.

Details on the why and the how may be found on [our paper's pre-print](https://arxiv.org/abs/2504.04874).