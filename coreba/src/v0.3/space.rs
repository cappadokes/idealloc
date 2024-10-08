use crate::ByteSteps;

/// Holds the allocation's spatial context.
///
/// - [`req_size`](Placement::req_size) stores how many bytes were
/// *initially requested*. Since `idealloc` sprung from research on
/// memory fragmentation, it needs to account for internal fragmentation
/// --which is precisely the difference in size between requested and
/// returned memory.
///
/// - [`alloc_size`](Placement::alloc_size) stores how many bytes were
/// *actually allocated* (see remarks above).
///
/// - [`offset`](Placement::offset) stores the distance, in bytes, of
/// the allocated block of memory from "address zero"--that is, from the
/// beginning of the contiguous memory space in which `idealloc` needs
/// to place its [`Job`]s.
///
/// - [`curr_offset`](Placement::curr_offset) holds the current offset.
/// There is an element of iteration in `idealloc` (for `n` iterations,
/// `n` candidate offsets per job are calculated, and the best is kept
/// at the end).
///
/// - [`alignment`](Placement::alignment) stores potential alignment
/// requirements. If present, it guarantees that [`offset`](Placement::offset)%[`alignment`](Placement::alignment)
///  == 0.
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct Placement {
    pub req_size:       ByteSteps,
    pub alloc_size:     ByteSteps,
    pub offset:         Option<ByteSteps>,
    pub curr_offset:    Option<ByteSteps>,
    pub alignment:      Option<u16>,
}
