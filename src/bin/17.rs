#![feature(
    absolute_path,
    addr_parse_ascii,
    alloc_error_hook,
    alloc_internals,
    alloc_layout_extra,
    allocator_api,
    arc_unwrap_or_clone,
    array_chunks,
    array_into_iter_constructors,
    array_methods,
    array_try_from_fn,
    array_try_map,
    array_windows,
    as_array_of_cells,
    assert_matches,
    async_iter_from_iter,
    async_iterator,
    atomic_bool_fetch_not,
    atomic_from_mut,
    backtrace_frames,
    bigint_helper_methods,
    binary_heap_as_slice,
    binary_heap_drain_sorted,
    binary_heap_into_iter_sorted,
    bound_as_ref,
    bound_map,
    box_into_boxed_slice,
    box_into_inner,
    btreemap_alloc,
    buf_read_has_data_left,
    byte_slice_trim_ascii,
    c_size_t,
    c_void_variant,
    can_vector,
    cell_leak,
    cell_update,
    cfg_accessible,
    cfg_eval,
    char_indices_offset,
    char_internals,
    coerce_unsized,
    concat_bytes,
    concat_idents,
    const_align_of_val,
    const_align_of_val_raw,
    const_align_offset,
    const_alloc_error,
    const_alloc_layout,
    const_arguments_as_str,
    const_array_from_ref,
    const_array_into_iter_constructors,
    const_assert_type2,
    const_assume,
    const_bigint_helper_methods,
    const_black_box,
    const_box,
    const_btree_len,
    const_caller_location,
    const_cell_into_inner,
    const_collections_with_hasher,
    const_cow_is_borrowed,
    const_discriminant,
    const_eval_select,
    const_float_bits_conv,
    const_float_classify,
    const_fmt_arguments_new,
    const_format_args,
    const_heap,
    const_index_range_slice_index,
    const_inherent_unchecked_arith,
    const_int_unchecked_arith,
    const_intoiterator_identity,
    const_intrinsic_forget,
    const_intrinsic_raw_eq,
    const_io_structs,
    const_ip,
    const_ipv4,
    const_ipv6,
    const_likely,
    const_location_fields,
    const_maybe_uninit_array_assume_init,
    const_maybe_uninit_as_mut_ptr,
    const_maybe_uninit_assume_init,
    const_maybe_uninit_assume_init_read,
    const_maybe_uninit_uninit_array,
    const_maybe_uninit_write,
    const_maybe_uninit_zeroed,
    const_nonnull_new,
    const_option,
    const_option_ext,
    const_pin,
    const_pointer_byte_offsets,
    const_pref_align_of,
    const_ptr_as_ref,
    const_ptr_is_null,
    const_ptr_sub_ptr,
    const_ptr_write,
    const_raw_ptr_comparison,
    const_replace,
    const_result,
    const_size_of_val,
    const_size_of_val_raw,
    const_slice_first_last,
    const_slice_from_mut_ptr_range,
    const_slice_from_ptr_range,
    const_slice_from_raw_parts_mut,
    const_slice_from_ref,
    const_slice_index,
    const_slice_ptr_len,
    const_slice_split_at_mut,
    const_str_from_utf8,
    const_str_from_utf8_unchecked_mut,
    const_swap,
    const_type_id,
    const_type_name,
    const_unicode_case_lookup,
    const_unsafecell_get_mut,
    const_waker,
    container_error_extra,
    control_flow_enum,
    convert_float_to_int,
    core_intrinsics,
    core_panic,
    core_private_bignum,
    core_private_diy_float,
    cow_is_borrowed,
    cursor_remaining,
    deadline_api,
    dec2flt,
    derive_clone_copy,
    derive_eq,
    dir_entry_ext2,
    discriminant_kind,
    dispatch_from_dyn,
    div_duration,
    downcast_unchecked,
    drain_keep_rest,
    duration_constants,
    duration_consts_float,
    edition_panic,
    entry_insert,
    error_generic_member_access,
    error_in_core,
    error_iter,
    error_reporter,
    error_type_id,
    exact_size_is_empty,
    exclusive_wrapper,
    exit_status_error,
    exitcode_exit_method,
    extend_one,
    fd,
    file_create_new,
    file_set_times,
    float_minimum_maximum,
    float_next_up_down,
    flt2dec,
    fmt_helpers_for_derive,
    fmt_internals,
    fn_traits,
    forget_unsized,
    format_args_nl,
    fs_try_exists,
    future_join,
    gen_future,
    generator_trait,
    generic_assert_internals,
    get_mut_unchecked,
    hash_raw_entry,
    hash_set_entry,
    hasher_prefixfree_extras,
    hashmap_internals,
    hint_must_use,
    inplace_iteration,
    int_roundings,
    internal_output_capture,
    io_error_downcast,
    io_error_more,
    io_error_uncategorized,
    io_slice_advance,
    ip,
    is_ascii_octdigit,
    is_sorted,
    iter_advance_by,
    iter_array_chunks,
    iter_collect_into,
    iter_from_generator,
    iter_intersperse,
    iter_is_partitioned,
    iter_next_chunk,
    iter_order_by,
    iter_partition_in_place,
    iterator_try_collect,
    iterator_try_reduce,
    layout_for_ptr,
    liballoc_internals,
    libstd_sys_internals,
    linked_list_cursors,
    linked_list_remove,
    linux_pidfd,
    log_syntax,
    map_entry_replace,
    map_many_mut,
    map_try_insert,
    maybe_uninit_array_assume_init,
    maybe_uninit_as_bytes,
    maybe_uninit_slice,
    maybe_uninit_uninit_array,
    maybe_uninit_uninit_array_transpose,
    maybe_uninit_write_slice,
    mem_copy_fn,
    mutex_unlock,
    mutex_unpoison,
    new_uninit,
    nonzero_ops,
    numfmt,
    one_sided_range,
    option_get_or_insert_default,
    option_zip,
    panic_always_abort,
    panic_backtrace_config,
    panic_can_unwind,
    panic_info_message,
    panic_internals,
    panic_unwind,
    panic_update_hook,
    path_file_prefix,
    pattern,
    peer_credentials_unix_socket,
    pin_deref_mut,
    pointer_byte_offsets,
    pointer_is_aligned,
    portable_simd,
    prelude_2024,
    print_internals,
    process_exitcode_internals,
    process_internals,
    ptr_alignment_type,
    ptr_as_uninit,
    ptr_internals,
    ptr_mask,
    ptr_metadata,
    ptr_sub_ptr,
    ptr_to_from_bits,
    pub_crate_should_not_need_unstable_attr,
    raw_os_nonzero,
    raw_slice_split,
    raw_vec_internals,
    read_buf,
    ready_into_inner,
    receiver_trait,
    restricted_std,
    result_flattening,
    result_option_inspect,
    round_char_boundary,
    rt,
    sealed,
    seek_stream_len,
    set_ptr_value,
    setgroups,
    sized_type_properties,
    slice_as_chunks,
    slice_concat_ext,
    slice_concat_trait,
    slice_flatten,
    slice_from_ptr_range,
    slice_group_by,
    slice_index_methods,
    slice_internals,
    slice_iter_mut_as_mut_slice,
    slice_partition_dedup,
    slice_pattern,
    slice_ptr_get,
    slice_ptr_len,
    slice_range,
    slice_split_at_unchecked,
    slice_swap_unchecked,
    slice_take,
    sort_floats,
    sort_internals,
    split_array,
    split_as_slice,
    std_internals,
    stdio_makes_pipe,
    stdsimd,
    step_trait,
    str_internals,
    strict_provenance_atomic_ptr,
    string_extend_from_within,
    string_remove_matches,
    sync_unsafe_cell,
    tcp_linger,
    tcp_quickack,
    tcplistener_into_incoming,
    test,
    thin_box,
    thread_id_value,
    thread_local_internals,
    thread_spawn_unchecked,
    trace_macros,
    transmutability,
    trusted_len,
    trusted_random_access,
    trusted_step,
    try_find,
    try_reserve_kind,
    try_trait_v2,
    try_trait_v2_residual,
    try_trait_v2_yeet,
    tuple_trait,
    type_name_of_val,
    unchecked_math,
    unicode_internals,
    unix_set_mark,
    unix_socket_ancillary_data,
    unix_socket_peek,
    unsize,
    unwrap_infallible,
    update_panic_count,
    utf16_extra,
    utf16_extra_const,
    utf8_chunks,
    variant_count,
    vec_into_raw_parts,
    vec_push_within_capacity,
    vec_split_at_spare,
    waker_getters,
    wrapping_int_impl,
    wrapping_next_power_of_two,
    write_all_vectored,
    yeet_desugar_details
)]

use aoc_2022::*;

const ROCKS: &str = "####

.#.
###
.#.

..#
..#
###

#
#
#
#

##
##";

#[derive(Default)]
struct Grid {
    rows: BTreeMap<i64, [bool; 7]>,
}

impl Grid {
    fn insert(&mut self, c: Coordinate2D) {
        self.rows.entry(c.1).or_default()[c.0 as usize] = true;
    }

    fn contains(&self, c: Coordinate2D) -> bool {
        self.rows.get(&c.1).is_some_and(|x| x[c.0 as usize])
    }

    fn bounds(&self) -> (i64, i64) {
        let &min = self.rows.iter().find(|x| x.1.iter().any(|&x| x)).unwrap().0;
        let &max = self
            .rows
            .iter()
            .rev()
            .find(|x| x.1.iter().any(|&x| x))
            .unwrap()
            .0;
        (min, max)
    }

    fn retain(&mut self, mut f: impl FnMut(i64) -> bool) {
        self.rows.retain(|&k, _| f(k))
    }
}

fn reset_if_needed(grid: &mut Grid) {
    let (min, max) = grid.bounds();

    for row in min..=max {
        if (0..7).all(|i| grid.contains(Coordinate2D(i, row))) {
            grid.retain(|r| r <= row + 10);
            grid.insert(Coordinate2D(0, max));
            return;
        }
    }
}

fn main() {
    let input = load_input(17);
    let input = input.trim();

    let cycles = input.len();

    let mut input = input.chars().cycle().peekable();

    let rocks = ROCKS
        .split("\n\n")
        .map(|r| parse_hashset(r, |f| f == '#'))
        .collect_vec();

    let mut rock_n = 0;

    let mut grid = Grid::default();
    for i in 0..7 {
        grid.insert(Coordinate2D(i, 0));
    }

    let mut incs = vec![];

    let total = 1000000000000;

    let mut prev_height = 0;

    let start_count = (cycles * 2).max(2022);

    let mut part1 = 0;

    for i in 0..start_count {
        let rock = rocks[rock_n].clone();
        let rock_width = [4, 3, 3, 1, 2][rock_n];
        let rock_height = [1, 3, 3, 4, 2][rock_n];

        let mut corner = Coordinate2D(2, grid.bounds().0 - 3 - rock_height);

        while let Some(&c) = input.peek() {
            let left = corner.0;
            let right = corner.0 + rock_width;

            if check_overlap(&rock, &grid, corner) {
                corner = corner.up(1);
                break;
            }

            match c {
                '<' => {
                    if left > 0 && !check_overlap(&rock, &grid, corner.left(1)) {
                        corner = corner.left(1)
                    }
                }
                '>' => {
                    if right < 7 && !check_overlap(&rock, &grid, corner.right(1)) {
                        corner = corner.right(1)
                    }
                }
                _ => unreachable!(),
            }

            corner = corner.down(1);

            input.next();
        }

        for &c in rock.iter() {
            grid.insert(corner + c);
        }

        rock_n += 1;
        rock_n %= rocks.len();

        reset_if_needed(&mut grid);

        let (min, max) = grid.bounds();

        let height = max - min;

        if i + 1 == 2022 && part1 == 0 {
            part1 = height;
            cp(part1);
        }

        incs.push(height - prev_height);
        prev_height = height;
    }

    let split = incs.split(|&x| x == 0).collect_vec();

    let longest_split = split.iter().skip(1).max_by_key(|x| x.len()).unwrap();
    let split2 = split.split(|x| x == longest_split).collect_vec();

    let repeats_every = longest_split.len()
        + 1
        + split2[split2.len() - 2]
            .iter()
            .map(|x| x.len() + 1)
            .sum::<usize>();

    let mut expected_incs_len = incs.len();

    while (total - expected_incs_len) % repeats_every != 0 {
        expected_incs_len += 1;
    }

    let cycle_height = incs.iter().rev().take(repeats_every).sum::<i64>();

    while incs.len() < expected_incs_len {
        let next = incs[incs.len() - repeats_every];
        incs.push(next);
    }

    let mut sum = incs.iter().sum::<i64>();

    sum += cycle_height * ((total - incs.len()) / repeats_every).int();

    cp(sum);
}

fn check_overlap(rock: &HashSet<Coordinate2D>, grid: &Grid, corner: Coordinate2D) -> bool {
    for &c in rock.iter() {
        if grid.contains(corner + c) {
            return true;
        }
    }
    false
}
