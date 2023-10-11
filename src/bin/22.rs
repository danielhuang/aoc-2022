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

fn main() {
    let input = load_input(22);

    let (header, body) = input.split_once("\n\n").unwrap();

    let grid = parse_grid(header, |c| c, ' ');

    let binding = body.replace('R', " R ").replace('L', " L ");
    let path = binding.split_whitespace().collect_vec();

    for cube in [false, true] {
        if cube && DEBUG {
            println!("Part 2 does not work with sample input");
            return;
        }

        let mut unit = Coordinate2D(1, 0);
        let mut pos = *grid
            .iter()
            .filter(|x| *x.1 == '.' && (x.0).1 == 0)
            .min_by_key(|x| (x.0).0)
            .unwrap()
            .0;

        let cube_width = header.lines().map(|x| x.trim().len()).min().unwrap().int();

        for &ins in &path {
            if let Ok(num) = ins.parse::<i64>() {
                for _ in 0..num {
                    let before_moving = pos;
                    let before_moving_unit = unit;
                    pos += unit;
                    if grid[pos] == '#' {
                        pos += -unit;
                    }
                    if grid[pos] == ' ' {
                        if cube {
                            if let Some(new_pos) = before_moving
                                .go_straight(unit + unit.rotate_right())
                                .take(cube_width.uint() + 1)
                                .find(|p| grid[p] != ' ')
                            {
                                unit = unit.rotate_right();
                                pos = new_pos;
                            } else if let Some(new_pos) = before_moving
                                .go_straight(unit + unit.rotate_left())
                                .take(cube_width.uint() + 1)
                                .find(|p| grid[p] != ' ')
                            {
                                unit = unit.rotate_left();
                                pos = new_pos;
                            } else {
                                let block = before_moving / cube_width;
                                let offset = before_moving - block * cube_width;
                                if block == Coordinate2D(1, 0) && unit == Coordinate2D(-1, 0) {
                                    pos = Coordinate2D(0, 2) * cube_width
                                        + Coordinate2D(0, cube_width - offset.1 - 1);
                                    unit = -unit;
                                } else if block == Coordinate2D(1, 0) && unit == Coordinate2D(0, -1)
                                {
                                    pos = Coordinate2D(0, 3) * cube_width + offset.rotate_right();
                                    unit = unit.rotate_right()
                                } else if block == Coordinate2D(2, 0) && unit == Coordinate2D(0, -1)
                                {
                                    pos = Coordinate2D(0, 3) * cube_width
                                        + offset.down(cube_width - 1);
                                } else if block == Coordinate2D(2, 0) && unit == Coordinate2D(1, 0)
                                {
                                    pos = Coordinate2D(1, 2) * cube_width
                                        + Coordinate2D(offset.0, cube_width - offset.1 - 1);
                                    unit = -unit;
                                } else if block == Coordinate2D(0, 3) && unit == Coordinate2D(-1, 0)
                                {
                                    pos = Coordinate2D(1, 0) * cube_width + offset.rotate_left();
                                    unit = unit.rotate_left();
                                } else if block == Coordinate2D(0, 3) && unit == Coordinate2D(0, 1)
                                {
                                    pos =
                                        Coordinate2D(2, 0) * cube_width + offset.up(cube_width - 1);
                                } else if block == Coordinate2D(1, 2) && unit == Coordinate2D(1, 0)
                                {
                                    pos = Coordinate2D(2, 0) * cube_width
                                        + Coordinate2D(offset.0, cube_width - offset.1 - 1);
                                    unit = -unit;
                                } else if block == Coordinate2D(0, 2) && unit == Coordinate2D(-1, 0)
                                {
                                    pos = Coordinate2D(1, 0) * cube_width
                                        + Coordinate2D(0, cube_width - offset.1 - 1);
                                    unit = -unit;
                                } else {
                                    unreachable!()
                                }
                            }
                        } else {
                            pos = before_moving;
                            while grid[pos] != ' ' {
                                pos -= unit;
                            }
                            pos += unit;
                        }

                        if grid[pos] == '#' {
                            pos = before_moving;
                            unit = before_moving_unit;
                        }
                    }
                }
            } else if ins == "L" {
                unit = unit.rotate_left();
            } else if ins == "R" {
                unit = unit.rotate_right();
            } else {
                unreachable!()
            }
        }

        let row = pos.1 + 1;
        let col = pos.0 + 1;

        let facing = match (unit.0, unit.1) {
            (1, 0) => 0,
            (0, 1) => 1,
            (-1, 0) => 2,
            (0, -1) => 3,
            _ => unreachable!(),
        };

        cp(row * 1000 + col * 4 + facing);
    }
}
