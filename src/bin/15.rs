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

fn tune(c: Coordinate2D) -> i64 {
    c.0 * 4000000 + c.1
}

fn limit() -> i64 {
    if DEBUG {
        20
    } else {
        4000000
    }
}

fn inside(c: Coordinate2D) -> bool {
    c.0 >= 0 && c.0 <= limit() && c.1 >= 0 && c.1 <= limit()
}

fn line_slope(a: Coordinate2D, b: Coordinate2D) -> i64 {
    assert!(a.is_exactly_diagonal_with(b));
    let diff = b - a;
    diff.1 / diff.0
}

fn is_between(c: Coordinate2D, start: Coordinate2D, end: Coordinate2D) -> bool {
    assert!(start.is_exactly_diagonal_with(end));

    let (c, start, end) = (c - start, Coordinate2D(0, 0), end - start);

    (start == c || end == c)
        || line_slope(end, start) == line_slope(end, c)
            && (end - start).manhat_diag() >= (end - c).manhat_diag()
}

fn find_y(Coordinate2D(x, y): Coordinate2D, m: i64) -> i64 {
    y - m * x
}

fn find_intersect(
    line1: (Coordinate2D, Coordinate2D),
    line2: (Coordinate2D, Coordinate2D),
) -> Option<Coordinate2D> {
    if (line2.0 - line1.0).manhat() % 2 != 0 {
        return None;
    }

    let slope1 = line_slope(line1.0, line1.1);
    let slope2 = line_slope(line2.0, line2.1);

    if slope1 == slope2 {
        return None;
    }

    let y1 = find_y(line1.0, slope1);
    let y2 = find_y(line2.0, slope2);

    let x = (y2 - y1) / (slope1 - slope2);
    let y = slope1 * x + y1;

    let point = Coordinate2D(x, y);

    if is_between(point, line1.0, line1.1) {
        Some(point)
    } else {
        None
    }
}

fn main() {
    let input = load_input(15);

    let row = if DEBUG { 10 } else { 2000000 };

    let mut beacons = HashSet::new();

    let mut no_zones = Vec::new();

    let mut row_no_zone = Intervals::default();

    for line in input.lines() {
        let [x1, y1, x2, y2] = grab_nums(line);
        let sensor = Coordinate2D(x1, y1);
        let beacon = Coordinate2D(x2, y2);

        beacons.insert(beacon);

        let dist = (beacon - sensor).manhat();
        no_zones.push((sensor, dist));

        let from_row = (row - sensor.1).abs();
        let row_hw = (from_row - dist).abs();

        row_no_zone.add(sensor.0 - row_hw, sensor.0 + row_hw + 1);
    }

    for beacon in beacons.iter_c() {
        if beacon.1 == row {
            row_no_zone.remove_one(beacon.0);
        }
    }

    cp(row_no_zone.covered_size());

    let mut lines = vec![];

    for (center, dist) in no_zones.iter_c() {
        let top = center.up(dist + 1);
        let right = center.right(dist + 1);
        let bottom = center.down(dist + 1);
        let left = center.left(dist + 1);

        lines.push((top, right.up(1).left(1)));
        lines.push((right, bottom.up(1).right(1)));
        lines.push((bottom, left.down(1).right(1)));
        lines.push((left, top.down(1).left(1)));
    }

    let mut intersections = DefaultHashMap::new(0);

    for (line1, line2) in lines.iter().copied().tuple_combinations() {
        if let Some(intersection) = find_intersect(line1, line2) {
            intersections[intersection] += 1;
        }
    }

    for (&potential_beacon, _) in intersections.iter().filter(|x| *x.1 >= 4) {
        if inside(potential_beacon) {
            let mut possible = true;
            for (sensor, cannot_be_within) in no_zones.iter_c() {
                let dist = (potential_beacon - sensor).manhat();
                if dist <= cannot_be_within {
                    possible = false;
                }
            }
            if possible {
                cp(tune(potential_beacon));
                return;
            }
        }
    }
}
