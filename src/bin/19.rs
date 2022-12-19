#![feature(
    absolute_path,
    addr_parse_ascii,
    alloc_error_hook,
    alloc_internals,
    alloc_layout_extra,
    allocator_api,
    arc_unwrap_or_clone,
    array_chunks,
    array_error_internals,
    array_into_iter_constructors,
    array_methods,
    array_try_from_fn,
    array_try_map,
    array_windows,
    array_zip,
    as_array_of_cells,
    assert_matches,
    async_iter_from_iter,
    async_iterator,
    atomic_bool_fetch_not,
    atomic_from_mut,
    atomic_mut_ptr,
    backtrace_frames,
    bigint_helper_methods,
    binary_heap_as_slice,
    binary_heap_drain_sorted,
    binary_heap_into_iter_sorted,
    binary_heap_retain,
    bound_as_ref,
    bound_map,
    box_into_boxed_slice,
    box_into_inner,
    btree_drain_filter,
    btreemap_alloc,
    buf_read_has_data_left,
    build_hasher_simple_hash_one,
    byte_slice_trim_ascii,
    c_size_t,
    c_void_variant,
    can_vector,
    cell_leak,
    cell_update,
    cfg_accessible,
    cfg_eval,
    char_error_internals,
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
    const_bool_to_option,
    const_borrow,
    const_box,
    const_btree_len,
    const_caller_location,
    const_cell_into_inner,
    const_clone,
    const_cmp,
    const_collections_with_hasher,
    const_convert,
    const_cow_is_borrowed,
    const_cstr_methods,
    const_default_impls,
    const_deref,
    const_discriminant,
    const_eval_select,
    const_float_bits_conv,
    const_float_classify,
    const_fmt_arguments_new,
    const_fn_trait_ref_impls,
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
    const_is_char_boundary,
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
    const_nonnull_slice_from_raw_parts,
    const_num_from_num,
    const_ops,
    const_option,
    const_option_cloned,
    const_option_ext,
    const_pin,
    const_pointer_byte_offsets,
    const_pref_align_of,
    const_ptr_as_ref,
    const_ptr_is_null,
    const_ptr_read,
    const_ptr_sub_ptr,
    const_ptr_write,
    const_raw_ptr_comparison,
    const_replace,
    const_result,
    const_result_drop,
    const_reverse,
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
    const_slice_split_at_not_mut,
    const_socketaddr,
    const_str_from_utf8,
    const_str_from_utf8_unchecked_mut,
    const_swap,
    const_transmute_copy,
    const_type_id,
    const_type_name,
    const_unicode_case_lookup,
    const_unsafecell_get_mut,
    const_waker,
    const_weak_new,
    container_error_extra,
    control_flow_enum,
    convert_float_to_int,
    core_intrinsics,
    core_panic,
    core_private_bignum,
    core_private_diy_float,
    cow_is_borrowed,
    cstr_from_bytes_until_nul,
    cstr_internals,
    cstr_is_empty,
    cursor_remaining,
    deadline_api,
    dec2flt,
    default_free_fn,
    derive_clone_copy,
    derive_eq,
    dir_entry_ext2,
    discriminant_kind,
    dispatch_from_dyn,
    div_duration,
    downcast_unchecked,
    drain_filter,
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
    hash_drain_filter,
    hash_raw_entry,
    hash_set_entry,
    hasher_prefixfree_extras,
    hashmap_internals,
    hint_must_use,
    inplace_iteration,
    int_error_internals,
    int_roundings,
    integer_atomics,
    internal_output_capture,
    io_error_downcast,
    io_error_more,
    io_error_other,
    io_error_uncategorized,
    io_slice_advance,
    ip,
    is_ascii_octdigit,
    is_some_and,
    is_sorted,
    is_terminal,
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
    libstd_thread_internals,
    linked_list_cursors,
    linked_list_remove,
    linux_pidfd,
    local_key_cell_methods,
    log_syntax,
    main_separator_str,
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
    nonnull_slice_from_raw_parts,
    nonzero_bits,
    nonzero_min_max,
    nonzero_negation_ops,
    nonzero_ops,
    numfmt,
    once_cell,
    one_sided_range,
    option_get_or_insert_default,
    option_result_contains,
    option_zip,
    os_fd,
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
    pin_macro,
    pointer_byte_offsets,
    pointer_is_aligned,
    poll_ready,
    portable_simd,
    prelude_2024,
    print_internals,
    process_exitcode_internals,
    process_internals,
    provide_any,
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
    result_contains_err,
    result_flattening,
    result_option_inspect,
    round_char_boundary,
    rt,
    saturating_int_assign_impl,
    saturating_int_impl,
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
    str_split_as_str,
    str_split_inclusive_as_str,
    str_split_whitespace_as_str,
    strict_provenance_atomic_ptr,
    string_extend_from_within,
    string_leak,
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
    unix_chown,
    unix_set_mark,
    unix_socket_abstract,
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
use rayon::prelude::{IndexedParallelIterator, IntoParallelRefIterator, ParallelIterator};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct State {
    ores: i64,
    clays: i64,
    obsidians: i64,
    geodes: i64,
    ore_robots: i64,
    clay_robots: i64,
    obsidian_robots: i64,
    geode_robots: i64,
    time_left: i64,
}

impl State {
    fn tick(&mut self) {
        assert!(self.time_left > 0);
        self.ores += self.ore_robots;
        self.clays += self.clay_robots;
        self.obsidians += self.obsidian_robots;
        self.geodes += self.geode_robots;
        self.time_left -= 1;
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct Blueprint {
    ore_cost_ore: i64,
    clay_cost_ore: i64,
    obsidian_cost_ore: i64,
    obsidian_cost_clay: i64,
    geode_cost_ore: i64,
    geode_cost_obsidian: i64,
}

fn geodes_possible(blueprint: Blueprint, state: State, max: &mut i64) -> i64 {
    let mut state = state;

    let mut skipped_ore = false;
    let mut skipped_clay = false;
    let mut skipped_obsidian = false;
    let mut skipped_geode = false;

    while state.time_left > 0 {
        if *max > assume_free_robots(state.clone()) {
            return *max;
        }

        if !skipped_ore
            && state.time_left > 0
            && state.ores >= blueprint.geode_cost_ore
            && state.obsidians >= blueprint.geode_cost_obsidian
        {
            skipped_ore = true;

            let mut state2 = state.clone();
            state2.ores -= blueprint.geode_cost_ore;
            state2.obsidians -= blueprint.geode_cost_obsidian;

            state2.tick();
            state2.geode_robots += 1;

            let cur = geodes_possible(blueprint, state2, max);

            *max = cur.max(*max);
        } else {
            if !skipped_clay
                && state.time_left > 0
                && state.ores >= blueprint.obsidian_cost_ore
                && state.clays >= blueprint.obsidian_cost_clay
                && state.obsidian_robots < blueprint.geode_cost_obsidian
            {
                skipped_clay = true;

                let mut state2 = state.clone();
                state2.ores -= blueprint.obsidian_cost_ore;
                state2.clays -= blueprint.obsidian_cost_clay;

                state2.tick();
                state2.obsidian_robots += 1;

                let cur = geodes_possible(blueprint, state2, max);

                *max = cur.max(*max);
            }

            if !skipped_obsidian
                && state.time_left > 0
                && state.ores >= blueprint.clay_cost_ore
                && state.clay_robots < blueprint.obsidian_cost_clay
            {
                skipped_obsidian = true;

                let mut state2 = state.clone();
                state2.ores -= blueprint.clay_cost_ore;

                state2.tick();
                state2.clay_robots += 1;

                let cur = geodes_possible(blueprint, state2, max);

                *max = cur.max(*max);
            }

            if !skipped_geode
                && state.time_left > 0
                && state.ores >= blueprint.ore_cost_ore
                && state.ore_robots
                    < blueprint.ore_cost_ore
                        + blueprint.clay_cost_ore
                        + blueprint.obsidian_cost_ore
                        + blueprint.geode_cost_ore
            {
                skipped_geode = true;

                let mut state2 = state.clone();
                state2.ores -= blueprint.ore_cost_ore;

                state2.tick();
                state2.ore_robots += 1;

                let cur = geodes_possible(blueprint, state2, max);

                *max = cur.max(*max);
            }
        }

        state.tick();
        *max = state.geodes.max(*max);
    }

    *max
}

fn assume_free_robots(state: State) -> i64 {
    let time_left = state.time_left;
    state.geodes + state.geode_robots * time_left + (0..time_left).sum::<i64>()
}

fn init(time_left: i64) -> State {
    State {
        ores: 0,
        clays: 0,
        obsidians: 0,
        geodes: 0,
        ore_robots: 1,
        clay_robots: 0,
        obsidian_robots: 0,
        geode_robots: 0,
        time_left,
    }
}

fn main() {
    let input = load_input(19);

    let input = if DEBUG {
        input.split("\n\n").collect_vec()
    } else {
        input.lines().collect_vec()
    };

    let mut v = vec![];

    for line in input {
        let [id, ore_cost_ore, clay_cost_ore, obsidian_cost_ore, obsidian_cost_clay, geode_cost_ore, geode_cost_obsidian] =
            grab_nums(line);

        let blueprint = Blueprint {
            ore_cost_ore,
            clay_cost_ore,
            obsidian_cost_ore,
            obsidian_cost_clay,
            geode_cost_ore,
            geode_cost_obsidian,
        };

        v.push((blueprint, id));
    }

    let part1: i64 = v
        .par_iter()
        .map(|(blueprint, id)| geodes_possible(*blueprint, init(24), &mut 0) * id)
        .sum();

    cp(part1);

    let part2: i64 = v
        .par_iter()
        .take(3)
        .map(|(blueprint, _)| geodes_possible(*blueprint, init(32), &mut 0))
        .product();

    cp(part2);
}
