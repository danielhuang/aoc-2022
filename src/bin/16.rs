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

use std::mem::{swap, take};

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

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
struct State {
    pos: String,
    total_rate: i64,
    opened: BTreeSet<String>,
    steps: i32,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
struct State2 {
    mpos: String,
    epos: String,
    lost: i64,
    opened: BTreeSet<String>,
    minutes: i64,
}

impl State2 {
    fn m_goto(&self, pos: String) -> State2 {
        let mut s = self.clone();
        s.mpos = pos;
        s
    }

    fn e_goto(&self, pos: String) -> State2 {
        let mut s = self.clone();
        s.epos = pos;
        s
    }

    fn open(&self, pos: &str, rate: i64) -> Option<State2> {
        if !self.opened.contains(pos) && rate > 0 {
            let mut s = self.clone();
            s.lost -= rate;
            s.opened.insert(pos.to_string());
            Some(s)
        } else {
            None
        }
    }

    fn tick(&self) -> State2 {
        let mut s = self.clone();
        s.minutes += 1;
        s
    }
}

fn main() {
    let input = load_input(16);

    let mut valves = HashMap::new();

    for line in input.lines() {
        let [rate] = grab_nums(line);
        let from = line
            .strip_prefix("Valve ")
            .unwrap()
            .chars()
            .take(2)
            .collect_string();

        let mut to = vec![];
        for mut item in line.rsplit(" ") {
            if item.ends_with(",") {
                item = item.strip_suffix(",").unwrap();
            }

            if item.len() != 2 {
                break;
            }
            to.push(item.to_string());
        }

        valves.insert(from, (rate, to));
    }

    let total_pressure = valves.values().map(|x| x.0).sum::<i64>();
    dbg!(&total_pressure);

    let mut offset = 0;

    let total_minutes = 26;

    let ops = loop {
        let state = State2 {
            epos: "AA".into(),
            mpos: "AA".into(),
            lost: total_pressure,
            opened: Default::default(),
            minutes: 1,
        };

        let mut max_minutes = 0;

        let ops = dijkstra(
            &state,
            |item| {
                let mut result: Vec<Option<(State2, i64)>> = vec![];

                // if item.minutes >= total_minutes {
                //     return vec![];
                // }

                if item.minutes > max_minutes {
                    dbg!(&item.minutes);
                    max_minutes = item.minutes;
                }

                let mut item = item.clone();

                let (me_rate, me_options) = valves[&item.mpos].clone();
                let (el_rate, el_options) = valves[&item.epos].clone();

                result.push(Some((item.tick(), item.lost)));

                if item.mpos != item.epos {
                    // M opens, E nothing
                    result.push(
                        item.open(&item.mpos, me_rate)
                            .map(|x| (x.tick(), item.lost - me_rate)),
                    );
                    // E opens, M nothing
                    result.push(
                        item.open(&item.epos, el_rate)
                            .map(|x| (x.tick(), item.lost - el_rate)),
                    );
                    // both open
                    result.push(
                        item.open(&item.epos, el_rate)
                            .and_then(|item2| item2.open(&item2.mpos, me_rate))
                            .map(|x| (x.tick(), item.lost - me_rate - el_rate)),
                    );
                } else {
                    // same spot: one can open, other does nothing
                    // E opens, M nothing
                    result.push(
                        item.open(&item.epos, el_rate)
                            .map(|x| (x.tick(), item.lost - el_rate)),
                    );
                }

                for el_option in el_options.clone() {
                    for me_option in me_options.clone() {
                        // both move
                        result.push(Some((
                            item.m_goto(me_option).e_goto(el_option.clone()).tick(),
                            item.lost,
                        )));
                    }
                }

                for el_option in el_options {
                    // E move, M open
                    result.push(
                        item.e_goto(el_option.clone())
                            .open(&item.mpos, me_rate)
                            .map(|x| (x.tick(), item.lost - me_rate)),
                    );
                    // E move, M nothing
                    result.push(Some((item.e_goto(el_option.clone()).tick(), item.lost)));
                }

                for me_option in me_options {
                    // M move, E open
                    result.push(
                        item.m_goto(me_option.clone())
                            .open(&item.epos, el_rate)
                            .map(|x| (x.tick(), item.lost - el_rate)),
                    );
                    // M move, E nothing
                    result.push(Some((item.m_goto(me_option.clone()).tick(), item.lost)));
                }

                // both nothing
                result.push(Some((item.tick(), item.lost)));

                for next in result.iter() {
                    if let Some(next) = next {
                        assert!(next.0.minutes == item.minutes + 1);
                    }
                }

                for next in result.iter_mut() {
                    if let Some(next) = next {
                        // next.1 = item.lost;
                    }
                }

                result.into_iter().flatten().collect_vec()
            },
            |item| item.minutes == total_minutes,
        );

        if let Some(ops) = ops {
            break ops;
        }

        offset += 1;

        dbg!(&offset);
    };

    for (min, state) in ops.0.iter().enumerate() {
        println!("MINUTE {} {:#?}", min + 1, state);
        println!();
    }

    dbg!(&ops.0.len());

    dbg!(total_pressure * total_minutes - ops.1);

    let mut released = 0;
    for state in ops.0.clone() {
        released += total_pressure - state.lost;
    }
    if ops.0.len() != total_minutes as usize {
        released += (total_minutes - ops.0.len() as i64) * (total_pressure - offset);
        panic!("bad");
    }
    dbg!(&offset);
    cp(released);

    // let mut parents: HashMap<State, State> = HashMap::new();

    // let reach = dfs_reach(state, |item| {
    //     let mut result = vec![];

    //     if item.steps == 30 {
    //         return result;
    //     }

    //     let (rate, options) = valves[&item.pos].clone();

    //     if !item.opened.contains(&item.pos) {
    //         let mut opened = item.opened.clone();
    //         opened.insert(item.pos.to_string());
    //         result.push(State {
    //             pos: item.pos.clone(),
    //             total_rate: item.total_rate + rate,
    //             opened,
    //             steps: item.steps + 1,
    //         });
    //     }

    //     for option in options {
    //         result.push(State {
    //             pos: option,
    //             total_rate: item.total_rate,
    //             opened: item.opened.clone(),
    //             steps: item.steps + 1,
    //         });
    //     }

    //     result.push(State {
    //         pos: item.pos.clone(),
    //         total_rate: item.total_rate,
    //         opened: item.opened.clone(),
    //         steps: item.steps + 1,
    //     });

    //     for r in result.clone() {
    //         if parents.contains_key(&r) {
    //             // dbg!(&parents);
    //             // dbg!(&r);
    //             // dbg!(&parents[&r]);
    //             // dbg!(&item);
    //             let mut released = 0;

    //             let other = parents[&r].clone();
    //             if other.total_rate < r.total_rate {
    //                 parents.insert(r, item.clone());
    //             }
    //         } else {
    //             parents.insert(r, item.clone());
    //         }
    //     }

    //     // dbg!(&result);

    //     result
    // })
    // .collect_vec();

    // let mut max = 0;
    // for attempt in reach {
    //     let mut stack = vec![];
    //     let mut top = attempt.clone();
    //     while let Some(parent) = parents.get(&top).cloned() {
    //         stack.push(parent.clone());
    //         top = parent;
    //     }
    //     stack.reverse();
    //     stack.push(attempt);

    //     let mut released = 0;
    //     for min in stack.clone() {
    //         released += min.total_rate;
    //     }
    //     if released > max {
    //         max = released;
    //     }
    // }

    // cp(max);
    // let mut visited = HashSet::new();

    // let mut edge = HashSet::new();
    // edge.insert(state);

    // for _ in 0..30 {
    //     for item in take(&mut edge) {
    //         if !visited.contains(&item) {
    //             let (rate, options) = valves[&item.pos].clone();
    //             edge.insert(State {
    //                 pos: item.pos.clone(),
    //                 total_rate: item.total_rate + rate,
    //                 released: item.total_rate + item.released,
    //             });
    //             for option in options {
    //                 edge.insert(State {
    //                     pos: option,
    //                     total_rate: item.total_rate,
    //                     released: item.total_rate + item.released,
    //                 });
    //             }
    //         }
    //     }
    //     visited.extend(edge.clone());
    // }

    // visited.extend(edge);

    // dbg!(&visited);
}
