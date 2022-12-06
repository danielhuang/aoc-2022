#!/usr/bin/env bash
AOC_COPY_CLIPBOARD=1 MIMALLOC_LARGE_OS_PAGES=1 MIMALLOC_PAGE_RESET=0 cargo watch -x "run --bin $*" --clear --delay 0
