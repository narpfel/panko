#!/usr/bin/env bash

pending_snapshots="$(cargo insta pending-snapshots)"

if [ -n "$pending_snapshots" ]; then
    echo "$pending_snapshots"
    exit 1
fi
