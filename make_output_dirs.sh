#!/bin/bash

for dir in output logs; do
    if [ ! -d "$dir" ]; then
        if [ -d /dev/shm ]; then
            name="/dev/shm/compiler-$(id -u -n)-$dir-$(basename "$(pwd)")"
            rm -rf "$name"
            mkdir -p -m '0700' "$name"
            ln -s "$name" "$dir"
        else
            mkdir "$dir"
        fi
    fi
done

