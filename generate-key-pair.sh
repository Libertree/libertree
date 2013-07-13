#!/bin/bash

privkey_file="${1:-private.key}"

if [ -e "$privkey_file" ]; then
    echo "$privkey_file exists.  Not overwriting; aborting." >&2
    exit 1
fi

openssl genrsa -out "$privkey_file" 2048 && \
    chmod 0600 "$privkey_file" && \
    echo "Generated $privkey_file."
