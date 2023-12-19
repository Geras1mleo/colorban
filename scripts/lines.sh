#!/bin/bash

find ./src ./lib ./test -name "*.hs" -exec wc -l {} \; | cut -f1 -d ' ' | sed -z "s/\n/ + /g;s/+ $/\n/" | bc
