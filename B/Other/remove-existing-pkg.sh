#!/usr/bin/env sh

present=$(raco pkg show -a | grep -i "Other" | wc -l)

if [ "$present" = "1" ]; then
    echo "removing"
    raco pkg remove Other
else
    echo "nothing to remove"
fi
