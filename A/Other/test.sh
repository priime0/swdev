#!/usr/bin/env sh

actual=$(../xjson < ../Tests/0-in.json)
expected=$(cat ../Tests/0-out.json)

if [ "$actual" = "$expected" ]
then
    echo "Tests passed"
    exit 0
else
    echo "Tests failed" >&2
    exit 1
fi
