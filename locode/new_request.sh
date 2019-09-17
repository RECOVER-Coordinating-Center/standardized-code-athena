#!/bin/bash

# Set up new directory tree for a request using the standard framework
#
# Usage: /path/to/new_request.sh dir_name

target=${1:-new_request}
if [[ $(ls "$target"/* >/dev/null 2>&1) || \
      $(ls "$target"/.[a-z]* >/dev/null 2>&1 ; \
      echo $?) -eq 0  ]]
then
    echo >&2 "Target directory $target already has content."
    exit 1
fi
if [[ ! -d "$target" ]]; then mkdir -p "$target"; fi
target=$(cd "$target"; pwd)

src=$(dirname $0)
src=${src:-$PWD}
if [[ ! -d "$src" || ! -e "$src/new_request.sh" ]]
then
    echo >&2 "Unable to find standard framework"
    exit 1
fi

cd "$src/.."
cp -R code  local locode reporting request results site specs "$target"
cp .gitignore "$target"

cd "$target"
if [[ $(git status >/dev/null 2>&1; echo $?) -ne 0 ]]; then git init .; fi
git add .
git commit -m 'Standard framework'

echo "Done."

