#!/bin/bash

cd ${0%/*}

find home -type f -print | while read file; do
    dest=$(sed -e "s#home#$HOME#" <<< "$file")
    mv -v $dest $dest.bak
    cp -v $file $dest
done
