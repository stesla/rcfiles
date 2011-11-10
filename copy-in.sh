#!/bin/bash

cd ${0%/*}

find home -type f -print | while read file; do
    cp -v $(sed -e "s#home#$HOME#" <<< "$file") $file
done
