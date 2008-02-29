#!/bin/bash

error()
{
    echo "$1"
    exit 1
}

# First figure out our directories

src=$(dirname $0)/home
dest="$1"
if [ "$dest" == "" ]; then
    dest="$HOME"
fi
if [ "$dest" == "" ]; then
    error "Unable to determine destination."
fi

# Next ensure all of the directories exist

for dir in $(find $src -type d -print | sed -e "s#$src#$dest#"); do
    if [ ! -d $dir ]; then
        mkdir -p $dir
    fi
done

# Now link all of the files that we don't have linked

for file in $(find $src -type f -print); do
    dest_file=$(echo $file | sed -e "s#$src#$dest#")
    if [ -e $dest_file ]; then
        if [ ! $file -ef $dest_file ]; then
            ln -i $file $dest_file
        fi
    else
        ln $file $dest_file
    fi
done
