#!/bin/bash
set -e
set -o nounset

# First version written on or around November 22, 2016.
#
# This script takes a URL from the clipboard and downloads it to the directory
# $dir, which is ~/www/YYYY-MM-DD (and makes a record in an index file).  It
# then uses lynx to read the file and print a formatted plaintext dump to
# standard output.  It's mostly useful when invoked from within Vim, so that
# one can read a webpage in the editor.

datetime=$(date +"%Y-%m-%dT%H-%M-%S%z")
fname="$datetime".html
dir=$HOME/www/"${datetime%T*}"
useragent="Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:49.0) Gecko/20100101 Firefox/49.0"
downloader=${1:-wget}

mkdir -p $dir

if [ $downloader == "curl" ]
then
  xsel -ob | xargs curl -L --silent --compressed -A "$useragent" > $dir/$fname
else
  xsel -ob | wget --quiet --user-agent="$useragent" --convert-links \
    -i - -O $dir/$fname
fi

echo -n $fname >> $dir/index.txt
echo -ne "\t" >> $dir/index.txt
xsel -ob >> $dir/index.txt
echo "" >> $dir/index.txt

lynx -localhost -dump $dir/$fname
