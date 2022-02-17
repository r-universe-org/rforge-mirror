#!/bin/sh
# to be used as with git svn clone -authors-prog=findauthor.sh
Rscript -e "cat(rforgemirror:::find_author_prog('$1'))"
