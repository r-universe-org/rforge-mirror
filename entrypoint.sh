#!/bin/bash -l
set -e
if [ "${1}" = "true" ]; then
echo "Running FULL R-forge mirror"
Rscript -e "rforgemirror::rforge_mirror(this_week = FALSE)"
else
echo "Running quick R-forge mirror"
Rscript -e "rforgemirror::rforge_mirror()"
fi

if [ "${2}" = "true" ]; then
echo "Cleaning up dead repositories..."
Rscript -e "rforgemirror::rforge_cleanup_repos()"
fi

echo "Action complete!"
