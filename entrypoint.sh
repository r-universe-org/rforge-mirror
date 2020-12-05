#!/bin/bash -l
set -e

# First: cleanup dead repos (this is quick)
if [ "${2}" = "true" ]; then
echo "Cleaning up dead repositories..."
Rscript -e "rforgemirror::rforge_cleanup_repos()"
fi

# Clone and sync all the things
if [ "${1}" = "true" ]; then
echo "Running FULL R-forge mirror"
Rscript -e "rforgemirror::rforge_mirror(this_week = FALSE)"
else
echo "Running quick R-forge mirror"
Rscript -e "rforgemirror::rforge_mirror()"
fi

echo "Action complete!"
