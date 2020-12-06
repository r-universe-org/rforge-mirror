#!/bin/bash -l
set -e
ACTION="${1}"

if [ -z "$ACTION" ]; then
ACTION="quick"
fi

case $ACTION in
  quick)
    echo "Running quick R-forge mirror"
    Rscript -e "rforgemirror::rforge_mirror(this_week = TRUE)"
    ;;
  full)
    echo "Running FULL R-forge mirror"
    Rscript -e "rforgemirror::rforge_mirror(this_week = FALSE)"
    ;;
  cleanup)
    echo "Cleaning up dead repositories..."
    Rscript -e "rforgemirror::rforge_cleanup_repos()"
    ;;
  *)
    echo "Unknown action: $ACTION"
    exit 1
    ;;
esac
echo "Action complete!"
