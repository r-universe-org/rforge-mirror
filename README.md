# Action: mirror r-forge

This action is invoked a few times per day in the [control room](https://github.com/r-universe-org/control-room) to mirror SVN repositories from [r-forge](https://r-forge.r-project.org) to https://github.com/r-forge. We do this to ingest some of these packages in: https://r-forge.r-universe.dev

Note that some svn repositories cannot be mirrored, for example because they are non public, or empty, or they contain files over 100MB which are not allowed on GitHub. However we mostly really care about actively maintained packages (that are on CRAN), which is only a small fraction of them.
