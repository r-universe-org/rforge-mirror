#' R-forge mirror tools
#'
#' Mirrors SVN repositories to \url{https://github.com/r-forge}
#'
#' @export
#' @name rforge-mirror
#' @rdname rforge
#' @param this_week by default we only mirror repositories with activity in
#' the past week. Set this to FALSE to mirror all repos we can find.
rforge_mirror <- function(this_week = TRUE){
  user <- gh::gh_whoami()
  if(!isTRUE(user$login %in% c('rforge', 'r-forge')))
    stop("No valid PAT found for r-forge user")
  projects <- rforge_find_projects(this_week = this_week)
  if(isFALSE(this_week)){
    projects <- unique(c(find_all_mirrors(), projects))
  } else {
    cranpkgs <- basename(jsonlite::fromJSON('https://raw.githubusercontent.com/r-universe-org/cran-to-git/master/r-forge.json')$url)
    projects <- unique(c(cranpkgs, projects))
  }
  cat("Found active projects:", projects, "\n")
  projects <- setdiff(projects, skiplist)
  if(!nchar(Sys.getenv('FORCEFRESH')))
    projects <- randomize(projects)
  sapply(projects, mirror_one_project)
}

randomize <- function(x){
  sample(x, length(x))
}

#' @export
#' @rdname rforge
rforge_find_projects <- function(this_week = TRUE){
  url <- 'http://r-forge.r-project.org/top/mostactive.php'
  if(isTRUE(this_week))
    url <- paste0(url, '?type=week')
  projects <- find_projects(url)
  sort(basename(unlist(projects)))
}

find_all_mirrors <- function(){
  res <- gh::gh('/orgs/r-forge/repos', .limit = 10000)
  pushed <- vapply(res, '[[', character(1), 'pushed_at')
  names <- vapply(res, '[[', character(1), 'name')
  names[order(pushed)] # Sort from last mirrored
}

#' @export
#' @rdname rforge
#' @name project name of the project (usually package)
rforge_get_revision <- function(project){
  url <- paste0('svn://svn.r-forge.r-project.org/svnroot/', project)
  out <- sys::exec_internal('svn', c('info', url), error = FALSE)
  if(out$status > 0){
    stderr <- rawToChar(out$stderr)
    # weird error that indicates a private repo, treat same as empty
    if(grepl("Permission denied", stderr, fixed = TRUE) && grepl('/var/lib/gforge/chroot', stderr, fixed = TRUE)){
      message("Permission denied for r-forge repo: ", project)
      return("0")
    }
    if(grepl("No repository found", stderr, fixed = TRUE)){
      message("Project deleted from r-forge: ", project)
      return("0")
    }
    stop("Error cloning: ", project, ": ", stderr)
  }
  con <- rawConnection(out$stdout)
  on.exit(close(con))
  df <- as.data.frame(read.dcf(con))
  df$Revision
}

#NB: no longer need this, now that we have 'svn info' above
rforge_get_revision_fallback <- function(project){
  url <- sprintf('https://r-forge.r-project.org/scm/viewvc.php?root=%s&view=rev', project)
  doc <- xml2::read_html(url)
  node <- xml2::xml_find_all(doc, "//h1[starts-with(.,'Revision')]")
  if(length(node) != 1)
    stop("Failed to find H1 header with Revision number")
  sub("Revision ", "", xml_text(node))
}

project_need_update <- function(project){
  rev <- rforge_get_revision(project)
  cat(sprintf("SVN revision for '%s' is %s\n", project, rev))
  if(as.character(rev) == "0"){
    #try(delete_mirror(project))
    stop("This project seems unavailable or empty: ", project)
  }
  if(identical(Sys.getenv('FORCEFRESH'), 'true')){
    return(TRUE)
  }
  endpoint <- sprintf('/repos/r-forge/%s/commits/HEAD', project)
  message <- tryCatch(gh::gh(endpoint)$commit$message, error = function(e){""})
  svn_id <- utils::tail(strsplit(message, '\n')[[1]], 1)
  cat(sprintf("GitHub '%s' %s\n", project, svn_id))
  pattern <- paste0(project, '@', rev)
  !isTRUE(grepl(pattern, message))
}

find_projects <- function(page){
  main <- xml2::read_html(page)
  links <- rvest::html_nodes(main, 'a[href^="https://r-forge.r-project.org/projects/"]')
  cat(sprintf("Found %d projects in %s\n", length(links), basename(page)))
  rvest::html_attr(links, "href")
}

mirror_one_project <- function(project){
  tryCatch({
    needs_update <- project_need_update(project) #will error in case of empty
    create_gh_repo(project)
    if(needs_update){
      cat(sprintf("Starting full mirror for: %s\n", project))
      clone_and_push(project)
      'DONE'
    } else {
      cat(sprintf("Mirror is up-to-date: %s\n", project))
      'SKIPPED'
    }
  }, error = function(e){paste("ERROR:", e$message)})
}

create_gh_repo <- function(project){
  endpoint <- paste0('/repos/r-forge/', project)
  tryCatch({
    res <- gh::gh(endpoint)
    cat(sprintf('Found existing repo "%s"\n', res$full_name))
  }, http_error_404 = function(e){
    cat(sprintf('Did not find repo "%s". Creating new repo...\n', project))
    res <- gh::gh('/orgs/r-forge/repos', .method = 'POST',
           name = project,
           has_issues = FALSE,
           has_wiki = FALSE,
           has_downloads = FALSE,
           has_projects = FALSE,
           homepage = paste0("https://r-forge.r-project.org/projects/", project),
           description = sprintf("Read-only mirror of \"%s\" from r-forge SVN.", project))
    cat(sprintf('Created new repo "%s"\n', res$full_name))
  })
}

find_author_prog <- function(user){
  userdbfile <- '~/authors.rds'
  userdb <- if(file.exists(userdbfile)){
    readRDS(userdbfile)
  } else {
    list()
  }
  if(!length(userdb[[user]])){
    message("Scraping user details for: ", user)
    realname <- user
    url <- sprintf('https://r-forge.r-project.org/users/%s/', curl::curl_escape(user))
    req <- curl::curl_fetch_memory(url)
    if(req$status_code >= 400){
      message("User does not exist on r-forge: ", user)
      userdb[[user]] <- paste(user, "<unknown@noreply.com>")
      return(userdb[[user]])
    }
    doc <- xml2::read_html(req$content)
    namenode <- xml2::xml_find_first(doc, "//span[@property='foaf:name'][1]")
    if(length(namenode) > 0){
      realname <- sprintf('%s (%s)', realname, xml2::xml_text(namenode))
    } else {
      message("Failed to find real name for r-forge user: ", user)
    }
    mailnode <- xml2::xml_find_first(doc, "//a[contains(@href,'sendmessage.php')][1]")
    nodetext <- xml2::xml_text(mailnode)
    email <- if(length(mailnode) > 0 && grepl('nospam', nodetext)){
      sub('\\s*@nospam@\\s*', '@', nodetext)
    } else {
      message("Failed to find email address for r-forge user: ", user)
      "unknown@noreply.com"
    }
    userdb[[user]] <- sprintf('%s <%s>', realname, email)
    saveRDS(userdb, userdbfile)
  } else {
    message("Found existing user details for: ", user)
  }
  return(userdb[[user]])
}


clone_and_push <- function(project){
  old_dir <- getwd()
  git_dir <- file.path(tempdir(), project)
  on.exit({
    setwd(old_dir)
    unlink(git_dir, recursive = TRUE)
  })
  findauthor <- normalizePath(system.file('findauthor.sh', package = 'rforgemirror'), mustWork = TRUE)
  res <- system2("git", c("svn", "clone", "--log-window-size=10000", paste0('--authors-prog=', findauthor),
    sprintf("svn://svn.r-forge.r-project.org/svnroot/%s", project), git_dir))
  if(res != 0)
    stop(paste("git svn clone failed for:", project))
  setwd(git_dir)
  gert::git_remote_add(sprintf('https://rforge:%s@github.com/r-forge/%s', Sys.getenv('GITHUB_PAT'), project))
  if(system("git push --force origin master") == 0){
    message("Successfully mirrored: ", project)
  } else {
    message("Failure pushing: ", project, '. Trying to remove big files with BFG...')
    system('git gc')
    system(paste('java -jar /bfg.jar --strip-blobs-bigger-than 100M', git_dir))
    system('git reflog expire --expire=now --all && git gc --prune=now --aggressive')
    if(system("git push --force origin master") != 0){
      message('Failed to push: ', project)
    }
  }
  # This seems to give strange "ignoring sigpipe" erros in R for certain large files?
  #gert::git_push('origin', force = TRUE, mirror = TRUE)
}

#' @rdname rforge
#' @export
rforge_list_repos <- function(){
  repos <- gh::gh('/users/r-forge/repos', .limit = 1e5)
  vapply(repos, function(x){x$name}, character(1))
}

#' @rdname rforge
#' @export
rforge_cleanup_repos <- function(){
  repos <- rforge_list_repos()
  status <- rforge_project_status(repos)
  dead <- repos[which(status == 404)]
  cat("Found dead repositories:", dead, "\n")
  skipped <- intersect(repos, skiplist)
  todelete <- c(dead, skipped)
  if(length(todelete) > 5){
    stop("More than 5 repos seem to be deleted. Something may be wrong.")
  }
  # 2023 skip deleting for now because r-forge seems to be missing things
  #lapply(todelete, delete_mirror)
}

delete_mirror <- function(project){
  endpoint <- paste0("/repos/r-forge/", project)
  gh::gh(endpoint, .method = 'DELETE')
  cat("DELETED", project, "\n")
}

rforge_project_status <- function(projects){
  out <- rep(0, length(projects))
  pool <- curl::new_pool()
  lapply(seq_along(projects), function(i){
    name <- projects[i]
    url <- paste0("https://r-forge.r-project.org/projects/", name)
    curl::curl_fetch_multi(url, done = function(res){
      out[i] <<- res$status_code
      cat(sprintf("Project '%s' is %d\n", name, res$status_code), file = stderr())
    }, pool = pool)
  })
  curl::multi_run(pool = pool)
  structure(out, names = projects)
}

# These fail for whatever reason.
# Usually either disabled on r-forge or has files bigger than 100MB
skiplist <-
  c("adrminer", "arules", "bayessdeevol", "biocep-distrib",
    "dtrees2", "dtw", "estimators4nfi",
    "fpt", "fxregime", "gamboostlss",
    "hmmr", "ipeglim",
    "jmr", "laicpms", "metasem", "mpdir",
    "multivarseg", "nem", "oem",
    "open-tfdea", "optbiomarker", "patchwork", "pmoments", "polyploid",
    "ptauxpc", "rftestproject12",
    "teatime", "toxcast", "tuner",
    "a2a", "dptee", "fntd", "gecon")
