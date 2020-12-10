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
  cat("Found active projects:", projects, "\n")
  projects <- setdiff(projects, skiplist)
  sapply(projects, mirror_one_project)
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

#' @export
#' @rdname rforge
#' @name project name of the project (usually package)
rforge_get_revision <- function(project){
  out <- sys::exec_internal('svn', c('info', paste0('svn://svn.r-forge.r-project.org/svnroot/', project)))
  df <- as.data.frame(read.dcf(rawConnection(out$stdout)))
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
    create_gh_repo(project)
    if(project_need_update(project))
      clone_and_push(project)
    'OK'
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

clone_and_push <- function(project){
  old_dir <- getwd()
  git_dir <- file.path(tempdir(), project)
  on.exit({
    setwd(old_dir)
    unlink(git_dir, recursive = TRUE)
  })
  res <- system2("git", c("svn", "clone", "--log-window-size=10000",
    sprintf("svn://svn.r-forge.r-project.org/svnroot/%s", project), git_dir))
  if(res != 0)
    stop(paste("git svn clone failed for:", project))
  setwd(git_dir)
  gert::git_remote_add(paste0('https://github.com/r-forge/', project))
  gert::git_push('origin', force = TRUE, mirror = TRUE)
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
  lapply(dead, function(project){
    endpoint <- paste0("/repos/r-forge/", project)
    gh::gh(endpoint, .method = 'DELETE')
    cat("DELETED", project, "\n")
  })
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
  c("adrminer", "arules", "bayessdeevol", "bbmm-analysis", "biocep-distrib",
    "casper", "dcgor", "dnet", "dtrees2", "dtw", "estimators4nfi",
    "flr", "fpt", "fxregime", "gamboostlss", "genabel", "gsdesign",
    "gsif", "gsmoothr", "gwidgets", "hmmr", "htda", "ihelp", "ipeglim",
    "jmr", "laicpms", "metasem", "mmsa", "morgan-rtools", "mpdir",
    "msgl", "multivarseg", "nem", "neuroim", "oem", "omearalab",
    "open-tfdea", "optbiomarker", "patchwork", "pmoments", "polyploid",
    "ptauxpc", "rftestproject12", "r-survey", "seqinr", "sprint",
    "teatime", "toxcast", "trajectory-sim", "treevo", "tuner", "waveslim",
    "wavetiling", "hyperspec")
