#' @useDynLib rforgemirror C_set_ignore_sigpipe
set_ignore_sigpipe <- function(set){
  .Call(C_set_ignore_sigpipe, set)
}
