#define R_INTERFACE_PTRS
#include <Rinterface.h>
#include <Rembedded.h>
#include <Rconfig.h>

extern int R_ignore_SIGPIPE;

SEXP C_set_ignore_sigpipe(SEXP set){
  R_ignore_SIGPIPE = Rf_asLogical(set);
  return Rf_ScalarLogical(R_ignore_SIGPIPE);
}
