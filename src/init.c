#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

extern SEXP C_filter_oob_index(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"C_filter_oob_index", (DL_FUNC) &C_filter_oob_index, 3},
    {NULL, NULL, 0}
};

void R_init_mlr3(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
