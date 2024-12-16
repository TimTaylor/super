#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP glue(SEXP, SEXP);
extern SEXP glue_free(void);
extern SEXP trim(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"glue",      (DL_FUNC) &glue,      2},
    {"glue_free", (DL_FUNC) &glue_free, 0},
    {"trim",      (DL_FUNC) &trim,      1},
    {NULL, NULL, 0}
};

void R_init_super(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}

