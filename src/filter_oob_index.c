#include <R.h>
#include <Rinternals.h>

#ifndef INTEGER_RO
#define INTEGER_RO(x) ((const int *) INTEGER(x))
#endif

SEXP C_filter_oob_index(SEXP x_, SEXP min_, SEXP max_) {
    const R_xlen_t min = INTEGER_RO(min_)[0];
    const R_xlen_t max = INTEGER_RO(max_)[0];
    const R_xlen_t n = xlength(x_);
    const int const * xi = INTEGER_RO(x_);
    const int const * xe = xi + n;

    SEXP y_ = PROTECT(allocVector(INTSXP, n));
    int * y = INTEGER(y_);
    R_xlen_t j = 0;

    for (; xi != xe; xi++) {
        if (*xi >= min && *xi <= max) {
            y[j] = *xi;
            j++;
        }
    }

    if (j < n)
        SETLENGTH(y_, j);

    UNPROTECT(1);
    return y_;
}
