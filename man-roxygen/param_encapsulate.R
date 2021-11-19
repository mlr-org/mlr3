#' @param encapsulate (`character(1)`)\cr
#'  If not `NA`, enables encapsulation by setting the field
#'  `Learner$encapsulate` to one of the supported values:
#'  `"none"` (disable encapsulation),
#'  `"evaluate"` (execute via \CRANpkg{evaluate}) and
#'  `"callr"` (start in external session via \CRANpkg{callr}).
#'  If `NA`, encapsulation is not changed, i.e. the settings of the
#'  individual learner are active.
#'  Additionally, if encapsulation is set to `"evaluate"` or `"callr"`,
#'  the fallback learner is set to the featureless learner if the learner
#'  does not already have a fallback configured.
