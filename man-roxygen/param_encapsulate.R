#' @param encapsulate (`character(1)`)\cr
#'  If not `NA`, enables encapsulation by setting the field
#'  `Learner$encapsulate` to one of the supported values:
#'  `"none"` (disable encapsulation),
#'  `"try"` (captures errors but output is printed to the console and not logged),
#'  `"evaluate"` (execute via \CRANpkg{evaluate}),
#'  `"callr"` (start in external session via \CRANpkg{callr}) and
#'  `"mirai"` (start in a \CRANpkg{mirai} daemon, see [Learner]).
#'  If `NA`, encapsulation is not changed, i.e. the settings of the
#'  individual learner are active.
#'  Additionally, if encapsulation is set to `"try"`, `"evaluate"`, `"callr"`, or `"mirai"`,
#'  the fallback learner is set to the featureless learner if the learner
#'  does not already have a fallback configured.
