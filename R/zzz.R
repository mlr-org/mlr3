#' @import data.table
#' @import checkmate
#' @import paradox
#' @import mlr3misc
#' @importFrom R6 R6Class is.R6
#' @importFrom utils data head tail
#' @importFrom uuid UUIDgenerate
#' @section Additional resources:
#' * Book on mlr3: \url{https://mlr3book.mlr-org.com}
#' * Use cases and examples: \url{https://mlr3gallery.mlr-org.com}
#' * More classification and regression learners: \CRANpkg{mlr3learners}
#' * Preprocessing and machine learning pipelines: \CRANpkg{mlr3pipelines}
#' * Tuning of hyperparameters: \CRANpkg{mlr3tuning}
#' * Visualizations for many \pkg{mlr3} objects: \CRANpkg{mlr3viz}
#' * Survival analysis and probabilistic regression: \CRANpkg{mlr3proba}
#' * Feature selection filters: \CRANpkg{mlr3filters}
#' * Interface to real (out-of-memory) data bases: \CRANpkg{mlr3db}
#' * Performance measures as plain functions: \CRANpkg{mlr3measures}
#' * Parallelization framework: \CRANpkg{future}
#' * Progress bars: \CRANpkg{progressr}
#' @references
#' \cite{mlr3}{pkg::citation}
"_PACKAGE"

dummy_import = function() {
  # nocov start
  # this function is required to silence R CMD check
  mlbench::mlbench.xor
  mlr3measures::mse
} # nocov end


.onLoad = function(libname, pkgname) {
  # nocov start
  backports::import(pkgname)

  # setup logger
  assign("lg", lgr::get_logger(pkgname), envir = parent.env(environment()))
  if (Sys.getenv("IN_PKGDOWN") == "true") {
    lg$set_threshold("warn")
  }
} # nocov end


leanificate_method <- function(cls, assignwhich, name, env = parent.frame()) {
  assert_choice(assignwhich, c("public_methods", "private_methods", "active"))
  which <- switch(assignwhich, public_methods = "public", private_methods = "private", active = "active")
  cname <- cls$classname
  exportfname <- sprintf(".__%s__%s", cname, name)
  fn <- cls[[assignwhich]][[name]]
  origformals <- formals(fn)
  formals(fn) <- c(pairlist(self = substitute(), private = substitute(), super = substitute()), formals(fn))
  assign(exportfname, fn, env)
  replacingfn <- eval(call("function", origformals,
    as.call(c(list(as.symbol(exportfname)), sapply(names(formals(fn)), as.symbol, simplify = FALSE)))))
  environment(replacingfn) <- environment(fn)
  cls$set(which, name, replacingfn, overwrite = TRUE)
}

leanify_r6 <- function(cls, env = parent.frame()) {
  for (assignwhich in c("public_methods", "private_methods", "active")) {
    for (fname in names(cls[[assignwhich]])) {
      leanificate_method(cls, assignwhich, fname, env = env)
    }
  }
}

for (varname in ls()) {
  content <- get(varname)
  if (R6::is.R6Class(content)) {
    leanify_r6(content)
  }
}
