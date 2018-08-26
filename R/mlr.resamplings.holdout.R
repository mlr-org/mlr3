#' @include Resampling.R
#' @include mlr.resamplings.subsampling.R
local({
  rr = ResamplingSubsampling$new()
  rr$id = "holdout"
  rr$repeats = 1L
  mlr.resamplings$add(rr)
})
