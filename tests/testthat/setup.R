old_opts = options(
  warnPartialMatchArgs = TRUE,
  warnPartialMatchAttr = TRUE,
  warnPartialMatchDollar = TRUE,
  mlr3.on_deprecated = "error"
)

# https://github.com/HenrikBengtsson/Wishlist-for-R/issues/88
old_opts[1:3] = lapply(old_opts[1:3], function(x) if (is.null(x)) FALSE else x)

old_threshold = lg$threshold
old_plan = future::plan()
lg$set_threshold(0)
future::plan("sequential")
