skim = function(x) {
  UseMethod("skim")
}

skim.numeric = function(x, ...) {
  as.list(c(mean = mean(x), sd = sd(x), setNames(quantile(x, c(0, 0.25, 0.5, 0.75, 1)), c("p0", "p25", "p50", "p75", "p100"))))
}

skim.character = function(x) {
  skim.factor(x)
}

skim.factor = function(x) {
  tab = table(x)
  names(tab) = substr(names(tab), 1L, 3L)
  imin = which.min(tab)
  imax = which.max(tab)
  list(
    lvls = length(tab),
    unique = length(tab[tab > 0L]),
    min.n = tab[imin],
    min.lvl = names(tab)[imin],
    max.n = tab[imax],
    max.lvl = names(tab)[imax],
    "min/max" = tab[imin] / tab[imax]
  )

}

skim.data.frame = function(x) {
  groups = lapply(x, class)
  res = named_list(unique(groups))
  for (grp in names(res)) {
    res[[grp]] = rbindlist(
      lapply(x[grp == groups], skim)
    )
  }
  res
}


if (FALSE) {
  skim(iris)
  skim(iris[1:10, ])
}
