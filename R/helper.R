translate_types = function(x) {
  r_types = mlr_reflections$task_feature_types
  p_types = names(mlr_reflections$task_feature_types)
  factor(map_values(x, r_types, p_types), levels = p_types)
}

get_featureless_learner = function(task_type) {
  if (!is.na(task_type)) {
    id = paste0(task_type, ".featureless")
    if (mlr_learners$has(id)) {
      return(mlr_learners$get(id))
    }
  }

  return(NULL)
}

assert_ordered_set = function(x, y, ...) {
  assert_subset(x, y, ...)
  x[reorder_vector(x, y)]
}

set_data_table_class = function(x, class = NULL) {
  setattr(x, "class", c(class, "data.table", "data.frame"))
}

print_data_table = function(x, hidden_columns) {
  hidden_columns = intersect(names(x), hidden_columns)
  extra_class = class(x)[1L]
  set_data_table_class(x)
  print(x[, .SD, .SDcols = !hidden_columns])
  if (length(hidden_columns)) {
    catf(str_indent("Hidden columns:", hidden_columns))
  }
  set_data_table_class(x, extra_class)
}

clone_without = function(x, y) {
  y_prev = x[[y]]
  x[[y]] = NULL
  x2 = x$clone(deep = TRUE)
  x[[y]] = y_prev
  return(x2)
}

clone_rep = function(x, n) {
  xc = x$clone(deep = TRUE)
  lapply(seq_len(n), function(i) xc)
}

#' @title Assert Validate
#' @description
#' Asserts whether the input is a valid value for the `$validate` field of a [`Learner`].
#' @param x (any)\cr
#'   The input to check.
#' @export
#' @rdname mlr_assertions
assert_validate = function(x) {
  if (test_numeric(x, lower = 0, upper = 1, len = 1L, any.missing = FALSE)) {
    return(x)
  }
  assert_choice(x, c("predefined", "test"), null.ok = TRUE)
}


get_obs_loss = function(tab, measures) {
  for (measure in measures) {
    fun = measure$obs_loss
    value = if (is.function(fun)) {
      args = intersect(names(tab), names(formals(fun)))
      do.call(fun, tab[, args, with = FALSE])
    } else {
      NA_real_
    }

    set(tab, j = measure$id, value = value)
  }

  tab[]
}

# Generalization of quantile(type = 7) for weighted data.

quantile_weighted = function(x, probs, na.rm = FALSE, weights = NULL, digits = 7L, continuous = TRUE) {
  assert_flag(na.rm)
  assert_flag(continuous)
  assert_numeric(x, any.missing = na.rm)
  assert_numeric(probs, lower = -100 * .Machine$double.eps, upper = 1 + 100 * .Machine$double.eps)
  assert_numeric(weights, lower = 0, any.missing = FALSE, len = length(x), null.ok = TRUE)
  weights = weights[!is.na(x)]
  x = x[!is.na(x)]  # if na.rm is FALSE and there are NAs, the assert stops us from getting here

  # we default to the unweighted quantile if there are no weights, no (non-NA) probs, or no (non-NA) x, or all weights are the same
  if (is.null(weights) || length(x) == 0L || length(probs) == 0L || all(is.na(probs)) || length(unique(weights)) == 1L) {
    return(quantile(x, probs, na.rm = na.rm, digits = digits, type = if (continuous) 7 else 2))
  }

  # We create a piecewise linear function with breaks both at the midpoints of the weights, as well as at the boundaries of the weights
  # So if we have weights, ordered by corresponding x-value, w_(1), w_(2), ..., then we have breaks at
  # 0 [weight midpoint], w_(1)/2 [weight boundary], w_(1)/2 + w_(2)/2 [weight midpoint], w_(1)/2 + w_(2) [weight boundary], w_(2)/2 + w_(3)/2 [weight midpoint]
  # The function values at the weight midpoints are the x-values corresponding to the weights, and the function values at the weight boundaries are the
  # weighted average of the x-values of the previous and next weight midpoint.
  x_order = order(x)
  x = x[x_order]
  weights = weights[x_order]
  if (continuous) {
    double_weights = rep(weights / 2, each = 2)[c(-1, -2 * length(weights))] + .Machine$double.xmin  # avoid 0 weights so we don't have to handle division by 0
    double_x = rep(x, each = 2)[c(-1, -2 * length(x))] * double_weights

    double_x_weighted = (c(double_x, double_x[[length(double_x)]]) + c(double_x[[1]], double_x)) /
      (c(double_weights, double_weights[[length(double_weights)]]) + c(double_weights[[1]], double_weights))

    pivots = c(0, cumsum(double_weights))

    weights_total = pivots[[length(pivots)]]
    weight_targets = weights_total * probs
    weight_targets[weight_targets < 0] = 0  # since we allow -100 * .Machine$double.eps probs

    lo_indices = findInterval(weight_targets, pivots)
    hi_indices = lo_indices + 1L
    lo_values = double_x_weighted[lo_indices]
    hi_values = double_x_weighted[hi_indices]
    result = lo_values + (weight_targets - pivots[lo_indices]) * (hi_values - lo_values) / (pivots[hi_indices] - pivots[lo_indices])
    result[hi_indices > length(double_x_weighted)] = double_x_weighted[length(double_x_weighted)]
  } else {
    pivots = c(0, cumsum(weights))
    weight_targets = pivots[[length(pivots)]] * probs
    weight_targets[weight_targets < 0] = 0  # since we allow -100 * .Machine$double.eps probs
    pivots_used = pivots[-length(pivots)]
    hi_indices = findInterval(weight_targets, pivots_used)
    lo_indices = hi_indices - (weight_targets %in% pivots_used[-1])  # use weighted average for ties
    lo_values = x[lo_indices]
    hi_values = x[hi_indices]
    weights = weights + .Machine$double.xmin
    result = (lo_values * weights[lo_indices] + hi_values * (weights[hi_indices])) / (weights[lo_indices] + weights[hi_indices])
  }

  pnames = paste0(formatC(probs * 100, format = "fg", width = 1, digits = digits), "%")
  pnames[is.na(pnames)] = ""
  names(result) = pnames
  result
}

weighted_mean_sd = function(x, weights) {
  if (is.null(weights)) {
    return(list(mean = mean(x), sd = sd(x)))
  }
  weights_sum = sum(weights)
  mean = sum(x * weights) / weights_sum
  sd = sqrt(sum(weights * (x - mean)^2) / (weights_sum - sum(weights ^2) / weights_sum))
  list(mean = mean, sd = sd)
}
