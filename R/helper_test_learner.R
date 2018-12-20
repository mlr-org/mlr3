# Helper function which generates test data.
# args:
# feature_types: character(). Must be subset of mlr_reflections$task_feature_types
# target: character(1). "twoclass", "multiclass" or "regr"
# missings: logical(1). If TRUE, all feature columns will contain missings
make_data = function(feature_types, target, missings = FALSE) {
  assert_subset(target, c("twoclass", "multiclass", "regr"))
  assert_true(length(target) == 1)
  feat_logical = rep(c(TRUE, FALSE), each = 10)
  feat_integer = rep(c(1L, 0L), each = 10)
  feat_numeric = rep(c(1.1, 0.2), each = 10)
  feat_character = rep(c("one", "zero"), each = 10)
  feat_factor = as.factor(rep(c("one", "zero"), each = 10))
  feat_ordered = factor(rep(c("one", "two", "three", "four"), each = 5), ordered = TRUE,
    levels = c("one", "two", "three", "four"))

  target_twoclass = rep(c("M", "R"), each = 10)
  target_multiclass = rep(c("M", "R", "X", "Z"), each = 5)
  target_regr = rep(c(1, 2), each = 10)
  features = data.frame(logical = feat_logical, integer = feat_integer, numeric = feat_numeric,
    character = feat_character, factor = feat_factor,
    ordered = feat_ordered,
    stringsAsFactors = FALSE)
  targets = data.frame(twoclass = target_twoclass, multiclass = target_multiclass, regr = target_regr)
  if (missings)
    for (i in 1:ncol(features)) features[i, i] = NA
  df = data.frame(features, targets)
  df[, c(feature_types, target)]
}

# Helper function which generates classification tasks for every feature type combination.
# E.g.:
# combinations length 1: one logical feature, one integer feature, one numeric feature ...
# combinations length 2: logical + integer features, logical + numeric features, ....
# ...
# combinations length length(lrn$feature_types): ...
#
# missings will be TRUE, if learner supports missings.
# Only multiclass will be tested, if multiclass is supported (twoclass otherwise).
# args:
# lrn: learner
# target: character(1). "twoclass", "multiclass" or "regr"
# missings: logical(1). If TRUE, all feature columns will contain missings
make_classif_test_tasks = function(lrn) {
  combn = NULL
  feature_types = lrn$feature_types
  target = ifelse("multiclass" %in% lrn$properties, "multiclass", "twoclass")
  missings = "missings" %in% lrn$properties

  combs = lapply(1:length(feature_types), combn, x = feature_types, simplify = FALSE) #get all possible feature type combinations
  tasks = list()
  #loop over number of feature types
  for (i in 1:length(combs)) {
    combs_i = combs[[i]]
    comb_names = lapply(combs_i, function(x) paste0(unlist(x), collapse = "."))
    df_list = lapply(combs_i, function(x) make_data(feature_types = x, target = target, missings))
    data_backend_list = lapply(df_list, as_data_backend)
    tasks_list = mapply(function(x,y) TaskClassif$new(id = x, backend = y, target = target), x = comb_names, y = data_backend_list)
    tasks[[i]] = tasks_list
  }
  tasks
}

# basic unit tests for train and predict
# args:
# e: experiment from Experiment$new(task, learner)
test_train_predict = function(e, id) {
  e$train()
  expect_false(e$has_errors, info = id)
  e$predict()
  expect_class(e$prediction, "Prediction", info = id)
  e$score()
  expect_number(e$performance, info = id)
}
