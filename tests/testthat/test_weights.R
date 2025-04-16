


task = TaskRegr$new("foo", as_data_backend(cbind(iris, data.frame(w = rep(c(1, 10, 100), each = 50)))), target = "Sepal.Length")
task$set_col_roles("w", character())
learner = lrn("regr.rpart", use_weights = "use")

learner$train(task)
p1 = learner$predict(task)

task$set_col_roles("w", "weights_learner")
learner$train(task)
p2 = learner$predict(task)

expect_lt(p1$score(), p2$score())


# does score_measure work the way it seems?

# - assert_learnable: triggers error when 'use_weights' is "error"
# - assert_measure: triggers error when 'use_weights' is "error"
# - learner autotest should check that .get_weights() is called -- even when use_weights is "ignore"
# - test quantile_weighted()
# - where does resample result store its weights?
#   - should go into prediction
# - measureclassifcosts with weights
# - weighted_confusion()
# - all the tests with predictions should also be done with weighted tasks
# - error messages when prediction weights are present