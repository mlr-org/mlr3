


task = TaskRegr$new("foo", as_data_backend(cbind(iris, data.frame(w = rep(c(1, 10, 100), each = 50)))), target = "Sepal.Length")
task$set_col_roles("w", character())
learner = lrn("regr.rpart", use_weights = "use")

learner$train(task)
p1 = learner$predict(task)

task$set_col_roles("w", "weights_learner")
learner$train(task)
p2 = learner$predict(task)

expect_lt(p1$score(), p2$score())

