task = tsk("iris")
learner = lrn("classif.rpart")

learner$train(task)
learner$predict(task, row_ids = 1:3)
