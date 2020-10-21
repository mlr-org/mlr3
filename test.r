task = tsk("spam")
task$row_roles$holdout = 4001:4601
task$row_roles$use = setdiff(task$row_roles$use, task$row_roles$holdout)

learner = lrn("classif.rpart", predict_sets = c("test", "holdout"))

rr = resample(task, learner, rsmp("holdout"))


p = rr$prediction()
rr$prediction("test")$score(m1)
rr$prediction("holdout")$score(m1)

m1 = msr("classif.acc", id = "acc.test", predict_sets = "test")
m2 = msr("classif.acc", id = "acc.holdout", predict_sets = "holdout")
rr$aggregate(list(m1, m2))


rr$data$data$fact$prediction[[1]]$holdout
