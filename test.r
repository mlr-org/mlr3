tasks = tsks(c("iris", "sonar"))
rearners = list(
  lrn("classif.rpart"),
  lrn("classif.rpart", predict_type = "prob", id = "ptree")
)

grid = benchmark_grid(tasks, learners, rsmp("cv", folds = 3))
bmr = benchmark(grid)
rr = bmr$resample_result(1)
p = rr$prediction()

rr$score(msr("classif.auc"))

m = msr("classif.ce")
m$score(p)

m = msr("classif.auc")
m$score(p)

p$score(m)

m$check_prerequisites = "ignore"
m$score(p)

m = msr("classif.auc")
bmr$score(msrs(c("classif.auc", "classif.ce")))
