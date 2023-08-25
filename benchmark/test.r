task = tsk("pima")
learner = lrn("classif.rpart")
resampling = rsmp("cv", folds = 3)
resampling$instantiate(task)

theta1 = list(cp = 0.01, minbucket = 10)
theta2 = list(cp = 0.2, minbucket = 6)
thetas_rpart = list(theta1, theta2)

design = data.table::data.table(
    task = list(task),
    learner = list(learner),
    resampling = list(resampling),
    param_value = list(thetas_rpart)
)


bmr = benchmark(design)
as.data.table(bmr)

###########################

design = benchmark_grid(task, learner, resampling)
bmr = benchmark(design)
as.data.table(bmr)

###############

design = benchmark_grid(task, lrns(c("classif.rpart", "classif.debug")), resampling)
bmr = benchmark(design)
as.data.table(bmr)

#######

design = data.table::data.table(
    task = list(task, task),
    learner = lrns(c("classif.rpart", "classif.featureless")),
    resampling = list(resampling, resampling),
    param_value = list(thetas_rpart, list(list(), list()))
)

bmr = benchmark(design)


###########

grid = benchmark_grid(
    tsk("pima"),
    lrn("classif.rpart"),
    rsmp("cv", folds = 3),
    param_values = list(list(list(cp = 0.01, minbucket = 10), list(cp = 0.2, minbucket = 6), list(cp = 0.3, minbucket = 7)))
)

grid = benchmark_grid(
    tsk("pima"),
    lrns(c("classif.rpart", "classif.debug")),
    rsmp("cv", folds = 3),
    param_values = list(
        list(list(cp = 0.01, minbucket = 10), list(cp = 0.2, minbucket = 6), list(cp = 0.3, minbucket = 7)),
        list(list(x = 0.1), list(x = 0.2))
    )
)

bmr = benchmark(grid)
grid = benchmark_grid(
    tsk("pima"),
    lrns(c("classif.rpart", "classif.debug")),
    rsmp("cv", folds = 3),
    param_values = list(
        list(list(cp = 0.01, minbucket = 10), list(cp = 0.2, minbucket = 6), list(cp = 0.3, minbucket = 7)),
        list(list(x = 0.1), list(x = 0.2))
    )
)

bmr = benchmark(grid)

as.data.table(bmr)



###############################

learner = lrn("classif.rpart")
task = tsk("pima")
resampling = rsmp("cv", folds = 3)

tune_old = function(n, learner, task, resampling) {
    learners = replicate(n, learner$clone(deep = TRUE))
    grid = benchmark_grid(task, learners, resampling)
    benchmark(grid)
}

tune_old(3, learner, task, resampling)
