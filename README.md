# mlr3

Efficient, object-oriented programming on the building blocks of machine learning.
Successor of [mlr](https://github.com/mlr-org/mlr).

[![Build Status](https://travis-ci.org/mlr-org/mlr3.svg?branch=master)](https://travis-ci.org/mlr-org/mlr3)
[![Build status](https://ci.appveyor.com/api/projects/status/m2tuhgdxo8is0nv0?svg=true)](https://ci.appveyor.com/project/mlr-org/mlr3)
[![CRAN](https://www.r-pkg.org/badges/version/mlr3)](https://cran.r-project.org/package=mlr3)
[![codecov](https://codecov.io/gh/mlr-org/mlr3/branch/master/graph/badge.svg)](https://codecov.io/gh/mlr-org/mlr3)

## Resources

* We started to write a [book](https://mlr3book.mlr-org.com/), but it is still very unfinished.
* [Reference Manual](https://mlr3.mlr-org.com/reference/)
* [Extension packages](https://github.com/mlr-org/mlr3/wiki/Extension-Packages)
* [useR2019 talks](https://github.com/mlr-org/mlr-outreach/tree/master/2019_useR)

## Installation


```r
remotes::install_github("mlr-org/mlr3")
```

## Example


```r
library(mlr3)
set.seed(1)

# create learning task
task_iris = TaskClassif$new(id = "cars", backend = iris, target = "Species")
task_iris
```

```
## <TaskClassif:cars> (150 x 5)
## Target: Species
## Properties: multiclass
## Features (4):
## * dbl (4): Petal.Length, Petal.Width, Sepal.Length, Sepal.Width
```

```r
# load learner
learner = mlr_learners$get("classif.rpart")

# set hyperparameter
learner$param_set$values = list(cp = 0.01)

# train/test split
train_set = sample(task_iris$nrow, 0.8 * task_iris$nrow)
test_set = setdiff(seq_len(task_iris$nrow), train_set)

# train the model
learner$train(task_iris, row_ids = train_set)

# predict data
prediction = learner$predict(task_iris, row_ids = test_set)

# calculate performance
prediction$confusion
```

```
##             truth
## response     setosa versicolor virginica
##   setosa         11          0         0
##   versicolor      0         12         1
##   virginica       0          0         6
```

```r
measure = mlr_measures$get("classif.acc")
prediction$score(measure)
```

```
## classif.acc 
##   0.9666667
```

```r
# automatic resampling
resampling = mlr_resamplings$get("cv", param_vals = list(folds = 3L))
rr = resample(task_iris, learner, resampling)
```

```
## INFO  [20:43:19.151] Applying learner 'classif.rpart' on task 'cars' (iter 1/3) 
## INFO  [20:43:19.216] Applying learner 'classif.rpart' on task 'cars' (iter 2/3) 
## INFO  [20:43:19.239] Applying learner 'classif.rpart' on task 'cars' (iter 3/3)
```

```r
rr$performance(measure)
```

```
##             task task_id               learner    learner_id
##           <list>  <char>                <list>        <char>
## 1: <TaskClassif>    cars <LearnerClassifRpart> classif.rpart
## 2: <TaskClassif>    cars <LearnerClassifRpart> classif.rpart
## 3: <TaskClassif>    cars <LearnerClassifRpart> classif.rpart
##        resampling resampling_id iteration          prediction classif.acc
##            <list>        <char>     <int>              <list>       <num>
## 1: <ResamplingCV>            cv         1 <PredictionClassif>        0.94
## 2: <ResamplingCV>            cv         2 <PredictionClassif>        0.98
## 3: <ResamplingCV>            cv         3 <PredictionClassif>        0.92
```

```r
rr$aggregate()
```

```
## classif.ce 
## 0.05333333
```

## Why a rewrite?

[mlr](https://github.com/mlr-org/mlr) was first released to [CRAN](https://cran.r-project.org/package=mlr) in 2013.
Its core design and architecture date back even further.
The addition of many features has led to a [feature creep](https://en.wikipedia.org/wiki/Feature_creep) which makes [mlr](https://github.com/mlr-org/mlr) hard to maintain and hard to extend.
We also think that while mlr was nicely extensible in some parts (learners, measures, etc.), other parts were less easy to extend from the outside.
Also, many helpful R libraries did not exist at the time [mlr](https://github.com/mlr-org/mlr) was created, and their inclusion would result in non-trivial API changes.


## Design principles

* Only the basic building blocks for machine learning are implemented in this package.
* Focus on computation here. No visualization or other stuff. That can go in extra packages.
* Overcome the limitations of R's [S3 classes](https://adv-r.hadley.nz/s3.html) with the help of [R6](https://cran.r-project.org/package=R6).
* Embrace [R6](https://cran.r-project.org/package=R6), clean OO-design, object state-changes and reference semantics. This might be less "traditional R", but seems to fit `mlr` nicely.
* Embrace [`data.table`](https://cran.r-project.org/package=data.table) for fast and convenient data frame computations.
* Combine `data.table` and `R6`, for this we will make heavy use of list columns in data.tables.
* Be light on dependencies. `mlr3` requires the following packages:
    - [`backports`](https://cran.r-project.org/package=backports): Ensures backward compatibility with older R releases. Developed by members of the `mlr` team. No recursive dependencies.
    - [`checkmate`](https://cran.r-project.org/package=checkmate): Fast argument checks. Developed by members of the `mlr` team. No extra recursive dependencies.
    - [`mlr3misc`](https://github.com/mlr-org/mlr3misc) Miscellaneous functions used in multiple mlr3 [extension packages](https://github.com/mlr-org/mlr3/wiki/Extension-Packages). Developed by the `mlr` team. No extra recursive dependencies.
    - [`paradox`](https://github.com/mlr-org/paradox): Descriptions for parameters and parameter sets. Developed by the `mlr` team. No extra recursive dependencies.
    - [`R6`](https://cran.r-project.org/package=R6): Reference class objects. No recursive dependencies.
    - [`data.table`](https://cran.r-project.org/package=data.table): Extension of R's `data.frame`. No recursive dependencies.
    - [`digest`](https://cran.r-project.org/package=digest): Hash digests. No recursive dependencies.
    - [`lgr`](https://github.com/s-fleck/lgr): Logging facility. No extra recursive dependencies.
    - [`Metrics`](https://cran.r-project.org/package=Metrics): Package which implements performance measures. No recursive dependencies.
    - [`mlbench`](https://cran.r-project.org/package=mlbench): A collection of machine learning data sets. No dependencies.
* [Reflections](https://en.wikipedia.org/wiki/Reflection_%28computer_programming%29): Objects are queryable for properties and capabilities, allowing you to programm on them.
* Additional functionality that comes with extra dependencies:
    - For parallelization, `mlr3` utilizes the [`future`](https://cran.r-project.org/package=future) and [`future.apply`](https://cran.r-project.org/package=future.apply) packages.
    - To capture output, warnings and exceptions, [`evaluate`](https://cran.r-project.org/package=evaluate) and [`callr`](https://cran.r-project.org/package=callr) can be used.
