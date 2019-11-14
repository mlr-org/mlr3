
# mlr3 <img src="man/figures/logo.png" align="right" width = "120" />

Efficient, object-oriented programming on the building blocks of machine
learning. Successor of [mlr](https://github.com/mlr-org/mlr).

[![Build
Status](https://img.shields.io/travis/mlr-org/mlr3/master?label=Linux&logo=travis&style=flat-square)](https://travis-ci.org/mlr-org/mlr3)
[![CircleCI](https://img.shields.io/circleci/build/gh/mlr-org/mlr3/master?label=Linux&logo=circle&logoColor=green&style=flat-square)](https://circleci.com/gh/mlr-org/mlr3)
[![cran
checks](https://cranchecks.info/badges/worst/mlr3)](https://cran.r-project.org/web/checks/check_results_mlr3.html)

[![CRAN Status
Badge](https://www.r-pkg.org/badges/version-ago/mlr3)](https://cran.r-project.org/package=mlr3)
[![codecov](https://codecov.io/gh/mlr-org/mlr3/branch/master/graph/badge.svg)](https://codecov.io/gh/mlr-org/mlr3)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
[![Dependencies](https://tinyverse.netlify.com/badge/mlr3)](https://cran.r-project.org/package=mlr3)

## Resources

  - We *started* writing a [book](https://mlr3book.mlr-org.com/), but it
    is still in early stages.
  - [Reference Manual](https://mlr3.mlr-org.com/reference/)
  - [Extension
    packages](https://github.com/mlr-org/mlr3/wiki/Extension-Packages)
  - [mlr-outreach](https://github.com/mlr-org/mlr-outreach) contains
    talks and slides
  - [Blog](https://mlr-org.com/) about *mlr* and *mlr3*
  - [Wiki](https://github.com/mlr-org/mlr3/wiki)

## Installation

Install the last release from CRAN:

``` r
install.packages("mlr3")
```

Install the development version from GitHub:

``` r
remotes::install_github("mlr-org/mlr3")
```

## Example

### Constructing Learners and Tasks

``` r
library(mlr3)
set.seed(1)

# create learning task
task_iris = TaskClassif$new(id = "iris", backend = iris, target = "Species")
task_iris
```

    ## <TaskClassif:iris> (150 x 5)
    ## * Target: Species
    ## * Properties: multiclass
    ## * Features (4):
    ##   - dbl (4): Petal.Length, Petal.Width, Sepal.Length, Sepal.Width

``` r
# load learner and set hyperparamter
learner = lrn("classif.rpart", cp = 0.01)
```

### Basic train + predict

``` r
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

    ##             truth
    ## response     setosa versicolor virginica
    ##   setosa         11          0         0
    ##   versicolor      0         12         1
    ##   virginica       0          0         6

``` r
measure = msr("classif.acc")
prediction$score(measure)
```

    ## classif.acc 
    ##   0.9666667

### Resample

``` r
# automatic resampling
resampling = rsmp("cv", folds = 3L)
rr = resample(task_iris, learner, resampling)
```

    ## INFO  [23:07:19.534] Applying learner 'classif.rpart' on task 'iris' (iter 1/3) 
    ## INFO  [23:07:19.558] Applying learner 'classif.rpart' on task 'iris' (iter 2/3) 
    ## INFO  [23:07:19.574] Applying learner 'classif.rpart' on task 'iris' (iter 3/3)

``` r
rr$score(measure)
```

    ##             task task_id               learner    learner_id
    ##           <list>  <char>                <list>        <char>
    ## 1: <TaskClassif>    iris <LearnerClassifRpart> classif.rpart
    ## 2: <TaskClassif>    iris <LearnerClassifRpart> classif.rpart
    ## 3: <TaskClassif>    iris <LearnerClassifRpart> classif.rpart
    ##        resampling resampling_id iteration prediction classif.acc
    ##            <list>        <char>     <int>     <list>       <num>
    ## 1: <ResamplingCV>            cv         1     <list>        0.92
    ## 2: <ResamplingCV>            cv         2     <list>        0.92
    ## 3: <ResamplingCV>            cv         3     <list>        0.94

``` r
rr$aggregate(measure)
```

    ## classif.acc 
    ##   0.9266667

## Why a rewrite?

[mlr](https://github.com/mlr-org/mlr) was first released to
[CRAN](https://cran.r-project.org/package=mlr) in 2013. Its core design
and architecture date back even further. The addition of many features
has led to a [feature
creep](https://en.wikipedia.org/wiki/Feature_creep) which makes
[mlr](https://github.com/mlr-org/mlr) hard to maintain and hard to
extend. We also think that while mlr was nicely extensible in some parts
(learners, measures, etc.), other parts were less easy to extend from
the outside. Also, many helpful R libraries did not exist at the time
[mlr](https://github.com/mlr-org/mlr) was created, and their inclusion
would result in non-trivial API changes.

## Design principles

  - Only the basic building blocks for machine learning are implemented
    in this package.
  - Focus on computation here. No visualization or other stuff. That can
    go in extra packages.
  - Overcome the limitations of R’s [S3
    classes](https://adv-r.hadley.nz/s3.html) with the help of
    [R6](https://cran.r-project.org/package=R6).
  - Embrace [R6](https://cran.r-project.org/package=R6) for a clean
    OO-design, object state-changes and reference semantics. This might
    be less “traditional R”, but seems to fit `mlr` nicely.
  - Embrace
    [`data.table`](https://cran.r-project.org/package=data.table) for
    fast and convenient data frame computations.
  - Combine `data.table` and `R6`, for this we will make heavy use of
    list columns in data.tables.
  - Be light on dependencies. `mlr3` requires the following packages at
    runtime:
      - [`backports`](https://cran.r-project.org/package=backports):
        Ensures backward compatibility with older R releases. Developed
        by members of the `mlr` team. No recursive dependencies.
      - [`checkmate`](https://cran.r-project.org/package=checkmate):
        Fast argument checks. Developed by members of the `mlr` team. No
        extra recursive dependencies.
      - [`mlr3misc`](https://cran.r-project.org/package=mlr3misc):
        Miscellaneous functions used in multiple mlr3 [extension
        packages](https://github.com/mlr-org/mlr3/wiki/Extension-Packages).
        Developed by the `mlr` team. No extra recursive dependencies.
      - [`paradox`](https://cran.r-project.org/package=paradox):
        Descriptions for parameters and parameter sets. Developed by the
        `mlr` team. No extra recursive dependencies.
      - [`R6`](https://cran.r-project.org/package=R6): Reference class
        objects. No recursive dependencies.
      - [`data.table`](https://cran.r-project.org/package=data.table):
        Extension of R’s `data.frame`. No recursive dependencies.
      - [`digest`](https://cran.r-project.org/package=digest): Hash
        digests. No recursive dependencies.
      - [`uuid`](https://cran.r-project.org/package=uuid): Create unique
        string identifiers. No recursive dependencies.
      - [`lgr`](https://cran.r-project.org/package=lgr): Logging
        facility. No extra recursive dependencies.
      - [`mlr3measures`](https://cran.r-project.org/package=mlr3measures):
        Performance measures. No extra recursive dependencies.
      - [`mlbench`](https://cran.r-project.org/package=mlbench): A
        collection of machine learning data sets. No dependencies.
  - [Reflections](https://en.wikipedia.org/wiki/Reflection_%28computer_programming%29):
    Objects are queryable for properties and capabilities, allowing you
    to program on them.
  - Additional functionality that comes with extra dependencies:
      - For parallelization, `mlr3` utilizes the
        [`future`](https://cran.r-project.org/package=future) and
        [`future.apply`](https://cran.r-project.org/package=future.apply)
        packages.
      - To capture output, warnings and exceptions,
        [`evaluate`](https://cran.r-project.org/package=evaluate) and
        [`callr`](https://cran.r-project.org/package=callr) can be used.

## Extension Packages

<a href="https://raw.githubusercontent.com/mlr-org/mlr3/master/man/figures/mlr3verse.svg?sanitize=true"><img src="man/figures/mlr3verse.svg" /></a>

Consult the
[wiki](https://github.com/mlr-org/mlr3/wiki/Extension-Packages) for
short descriptions and links to the respective repositories.

## Contributing to mlr3

This R package is licensed under the
[LGPL-3](https://www.gnu.org/licenses/lgpl-3.0.en.html). If you
encounter problems using this software (lack of documentation,
misleading or wrong documentation, unexpected behaviour, bugs, …) or
just want to suggest features, please open an issue in the [issue
tracker](https://github.com/mlr-org/mlr3/issues). Pull requests are
welcome and will be included at the discretion of the maintainers.

Please consult the [wiki](https://github.com/mlr-org/mlr3/wiki/) for a
[style guide](https://github.com/mlr-org/mlr3/wiki/Style-Guide), a
[roxygen guide](https://github.com/mlr-org/mlr3/wiki/Roxygen-Guide) and
a [pull request
guide](https://github.com/mlr-org/mlr3/wiki/PR-Guidelines).
