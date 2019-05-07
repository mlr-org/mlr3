# mlr3

A clean, object-oriented rewrite of [mlr](https://github.com/mlr-org/mlr).

[![Build Status](https://travis-ci.org/mlr-org/mlr3.svg?branch=master)](https://travis-ci.org/mlr-org/mlr3)
[![Build status](https://ci.appveyor.com/api/projects/status/m2tuhgdxo8is0nv0?svg=true)](https://ci.appveyor.com/project/mlr-org/mlr3)
[![CRAN](https://www.r-pkg.org/badges/version/mlr3)](https://cran.r-project.org/package=mlr3)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![codecov](https://codecov.io/gh/mlr-org/mlr3/branch/master/graph/badge.svg)](https://codecov.io/gh/mlr-org/mlr3)

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
    - [`logger`](https://github.com/daroczig/logger): Logging facility. No recursive dependencies.
    - [`Metrics`](https://cran.r-project.org/package=Metrics): Package which implements performance measures. No recursive dependencies.
    - [`mlbench`](https://cran.r-project.org/package=mlbench): A collection of machine learning data sets. No dependencies.

* Additional functionality that comes with extra dependencies:
    - For parallelization, `mlr3` utilizes the [`future`](https://cran.r-project.org/package=future) and [`future.apply`](https://cran.r-project.org/package=future.apply) packages.
    - To capture output, warnings and exceptions, [`evaluate`](https://cran.r-project.org/package=evaluate) and [`callr`](https://cran.r-project.org/package=callr) can be used.


## State of the project

**This package is currently work-in-progress. Do not use in production. The API will change.**

### Already implemented:

* Basic building blocks of machine learning
    - [Experiment](https://mlr3.mlr-org.com/reference/Experiment.html): Class to store and access a single machine learning experiment
    - Tasks for [classification](https://mlr3.mlr-org.com/reference/TaskClassif.html) and [regression](https://mlr3.mlr-org.com/reference/TaskRegr.html)
    - Abstract [Learner](https://mlr3.mlr-org.com/reference/Learner.html)
    - [Resampling](https://mlr3.mlr-org.com/reference/Resampling.html) strategies
    - Performance [measures](https://mlr3.mlr-org.com/reference/Measure.html)
* Task [Generators](https://mlr3.mlr-org.com/reference/Generator.html)
* Single step experiments via the `Experiment` class
* Resampling via [`resample()`](https://mlr3.mlr-org.com/reference/resample.html)
* Stratification and blocking / grouping.
* Benchmarking via [`benchmark()`](https://mlr3.mlr-org.com/reference/benchmark.html)
* Parallelization via [`future`](https://cran.r-project.org/package=future).

The [webpage](https://mlr3.mlr-org.com) provides, besides a short introduction, a [function reference](https://mlr3.mlr-org.com/reference/).

### WiP

While `mlr3` implements the building blocks for machine learning, some of the advanced features of the monolithic  [mlr](https://github.com/mlr-org/mlr) are now shipped in multiple [extension packages](https://github.com/mlr-org/mlr3/wiki/Extension-Packages).
