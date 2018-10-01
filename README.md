# mlr3

[![Build Status](https://travis-ci.org/mlr-org/mlr3.svg?branch=master)](https://travis-ci.org/mlr-org/mlr3)
[![Build status](https://ci.appveyor.com/api/projects/status/00pdxs8n457tsfd1/branch/master?svg=true)](https://ci.appveyor.com/project/mllg/mlr3/branch/master)
<a href="https://codecov.io/gh/mlr-org/mlr3"><img src="https://codecov.io/gh/mlr-org/mlr3/branch/master/graph/badge.svg" alt="Coverage Status"/></a>

A clean, object-oriented rewrite of [mlr](https://github.com/mlr-org/mlr).

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
* Once the API is fixed, both advanced techniques and implementations for different learners will be implemented in extra packages to reduce the maintenance burden.
* Be light on dependencies. `mlr3` only requires the following packages:
    - [`R6`](https://cran.r-project.org/package=R6): Reference class objects. No extra reverse dependencies.
    - [`backports`](https://cran.r-project.org/package=backports): Ensures backward compatibility with older R releases. Developed by members of the `mlr` team. No extra reverse dependencies.
    - [`bit`](https://cran.r-project.org/package=bit): Efficient storage of logical vectors. No extra reverse dependencies.
    - [`checkmate`](https://cran.r-project.org/package=checkmate): Efficient storage of logical vectors. Developed by members of the `mlr` team. No extra reverse dependencies.
    - [`data.table`](https://cran.r-project.org/package=data.table): Extension of R's `data.frame`. No extra reverse dependencies.
    - [`digest`](https://cran.r-project.org/package=digest): Hash digests. No extra reverse dependencies.
    - [`measures`](https://cran.r-project.org/package=measures): Package which implements performance measures. Developed by members of the `mlr` team. No extra reverse dependencies.
    - [`paradox`](https://cran.r-project.org/package=paradox): Descriptions for parameters and parameter sets. Developed by the `mlr` team. No extra reverse dependencies. [TODO: Remove `BBmisc` dependency]

  For parallelization, `mlr3` optionally uses the [`future`](https://cran.r-project.org/package=future) and [`future.apply`](https://cran.r-project.org/package=future.apply) packages.



## State of the project

**This package is currently work-in-progress. Do not use in production. The API will change.**


### Already implemented:

* Basic building blocks of machine learning
    - Experiment: Class to store and access a single machine learning experiment
    - Tasks for classification and regression
    - Learner
    - Resampling strategies
    - Performance measures
* Data backend for tasks. This allows to transparently work on out-of-memory data like data bases.
  Prototype backend for `dbplyr` data can be found in [mlr3db](https://github.com/mlr-org/mlr3db) (outdated).
* Single step experiments via the `Experiment` class
* Resampling via `resample()`
* Benchmarking via `benchmark()`

Some objects are documented [here](https://mlr-org.github.io/mlr3/).


### WiP

* Tuning: [@jakob-r](https://github.com/jakob-r)
* Pipelining: [@berndbischl](https://github.com/berndbischl)


### Next steps

* Write a package which interfaces the most popular learners in R
* Write documentation and use cases
