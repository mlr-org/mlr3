---
title: 'mlr3: Machine Learning with R - next generation'
tags:
  - R
  - machine learning
  - classification
  - regression
authors:
  - name: Michel Lang
    orcid: 0000-0001-9754-0393
    affiliation: "1, 2"
  - name: Bernd Bischl
    orcid: 0000-0001-6002-6980
    affiliation: 2
  - name: Martin Binder
    orcid: ???
    affiliation: 2
  - name: Jakob Richter
    orcid: 0000-0003-4481-5554
    affiliation: 1
  - name: Patrick Schratz
    orcid: 0000-0003-0748-6624
    affiliation: 2
affiliations:
 - name: TU Dortmund University
   index: 1
 - name: LMU Munich
   index: 2
date: 1 December 2019
bibliography: paper.bib
---

# Summary

The [`R`](https://www.r-project.org/) [@R] package [`mlr3`](https://mlr3.mlr-org.com) package and ecosystem provides a generic, object-oriented, and extensible framework for classification, regression and other machine learning tasks.
It does not implement any learners itself, but provides a unified interface to many already existing learners in `R`.
This unified interface allows to provide functionality to extend and combine existing learners, intelligently select and tune the most appropriate technique for a task, and perform large-scale comparisons that enable meta-learning.
Examples of this advanced functionality include hyperparameter tuning, feature selection, and ensemble construction.
Parallelization of many operations is natively supported.

# Target Audience

`mlr3` provides a domain-specific language for machine learning in R.
We target both **practitioners** who want to quickly apply machine learning algorithms and **researchers** who want to implement, benchmark, and compare their new methods in a structured environment.
The package is a complete rewrite of an earlier version of [`mlr`](https://mlr.mlr-org.com) [@mlr] that leverages many years of experience to provide a state-of-the-art system that is easy to use and extend.
It is intended for users who have basic knowledge in machine learning and R and who are interested in complex projects and advanced functionality rather than one-liners for one specific thing.

# Why a Rewrite?

The predecessor `mlr` was first released to [CRAN](https://cran.r-project.org/) in 2013, with the core design and architecture dating back much further.
Over time, the addition of many features has led to a considerably more complex design that made it harder to build, maintain, and extend than we had hoped for.
With hindsight, we saw that some of the design and architecture changes in `mlr` made it difficult to support new features, in particular with respect to pipelines.
Furthermore, the `R` package ecosystem have undergone major changes in the meantime.
It would have been nearly impossible to integrate all these changes into the original design of `mlr`, especially in a backward-compatible fashion.

# Design Principles

We follow the general design principles:

* Backend over frontend.
  Most packages of the `mlr3` ecosystem focus on processing and transforming data, applying machine learning algorithms, and computing results.
  Graphical user interfaces as well as visualizations of data and results is developed in extra packages.
* Embrace [`R6`](https://cran.r-project.org/package=R6) for a clean object-oriented design, object state-changes, and reference semantics.
* Embrace [`data.table`](https://rdatatable.gitlab.io/data.table/) for fast and efficient computations on rectangular data.
* Unify container and result classes as much as possible and provide result data in `data.table`s.
  This considerably simplifies the API and allows easy selection and "split-apply-combine" (aggregation) operations.
  We combine `data.table` and `R6` to place references to non-atomic and compound objects in tables and make heavy use of list columns.
* Be light on dependencies.
  One of the main maintenance burdens for `mlr` was to keep up with changing learner interfaces and behaviour of the many packages it depended on.
  We require far fewer packages in `mlr3` to make installation and maintenance easier.
  Additionally, by modularizing the package, we are no able to release fixes more rapidly.

# References
