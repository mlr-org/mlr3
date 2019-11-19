---
title: 'mlr3: A modern object-oriented machine learning framework in R'
tags:
  - R
  - machine learning
  - classification
  - regression
authors:
  - name: Michel Lang
    orcid: 0000-0001-9754-0393
    affiliation: "1, 2"
  - name: Martin Binder
    affiliation: 2
  - name: Jakob Richter
    orcid: 0000-0003-4481-5554
    affiliation: 1
  - name: Patrick Schratz
    orcid: 0000-0003-0748-6624
    affiliation: 2
  - name: Florian Pfisterer
    orcid: 0000-0001-8867-762X
    affiliation: 2
  - name: Stefan Coors
    orcid: 0000-0002-7465-2146
    affiliation: 2
  - name: Quay Au
    orcid: 0000-0002-5252-8902
    affiliation: 2
  - name: Giuseppe Casalicchio
    orcid: 0000-0001-5324-5966
    affiliation: 2
  - name: Lars Kotthoff
    orcid: 0000-0003-4635-6873
    affiliation: 3
  - name: Bernd Bischl
    orcid: 0000-0001-6002-6980
    affiliation: 2
affiliations:
 - name: TU Dortmund University
   index: 1
 - name: LMU Munich
   index: 2
 - name: University of Wyoming
   index: 3
date: 1 December 2019
bibliography: paper.bib
---

# Summary

The [`R`](https://www.r-project.org/) [@R] package [`mlr3`](https://mlr3.mlr-org.com) and its associated ecosystem of extension packages implements a powerful, object-oriented and extensible framework for machine learning (ML) in `R`.
It provides a unified interface to many learning algorithms available on [CRAN](https://cran.r-project.org/), augmenting them with model-agnostic general-purpose functionality that is needed in every ML project, for example train-test-evaluation, resampling, preprocessing, hyperparameter tuning, nested resampling, and visualization of results from ML experiments.
The package is a complete reimplementation of the [`mlr`](https://mlr.mlr-org.com) [@mlr] package that leverages many years of experience and learned best practices to provide a state-of-the-art system that is powerful, flexible, extensible, and maintainable.
We target both **practitioners** who want to quickly apply ML algorithms to their problems and **researchers** who want to implement, benchmark, and compare their new methods in a structured environment.
[`mlr3`](https://mlr3.mlr-org.com) is suitable for short scripts that test an idea, for complex multi-stage experiments with advanced functionality that use a broad range of ML functionality, as a foundation to implement new ML (meta-)algorithms (for example AutoML systems), and everything in between.
Functional correctness is ensured through extensive unit and integration tests.

Several other general-purpose ML toolboxes exist for different programing languages.
The most widely used ones are [scikit-learn](https://scikit-learn.org/) [@sk-learn] for Python , [Weka](https://www.cs.waikato.ac.nz/ml/weka/) [@weka] for Java, and [mlj](https://github.com/alan-turing-institute/MLJ.jl) [@mlj] for Julia.
The most important toolboxes for `R` are [mlr](https://cran.r-project.org/package=mlr), [caret](https://cran.r-project.org/package=caret) [@caret] and [tidymodels](https://cran.r-project.org/package=tidymodels) [@tidymodels].

# Lessons Learned from 6 Years of Machine Learning in `R`

The predecessor package [`mlr`](https://mlr.mlr-org.com) was first released to [CRAN](https://cran.r-project.org/) in 2013, with the core design and architecture dating back much further.
As with most software, more code was added over time to integrate more ML algorithms, more approaches for feature selection or hyperparameter tuning, more methods to analyze trained models, and many other things.
With each addition, the code base became larger and more difficult to test and maintain, in particular as changes in the dozens of packages that we integrated with [`mlr`](https://mlr.mlr-org.com) would break our code and prevent releases.
Installing the package with all dependencies and a complete build with all tests would take hours -- we had arrived at a point where adding **any** new functionality became a major undertaking.
Further, some of the architectural and design decisions made it essentially impossible to support new cross-cutting functionality, for example ML pipelines, or using new `R` packages for better performance.

[`mlr3`](https://mlr3.mlr-org.com) takes these lessons learned to heart and now follows these design principles:

* Be modular and light on dependencies.
  The core [`mlr3`](https://mlr3.mlr-org.com) package provides only the basic building blocks of ML: tasks, a few learners, resampling methods, and performance measures.
  Everything else can be installed and loaded separately through additional packages in the [`mlr3`](https://mlr3.mlr-org.com) ecosystem, for example support for other kinds of data, methods for tuning hyperparameters, or integrations for additional ML packages.
* Leverage modern `R` packages, especially [`data.table`](https://rdatatable.gitlab.io/data.table/) for fast and efficient computations on rectangular data.
* Embrace [`R6`](https://cran.r-project.org/package=R6) for a clean object-oriented design, object state changes, and reference semantics.
* Defensive programming and type safety.
  All user input is checked with [`checkmate`](https://cran.r-project.org/package=checkmate) [@checkmate].
  Return types are documented and automatic type casting for "simplification" is avoided.

In addition, we simplified the API considerably by unifying container and result classes. Many result objects are now tabular by mixing `data.table`'s list-column feature with `R6` objects, which also allows for easy and efficient selection and "split-apply-combine" type operations.

# Ecosystem

In addition to the main [`mlr3`](https://cran.r-project.org/package=mlr3) package, [`mlr3learners`](https://cran.r-project.org/package=mlr3learners) provides integrations to a careful selection of the most important ML algorithms and packages in `R`.
Complex ML workflows (using directed acyclic graphs) that can incorporate preprocessing, (stacking) ensembles, alternative-branch execution, and much more can be built with the [`mlr3pipelines`](https://cran.r-project.org/package=mlr3pipelines) package.
Funtionality for hyperparameter tuning and nested resampling of learners and complex pipelines is provided by the [`mlr3tuning`](https://cran.r-project.org/package=mlr3tuning) package.
[`mlr3filters`](https://cran.r-project.org/package=mlr3filters) integrates many feature filtering technqiues and [`mlr3db`](https://cran.r-project.org/package=mlr3db) allows direct use of databases as data sources for out-of-memory data.
We are planning and working on many more packages; for example for Bayesian optimization, Hyperband, probabilistic regression, survival analysis, and spatial and temporal data.
A complete list of existing and planned extension packages can be found on the [mlr3 wiki](https://github.com/mlr-org/mlr3/wiki/Extension-Packages).

[`mlr3`](https://mlr3.mlr-org.com) and its ecosystem are documented in numerous manual pages and a comprehensive [book](https://mlr3book.mlr-org.com) (work in progress).
All packages are licensed under GNU Lesser General Public License ([LGPL-3](https://www.gnu.org/licenses/lgpl-3.0.en.html)).

# Acknowledgments

This work has been funded by the German Federal Ministry of Education and Research (BMBF) under Grant No. 01IS18036A. The authors of this work take full responsibilities for its content.

This work was partly supported by Deutsche Forschungsgemeinschaft (DFG) within the Collaborative Research Center SFB 876, A3.


# References
