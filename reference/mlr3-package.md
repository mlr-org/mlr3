# mlr3: Machine Learning in R - Next Generation

Efficient, object-oriented programming on the building blocks of machine
learning. Provides 'R6' objects for tasks, learners, resamplings, and
measures. The package is geared towards scalability and larger datasets
by supporting parallelization and out-of-memory data-backends like
databases. While 'mlr3' focuses on the core computational operations,
add-on packages provide additional functionality.

## Learn mlr3

- Book on mlr3: <https://mlr3book.mlr-org.com>

- Use cases and examples gallery: <https://mlr3gallery.mlr-org.com>

- Cheat Sheets: <https://github.com/mlr-org/mlr3cheatsheets>

## mlr3 extensions

- Preprocessing and machine learning pipelines:
  [mlr3pipelines](https://CRAN.R-project.org/package=mlr3pipelines)

- Analysis of benchmark experiments:
  [mlr3benchmark](https://CRAN.R-project.org/package=mlr3benchmark)

- More classification and regression tasks:
  [mlr3data](https://CRAN.R-project.org/package=mlr3data)

- Connector to [OpenML](https://www.openml.org):
  [mlr3oml](https://CRAN.R-project.org/package=mlr3oml)

- Solid selection of good classification and regression learners:
  [mlr3learners](https://CRAN.R-project.org/package=mlr3learners)

- Even more learners: <https://github.com/mlr-org/mlr3extralearners>

- Tuning of hyperparameters:
  [mlr3tuning](https://CRAN.R-project.org/package=mlr3tuning)

- Hyperband tuner:
  [mlr3hyperband](https://CRAN.R-project.org/package=mlr3hyperband)

- Visualizations for many mlr3 objects:
  [mlr3viz](https://CRAN.R-project.org/package=mlr3viz)

- Survival analysis and probabilistic regression:
  [mlr3proba](https://CRAN.R-project.org/package=mlr3proba)

- Cluster analysis:
  [mlr3cluster](https://CRAN.R-project.org/package=mlr3cluster)

- Feature selection filters:
  [mlr3filters](https://CRAN.R-project.org/package=mlr3filters)

- Feature selection wrappers:
  [mlr3fselect](https://CRAN.R-project.org/package=mlr3fselect)

- Interface to real (out-of-memory) data bases:
  [mlr3db](https://CRAN.R-project.org/package=mlr3db)

- Performance measures as plain functions:
  [mlr3measures](https://CRAN.R-project.org/package=mlr3measures)

- Resampling methods for spatiotemporal data:
  [mlr3spatiotempcv](https://CRAN.R-project.org/package=mlr3spatiotempcv)

- Data storage and prediction support for spatial objects:
  [mlr3spatial](https://CRAN.R-project.org/package=mlr3spatial)

## Suggested packages

- Parallelization framework:
  [future](https://CRAN.R-project.org/package=future)

- Progress bars:
  [progressr](https://CRAN.R-project.org/package=progressr)

- Encapsulated evaluation:
  [evaluate](https://CRAN.R-project.org/package=evaluate),
  [callr](https://CRAN.R-project.org/package=callr) (external process)

## Package Options

- `"mlr3.exec_random"`: Randomize the order of execution in
  [`resample()`](https://mlr3.mlr-org.com/reference/resample.md) and
  [`benchmark()`](https://mlr3.mlr-org.com/reference/benchmark.md)
  during parallelization with
  [future](https://CRAN.R-project.org/package=future). Defaults to
  `TRUE`. Note that this does not affect the order of results.

- `"mlr3.exec_chunk_size"`: Number of iterations to perform in a single
  [`future::future()`](https://future.futureverse.org/reference/future.html)
  during parallelization with
  [future](https://CRAN.R-project.org/package=future). Defaults to 1.

- `"mlr3.exec_chunk_bins"`: Number of bins to split the iterations into.
  If set, `"mlr3.exec_chunk_size"` is ignored.

- `"mlr3.debug"`: If set to `TRUE`, parallelization via
  [future](https://CRAN.R-project.org/package=future) is disabled to
  simplify debugging and provide more concise tracebacks. Note that
  results computed in debug mode use a different seeding mechanism and
  are **not reproducible**.

- `"mlr3.warn_version_mismatch"`: Set to `FALSE` to silence warnings
  raised during predict if a learner has been trained with a different
  version version of mlr3.

- `"mlr3.prob_as_default"`: Set to `TRUE` to set the predict type of
  classification learners to `"prob"` by default (if they support it).

- `"mlr3.mirai_parallelization"`: Compute profile to use for
  parallelization with
  [mirai](https://CRAN.R-project.org/package=mirai). Defaults to
  `"mlr3_parallelization"`.

- `"mlr3.mirai_encapsulation"`: Compute profile to use for encapsulation
  with [mirai](https://CRAN.R-project.org/package=mirai). Defaults to
  `"mlr3_encapsulation"`.

## Error Classes

- `Mlr3Error`: The base mlr3 error class.

- `Mlr3ErrorConfig`: This error signals that the user has misconfigured
  something. By default, this error is not caught when the learner is
  encapsulated.

- `Mlr3ErrorInput`: This error signals that the input to the function is
  invalid.

- `Mlr3ErrorLearner`: The base error class for errors related to the
  learner.

- `Mlr3ErrorLearnerTrain`: This error signals that the learner failed to
  train the model.

- `Mlr3ErrorLearnerPredict`: This error signals that something went
  wrong during prediction.

- `Mlr3TimeoutError`: This error signals that the encapsulation during
  train or predict timed out.

## Warning Classes

- `Mlr3Warning`: The base mlr3 warning class.

- `Mlr3WarningConfig`: This warning signals that the user has
  misconfigured something.

- `Mlr3WarningInput`: This warning signals that the input to the
  function is invalid.

## References

Lang M, Binder M, Richter J, Schratz P, Pfisterer F, Coors S, Au Q,
Casalicchio G, Kotthoff L, Bischl B (2019). “mlr3: A modern
object-oriented machine learning framework in R.” *Journal of Open
Source Software*.
[doi:10.21105/joss.01903](https://doi.org/10.21105/joss.01903) ,
<https://joss.theoj.org/papers/10.21105/joss.01903>.

## See also

Useful links:

- <https://mlr3.mlr-org.com>

- <https://github.com/mlr-org/mlr3>

- Report bugs at <https://github.com/mlr-org/mlr3/issues>

## Author

**Maintainer**: Marc Becker <marcbecker@posteo.de>
([ORCID](https://orcid.org/0000-0002-8115-0400))

Authors:

- Michel Lang <michellang@gmail.com>
  ([ORCID](https://orcid.org/0000-0001-9754-0393))

- Bernd Bischl <bernd_bischl@gmx.net>
  ([ORCID](https://orcid.org/0000-0001-6002-6980))

- Jakob Richter <jakob1richter@gmail.com>
  ([ORCID](https://orcid.org/0000-0003-4481-5554))

- Patrick Schratz <patrick.schratz@gmail.com>
  ([ORCID](https://orcid.org/0000-0003-0748-6624))

- Martin Binder <mlr.developer@mb706.com>

- Florian Pfisterer <pfistererf@googlemail.com>
  ([ORCID](https://orcid.org/0000-0001-8867-762X))

- Raphael Sonabend <raphaelsonabend@gmail.com>
  ([ORCID](https://orcid.org/0000-0001-9225-4654))

- Sebastian Fischer <sebf.fischer@gmail.com>
  ([ORCID](https://orcid.org/0000-0002-9609-3197))

Other contributors:

- Giuseppe Casalicchio <giuseppe.casalicchio@stat.uni-muenchen.de>
  ([ORCID](https://orcid.org/0000-0001-5324-5966)) \[contributor\]

- Stefan Coors <mail@stefancoors.de>
  ([ORCID](https://orcid.org/0000-0002-7465-2146)) \[contributor\]

- Quay Au <quayau@gmail.com>
  ([ORCID](https://orcid.org/0000-0002-5252-8902)) \[contributor\]

- Lennart Schneider <lennart.sch@web.de>
  ([ORCID](https://orcid.org/0000-0003-4152-5308)) \[contributor\]

- Lona Koers <lona.koers@gmail.com> \[contributor\]

- John Zobolas <bblodfon@gmail.com>
  ([ORCID](https://orcid.org/0000-0002-3609-8674)) \[contributor\]

- Maximilian Mücke <muecke.maximilian@gmail.com>
  ([ORCID](https://orcid.org/0009-0000-9432-9795)) \[contributor\]
