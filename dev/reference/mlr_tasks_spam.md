# Spam Classification Task

Spam data set from the UCI machine learning repository
(<http://archive.ics.uci.edu/dataset/94/spambase>). Data set collected
at Hewlett-Packard Labs to classify emails as spam or non-spam. 57
variables indicate the frequency of certain words and characters in the
e-mail. The positive class is set to "spam".

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) inheriting
from
[TaskClassif](https://mlr3.mlr-org.com/dev/reference/TaskClassif.md).

## Source

Creators: Mark Hopkins, Erik Reeber, George Forman, Jaap Suermondt.
Hewlett-Packard Labs, 1501 Page Mill Rd., Palo Alto, CA 94304

Donor: George Forman (gforman at nospam hpl.hp.com) 650-857-7835

Preprocessing: Columns have been renamed. Preprocessed data taken from
the [kernlab](https://CRAN.R-project.org/package=kernlab) package.

## Dictionary

This [Task](https://mlr3.mlr-org.com/dev/reference/Task.md) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_tasks](https://mlr3.mlr-org.com/dev/reference/mlr_tasks.md) or with
the associated sugar function
[`tsk()`](https://mlr3.mlr-org.com/dev/reference/mlr_sugar.md):

    mlr_tasks$get("spam")
    tsk("spam")

## Meta Information

- Task type: “classif”

- Dimensions: 4601x58

- Properties: “twoclass”

- Has Missings: `FALSE`

- Target: “type”

- Features: “address”, “addresses”, “all”, “business”, “capitalAve”,
  “capitalLong”, “capitalTotal”, “charDollar”, “charExclamation”,
  “charHash”, “charRoundbracket”, “charSemicolon”, “charSquarebracket”,
  “conference”, “credit”, “cs”, “data”, “direct”, “edu”, “email”,
  “font”, “free”, “george”, “hp”, “hpl”, “internet”, “lab”, “labs”,
  “mail”, “make”, “meeting”, “money”, “num000”, “num1999”, “num3d”,
  “num415”, “num650”, “num85”, “num857”, “order”, “original”, “our”,
  “over”, “parts”, “people”, “pm”, “project”, “re”, “receive”, “remove”,
  “report”, “table”, “technology”, “telnet”, “will”, “you”, “your”

## References

Dua, Dheeru, Graff, Casey (2017). “UCI Machine Learning Repository.”
<http://archive.ics.uci.edu/datasets>.

## See also

- Chapter in the [mlr3book](https://mlr3book.mlr-org.com/):
  <https://mlr3book.mlr-org.com/chapters/chapter2/data_and_basic_modeling.html>

- Package [mlr3data](https://CRAN.R-project.org/package=mlr3data) for
  more toy tasks.

- Package [mlr3oml](https://CRAN.R-project.org/package=mlr3oml) for
  downloading tasks from <https://www.openml.org>.

- Package [mlr3viz](https://CRAN.R-project.org/package=mlr3viz) for some
  generic visualizations.

- [Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
  of [Tasks](https://mlr3.mlr-org.com/dev/reference/Task.md):
  [mlr_tasks](https://mlr3.mlr-org.com/dev/reference/mlr_tasks.md)

- `as.data.table(mlr_tasks)` for a table of available
  [Tasks](https://mlr3.mlr-org.com/dev/reference/Task.md) in the running
  session (depending on the loaded packages).

- [mlr3fselect](https://CRAN.R-project.org/package=mlr3fselect) and
  [mlr3filters](https://CRAN.R-project.org/package=mlr3filters) for
  feature selection and feature filtering.

- Extension packages for additional task types:

  - Unsupervised clustering:
    [mlr3cluster](https://CRAN.R-project.org/package=mlr3cluster)

  - Probabilistic supervised regression and survival analysis:
    <https://mlr3proba.mlr-org.com/>.

Other Task: [`Task`](https://mlr3.mlr-org.com/dev/reference/Task.md),
[`TaskClassif`](https://mlr3.mlr-org.com/dev/reference/TaskClassif.md),
[`TaskRegr`](https://mlr3.mlr-org.com/dev/reference/TaskRegr.md),
[`TaskSupervised`](https://mlr3.mlr-org.com/dev/reference/TaskSupervised.md),
[`TaskUnsupervised`](https://mlr3.mlr-org.com/dev/reference/TaskUnsupervised.md),
[`california_housing`](https://mlr3.mlr-org.com/dev/reference/california_housing.md),
[`mlr_tasks`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks.md),
[`mlr_tasks_breast_cancer`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_breast_cancer.md),
[`mlr_tasks_german_credit`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_german_credit.md),
[`mlr_tasks_iris`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_iris.md),
[`mlr_tasks_mtcars`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_mtcars.md),
[`mlr_tasks_penguins`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_penguins.md),
[`mlr_tasks_pima`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_pima.md),
[`mlr_tasks_sonar`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_sonar.md),
[`mlr_tasks_wine`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_wine.md),
[`mlr_tasks_zoo`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_zoo.md)
