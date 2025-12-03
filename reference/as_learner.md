# Convert to a Learner

Convert object to a
[Learner](https://mlr3.mlr-org.com/reference/Learner.md) or a list of
[Learner](https://mlr3.mlr-org.com/reference/Learner.md).

## Usage

``` r
as_learner(x, ...)

# S3 method for class 'Learner'
as_learner(x, clone = FALSE, discard_state = FALSE, ...)

as_learners(x, ...)

# Default S3 method
as_learners(x, ...)

# S3 method for class 'list'
as_learners(x, ...)
```

## Arguments

- x:

  (any)  
  Object to convert.

- ...:

  (any)  
  Additional arguments.

- clone:

  (`logical(1)`)  
  If `TRUE`, ensures that the returned object is not the same as the
  input `x`.

- discard_state:

  (`logical(1)`) Whether to discard the state.

## Value

[Learner](https://mlr3.mlr-org.com/reference/Learner.md).
