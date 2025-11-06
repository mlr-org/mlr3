# Create a Fallback Learner

Create a fallback learner for a given learner. The function searches for
a suitable fallback learner based on the task type. Additional checks
are performed to ensure that the fallback learner supports the predict
type.

## Usage

``` r
default_fallback(learner, ...)

# S3 method for class 'Learner'
default_fallback(learner, ...)

# S3 method for class 'LearnerClassif'
default_fallback(learner, ...)

# S3 method for class 'LearnerRegr'
default_fallback(learner, ...)
```

## Arguments

- learner:

  [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md)  
  The learner for which a fallback learner should be created.

- ...:

  `any`  
  ignored.

## Value

[Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md)
