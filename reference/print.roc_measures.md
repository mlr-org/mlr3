# Print ROC Measures

Print the confusion matrix and a set of roc performance measures.

## Usage

``` r
# S3 method for class 'roc_measures'
print(x, abbreviations = TRUE, digits = 2L, ...)
```

## Arguments

- x:

  (`roc_measures`)  
  The object returned by `score_roc_measures`.

- abbreviations:

  (`logical(1)`)  
  If `TRUE`, print a list of abbreviations for the measures.

- digits:

  (`integer(1)`)  
  Number of digits to round the measures to.

- ...:

  (`any`)  
  Additional parameters, currently unused.
