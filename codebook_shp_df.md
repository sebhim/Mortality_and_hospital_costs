Codebook created on 2021-03-05 at 2021-03-05 14:50:19
================

## Dataset description

The data contains 101498 cases and 7 variables.

## Codebook

| name  | type      |      n | missing | unique |     mean |   median |      mode | mode\_value |       sd |    v |   min |       max |     range |   skew | skew\_2se |   kurt | kurt\_2se |
| :---- | :-------- | -----: | ------: | -----: | -------: | -------: | --------: | :---------- | -------: | ---: | ----: | --------: | --------: | -----: | --------: | -----: | --------: |
| long  | numeric   | 101498 |       0 |  31395 |    10.70 |    10.46 |     10.46 |             |     2.32 |      |  5.87 |     15.04 |      9.17 |   0.01 |      0.88 | \-1.33 |   \-43.37 |
| lat   | numeric   | 101498 |       0 |  25289 |    53.26 |    54.06 |     54.06 |             |     1.92 |      | 47.27 |     55.06 |      7.79 | \-1.72 |  \-112.14 |   1.89 |     61.40 |
| order | integer   | 101498 |       0 | 101498 | 50749.50 | 50749.50 |  50749.50 |             | 29300.09 |      |  1.00 | 101498.00 | 101497.00 |   0.00 |      0.00 | \-1.20 |   \-39.02 |
| hole  | logical   | 101498 |       0 |      3 |          |          | 101077.00 | FALSE       |          | 0.01 |       |           |           |        |           |        |           |
| piece | factor    | 101498 |       0 |     33 |          |          |  74755.00 | 1           |          | 0.44 |       |           |           |        |           |        |           |
| group | factor    | 101498 |       0 |     99 |          |          |  21618.00 | MV.1        |          | 0.89 |       |           |           |        |           |        |           |
| id    | character | 101498 |       0 |     17 |          |          |  35058.00 | MV          |          | 0.78 |       |           |           |        |           |        |           |

### Legend

  - **Name**: Variable name
  - **type**: Data type of the variable
  - **missing**: Proportion of missing values for this variable
  - **unique**: Number of unique values
  - **mean**: Mean value
  - **median**: Median value
  - **mode**: Most common value (for categorical variables, this shows
    the frequency of the most common category)
  - **mode\_value**: For categorical variables, the value of the most
    common category
  - **sd**: Standard deviation (measure of dispersion for numerical
    variables
  - **v**: Agresti’s V (measure of dispersion for categorical variables)
  - **min**: Minimum value
  - **max**: Maximum value
  - **range**: Range between minimum and maximum value
  - **skew**: Skewness of the variable
  - **skew\_2se**: Skewness of the variable divided by 2\*SE of the
    skewness. If this is greater than abs(1), skewness is significant
  - **kurt**: Kurtosis (peakedness) of the variable
  - **kurt\_2se**: Kurtosis of the variable divided by 2\*SE of the
    kurtosis. If this is greater than abs(1), kurtosis is significant.

This codebook was generated using the [Workflow for Open Reproducible
Code in Science (WORCS)](https://osf.io/zcvbs/)
