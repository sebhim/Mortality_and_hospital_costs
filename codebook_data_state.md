Codebook created on 2021-03-05 at 2021-03-05 14:50:21
================

## Dataset description

The data contains 6160 cases and 18 variables.

## Codebook

| name                             | type             |    n | missing | unique |      mean |    median |      mode | mode\_value |         sd |    v |           min |         max |       range |   skew | skew\_2se |  kurt | kurt\_2se |
| :------------------------------- | :--------------- | ---: | ------: | -----: | --------: | --------: | --------: | :---------- | ---------: | ---: | ------------: | ----------: | ----------: | -----: | --------: | ----: | --------: |
| ID                               | factor           | 6160 |    0.00 |    561 |           |           |     11.00 | 1           |            | 1.00 |               |             |             |        |           |       |           |
| year                             | factor           | 6160 |    0.00 |     12 |           |           |    560.00 | 2007        |            | 0.91 |               |             |             |        |           |       |           |
| region                           | factor           | 6160 |    0.00 |     17 |           |           |    385.00 | BB          |            | 0.94 |               |             |             |        |           |       |           |
| gender                           | factor           | 6160 |    0.00 |      3 |           |           |   3168.00 | Female      |            | 0.50 |               |             |             |        |           |       |           |
| agegroup                         | factor           | 6160 |    0.00 |     19 |           |           |    352.00 | \<1         |            | 0.94 |               |             |             |        |           |       |           |
| population                       | numeric          | 6160 |    0.00 |   5851 | 145363.25 |  84419.00 |  84419.00 |             |  164389.79 |      |       2595.00 |  1275591.00 |  1272996.00 |   2.32 |     37.11 |  6.96 |     55.74 |
| d\_log\_CVD\_mort\_100k          | pseries, numeric | 4912 |    0.20 |   4406 |    \-0.03 |    \-0.03 |    \-0.03 |             |       0.39 |      |        \-2.08 |        2.48 |        4.56 |   0.12 |      1.66 |  6.17 |     44.14 |
| d\_CVD\_mort\_100k               | pseries, numeric | 5600 |    0.09 |   2940 |   \-26.82 |    \-0.80 |    \-0.80 |             |     162.59 |      |     \-1855.90 |     1693.10 |     3549.00 | \-3.06 |   \-46.74 | 34.41 |    262.91 |
| d\_log\_CVD\_costs\_100k         | pseries, numeric | 5586 |    0.09 |   5587 |      0.03 |      0.02 |      0.02 |             |       0.25 |      |        \-2.37 |        3.42 |        5.80 |   0.53 |      8.11 | 26.63 |    203.23 |
| d\_CVD\_costs\_100k              | pseries, numeric | 5586 |    0.09 |   5587 | 566638.93 | 121570.81 | 121570.81 |             | 1952082.92 |      | \-17218781.41 | 20702205.44 | 37920986.85 |   1.84 |     28.13 | 17.26 |    131.70 |
| d\_lag\_log\_CVD\_costs\_100k    | pseries, numeric | 5026 |    0.18 |   5027 |      0.03 |      0.03 |      0.03 |             |       0.25 |      |        \-2.37 |        3.42 |        5.80 |   0.51 |      7.41 | 27.63 |    200.03 |
| d\_lag\_CVD\_costs\_100k         | pseries, numeric | 5026 |    0.18 |   5027 | 603242.70 | 130112.44 | 130112.44 |             | 1933791.78 |      | \-17218781.41 | 19708419.17 | 36927200.58 |   1.90 |     27.46 | 15.61 |    113.00 |
| d\_log\_cancer\_mort\_100k       | pseries, numeric | 5126 |    0.17 |   4731 |    \-0.02 |    \-0.01 |    \-0.01 |             |       0.38 |      |        \-2.40 |        1.99 |        4.39 | \-0.24 |    \-3.57 |  7.37 |     53.88 |
| d\_cancer\_mort\_100k            | pseries, numeric | 5600 |    0.09 |   2954 |    \-3.46 |    \-0.30 |    \-0.30 |             |      62.25 |      |      \-648.90 |      764.10 |     1413.00 |   0.33 |      5.03 | 24.19 |    184.82 |
| d\_log\_cancer\_costs\_100k      | pseries, numeric | 5393 |    0.12 |   5394 |      0.02 |      0.02 |      0.02 |             |       0.27 |      |        \-2.90 |        2.58 |        5.48 |   0.54 |      8.13 | 19.71 |    147.79 |
| d\_cancer\_costs\_100k           | pseries, numeric | 5393 |    0.12 |   5394 | 240343.44 | 101990.03 | 101990.03 |             | 1076159.10 |      |  \-6225133.67 | 11347851.74 | 17572985.41 |   0.92 |     13.83 |  8.21 |     61.58 |
| d\_lag\_log\_cancer\_costs\_100k | pseries, numeric | 4849 |    0.21 |   4850 |      0.02 |      0.02 |      0.02 |             |       0.27 |      |        \-2.90 |        2.58 |        5.48 |   0.67 |      9.58 | 20.67 |    146.96 |
| d\_lag\_cancer\_costs\_100k      | pseries, numeric | 4849 |    0.21 |   4850 | 234314.18 | 102795.92 | 102795.92 |             | 1080367.90 |      |  \-6225133.67 | 11347851.74 | 17572985.41 |   0.90 |     12.74 |  8.60 |     61.15 |

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
