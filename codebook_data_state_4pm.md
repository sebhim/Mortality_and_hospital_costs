Codebook created on 2021-03-01 at 2021-03-01 10:30:49
================

## Dataset description

The data contains 2240 cases and 30 variables.

## Codebook

| name                             | type             |    n | missing | unique |        mean |     median |       mode | mode\_value |          sd |    v |           min |          max |        range |   skew | skew\_2se |   kurt | kurt\_2se |
| :------------------------------- | :--------------- | ---: | ------: | -----: | ----------: | ---------: | ---------: | :---------- | ----------: | ---: | ------------: | -----------: | -----------: | -----: | --------: | -----: | --------: |
| ID                               | factor           | 2240 |    0.00 |    561 |             |            |       4.00 | 1           |             | 1.00 |               |              |              |        |           |        |           |
| year                             | factor           | 2240 |    0.00 |      5 |             |            |     560.00 | 1           |             | 0.75 |               |              |              |        |           |        |           |
| gender                           | factor           | 2240 |    0.00 |      3 |             |            |    1152.00 | Female      |             | 0.50 |               |              |              |        |           |        |           |
| agegroup                         | factor           | 2240 |    0.00 |     19 |             |            |     128.00 | \<1         |             | 0.94 |               |              |              |        |           |        |           |
| region                           | factor           | 2240 |    0.00 |     17 |             |            |     140.00 | BB          |             | 0.94 |               |              |              |        |           |        |           |
| population                       | numeric          | 2240 |    0.00 |   2191 |   145287.04 |   84343.50 |   84343.50 |             |   164156.05 |      |       2595.00 |   1248276.00 |   1245681.00 |   2.31 |     22.32 |   6.90 |     33.38 |
| d\_log\_CVD\_mort\_100k          | pseries, numeric | 1461 |    0.35 |   1397 |      \-0.09 |     \-0.09 |     \-0.09 |             |        0.37 |      |        \-2.12 |         1.79 |         3.91 | \-0.17 |    \-1.36 |   6.25 |     24.41 |
| log\_CVD\_mort\_100k             | numeric          | 2052 |    0.08 |   1378 |        4.30 |       4.17 |       4.17 |             |        2.77 |      |        \-2.30 |         9.80 |        12.10 |   0.12 |      1.13 | \-1.04 |    \-4.80 |
| d\_CVD\_mort\_100k               | pseries, numeric | 1680 |    0.25 |   1179 |     \-88.71 |     \-3.10 |     \-3.10 |             |      263.07 |      |     \-2762.90 |      1073.80 |      3836.70 | \-4.58 |   \-38.36 |  28.45 |    119.18 |
| CVD\_mort\_100k                  | numeric          | 2240 |    0.00 |   1378 |     1075.33 |      42.55 |      42.55 |             |     2658.96 |      |          0.00 |     17966.60 |     17966.60 |   3.56 |     34.44 |  13.67 |     66.09 |
| d\_log\_CVD\_costs\_100k         | pseries, numeric | 1679 |    0.25 |   1680 |        0.08 |       0.07 |       0.07 |             |        0.28 |      |        \-2.99 |         2.50 |         5.49 | \-0.63 |    \-5.29 |  29.82 |    124.90 |
| log\_CVD\_costs\_100k            | numeric          | 2239 |    0.00 |   2240 |       15.66 |      15.63 |      15.63 |             |        1.87 |      |         11.75 |        18.64 |         6.89 | \-0.03 |    \-0.31 | \-1.38 |    \-6.69 |
| d\_CVD\_costs\_100k              | pseries, numeric | 1679 |    0.25 |   1680 |  1776969.82 |  374284.43 |  374284.43 |             |  3279933.34 |      |  \-5035099.50 |  22069540.97 |  27104640.47 |   2.37 |     19.84 |   6.74 |     28.23 |
| CVD\_costs\_100k                 | numeric          | 2239 |    0.00 |   2240 | 23058216.95 | 6147241.79 | 6147241.79 |             | 29850087.22 |      |     126441.18 | 124642703.82 | 124516262.63 |   1.25 |     12.11 |   0.36 |      1.75 |
| d\_lag\_log\_CVD\_costs\_100k    | pseries, numeric | 1675 |    0.25 |   1676 |        0.08 |       0.08 |       0.08 |             |        0.24 |      |        \-2.34 |         2.44 |         4.78 |   0.20 |      1.66 |  19.96 |     83.49 |
| lag\_log\_CVD\_costs\_100k       | numeric          | 2235 |    0.00 |   2236 |       15.64 |      15.60 |      15.60 |             |        1.87 |      |         11.71 |        18.64 |         6.93 | \-0.03 |    \-0.30 | \-1.39 |    \-6.70 |
| d\_lag\_CVD\_costs\_100k         | pseries, numeric | 1675 |    0.25 |   1676 |  1809954.36 |  413838.91 |  413838.91 |             |  3394005.46 |      | \-13637201.99 |  26616072.05 |  40253274.03 |   2.20 |     18.38 |   7.36 |     30.79 |
| lag\_CVD\_costs\_100k            | numeric          | 2235 |    0.00 |   2236 | 22592932.60 | 5976472.25 | 5976472.25 |             | 29199121.61 |      |     122127.46 | 124777098.28 | 124654970.82 |   1.25 |     12.04 |   0.35 |      1.70 |
| d\_log\_cancer\_mort\_100k       | pseries, numeric | 1536 |    0.31 |   1481 |      \-0.06 |     \-0.04 |     \-0.04 |             |        0.36 |      |        \-2.08 |         2.15 |         4.23 | \-0.39 |    \-3.11 |   7.31 |     29.28 |
| log\_cancer\_mort\_100k          | numeric          | 2110 |    0.06 |   1483 |        4.46 |       4.85 |       4.85 |             |        2.38 |      |        \-0.69 |         8.07 |         8.77 | \-0.22 |    \-2.03 | \-1.35 |    \-6.36 |
| d\_cancer\_mort\_100k            | pseries, numeric | 1680 |    0.25 |   1203 |     \-10.34 |     \-1.60 |     \-1.60 |             |       67.23 |      |      \-472.40 |       499.40 |       971.80 | \-0.20 |    \-1.69 |  13.86 |     58.07 |
| cancer\_mort\_100k               | numeric          | 2240 |    0.00 |   1483 |      471.68 |      87.10 |      87.10 |             |      693.96 |      |          0.00 |      3210.10 |      3210.10 |   1.69 |     16.31 |   2.14 |     10.33 |
| d\_log\_cancer\_costs\_100k      | pseries, numeric | 1612 |    0.28 |   1613 |        0.06 |       0.07 |       0.07 |             |        0.28 |      |        \-2.65 |         2.15 |         4.80 | \-0.91 |    \-7.47 |  16.76 |     68.79 |
| log\_cancer\_costs\_100k         | numeric          | 2197 |    0.02 |   2198 |       15.41 |      15.64 |      15.64 |             |        1.41 |      |         11.91 |        17.63 |         5.72 | \-0.18 |    \-1.70 | \-1.39 |    \-6.67 |
| d\_cancer\_costs\_100k           | pseries, numeric | 1612 |    0.28 |   1613 |   771518.85 |  352050.14 |  352050.14 |             |  1513349.91 |      |  \-5578061.73 |  13741791.01 |  19319852.74 |   1.53 |     12.51 |   6.18 |     25.38 |
| cancer\_costs\_100k              | numeric          | 2197 |    0.02 |   2198 | 10731297.71 | 6208295.73 | 6208295.73 |             | 11060618.17 |      |     148244.90 |  45283593.41 |  45135348.52 |   0.95 |      9.11 | \-0.21 |    \-1.02 |
| d\_lag\_log\_cancer\_costs\_100k | pseries, numeric | 1621 |    0.28 |   1622 |        0.07 |       0.07 |       0.07 |             |        0.31 |      |        \-2.06 |         2.67 |         4.73 |   0.60 |      4.96 |  14.83 |     61.04 |
| lag\_log\_cancer\_costs\_100k    | numeric          | 2189 |    0.02 |   2190 |       15.38 |      15.65 |      15.65 |             |        1.42 |      |         11.55 |        17.62 |         6.08 | \-0.20 |    \-1.91 | \-1.36 |    \-6.50 |
| d\_lag\_cancer\_costs\_100k      | pseries, numeric | 1621 |    0.28 |   1622 |   703185.41 |  320634.19 |  320634.19 |             |  1349266.77 |      |  \-4224493.01 |   9295326.94 |  13519819.95 |   1.63 |     13.43 |   5.43 |     22.36 |
| lag\_cancer\_costs\_100k         | numeric          | 2189 |    0.02 |   2190 | 10499830.57 | 6239327.60 | 6239327.60 |             | 10770574.44 |      |     103483.03 |  45087647.09 |  44984164.06 |   0.93 |      8.93 | \-0.25 |    \-1.21 |

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
