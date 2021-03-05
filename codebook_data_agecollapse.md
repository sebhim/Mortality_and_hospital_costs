Codebook created on 2021-03-05 at 2021-03-05 14:50:23
================

## Dataset description

The data contains 3168 cases and 24 variables.

## Codebook

| name                             | type             |    n | missing | unique |        mean |      median |        mode | mode\_value |          sd |    v |           min |          max |        range |   skew | skew\_2se |   kurt | kurt\_2se |
| :------------------------------- | :--------------- | ---: | ------: | -----: | ----------: | ----------: | ----------: | :---------- | ----------: | ---: | ------------: | -----------: | -----------: | -----: | --------: | -----: | --------: |
| ID                               | factor           | 3168 |    0.00 |    289 |             |             |       11.00 | 1           |             | 1.00 |               |              |              |        |           |        |           |
| year                             | factor           | 3168 |    0.00 |     12 |             |             |      288.00 | 2007        |             | 0.91 |               |              |              |        |           |        |           |
| gender                           | factor           | 3168 |    0.00 |      3 |             |             |     1584.00 | Female      |             | 0.50 |               |              |              |        |           |        |           |
| agegroup                         | character        | 3168 |    0.00 |     10 |             |             |      352.00 | 1           |             | 0.89 |               |              |              |        |           |        |           |
| region                           | factor           | 3168 |    0.00 |     17 |             |             |      198.00 | BB          |             | 0.94 |               |              |              |        |           |        |           |
| population                       | numeric          | 2992 |    0.06 |   2989 |   299277.27 |   174729.00 |   174729.00 |             |   322445.40 |      |       3857.00 |   1894856.00 |   1890999.00 |   1.93 |     21.61 |   3.76 |     20.98 |
| d\_log\_CVD\_mort\_100k          | pseries, numeric | 2715 |    0.14 |   2669 |             |      \-0.02 |      \-0.02 |             |             |      |         \-Inf |          Inf |          Inf |        |           |        |           |
| d\_CVD\_mort\_100k               | pseries, numeric | 2720 |    0.14 |   2717 |     \-33.67 |      \-0.84 |      \-0.84 |             |      190.07 |      |     \-1855.96 |      1693.17 |      3549.13 | \-2.95 |   \-31.38 |  29.78 |    158.67 |
| CVD\_mort\_100k                  | numeric          | 2992 |    0.06 |   2959 |     1464.66 |       61.47 |       61.47 |             |     3350.42 |      |          0.00 |     17966.56 |     17966.56 |   3.03 |     33.83 |   8.72 |     48.72 |
| CVD\_deaths                      | numeric          | 2992 |    0.06 |   1370 |     1221.23 |      163.50 |      163.50 |             |     2486.63 |      |          0.00 |     21046.00 |     21046.00 |   3.55 |     39.62 |  15.55 |     86.90 |
| d\_log\_CVD\_costs\_100k         | pseries, numeric | 2720 |    0.14 |   2721 |        0.03 |        0.02 |        0.02 |             |        0.12 |      |        \-1.08 |         0.84 |         1.93 |   0.00 |      0.04 |   9.90 |     52.74 |
| d\_CVD\_costs\_100k              | pseries, numeric | 2720 |    0.14 |   2721 |   638855.42 |   145215.13 |   145215.13 |             |  1738598.71 |      | \-10030069.86 |  16077707.64 |  26107777.50 |   2.04 |     21.76 |  12.73 |     67.80 |
| CVD\_costs\_100k                 | numeric          | 2992 |    0.06 |   2993 | 24993176.90 |  7704577.17 |  7704577.17 |             | 29698282.98 |      |     133638.09 | 116557235.16 | 116423597.07 |   1.05 |     11.76 | \-0.12 |    \-0.70 |
| CVD\_costs\_sum                  | numeric          | 3168 |    0.00 |   2993 | 42362951.43 | 12670074.06 | 12670074.06 |             | 73582002.35 |      |          0.00 | 644363674.27 | 644363674.27 |   3.45 |     39.65 |  15.57 |     89.53 |
| d\_lag\_log\_CVD\_costs\_100k    | pseries, numeric | 2448 |    0.23 |   2449 |        0.03 |        0.03 |        0.03 |             |        0.12 |      |        \-1.08 |         0.84 |         1.93 | \-0.04 |    \-0.44 |  10.24 |     51.75 |
| d\_lag\_CVD\_costs\_100k         | pseries, numeric | 2448 |    0.23 |   2449 |   674592.06 |   153719.94 |   153719.94 |             |  1760049.94 |      | \-10030069.86 |  16077707.64 |  26107777.50 |   1.97 |     19.91 |  12.19 |     61.61 |
| d\_log\_cancer\_mort\_100k       | pseries, numeric | 2717 |    0.14 |   2696 |             |      \-0.01 |      \-0.01 |             |             |      |         \-Inf |          Inf |          Inf |        |           |        |           |
| d\_cancer\_mort\_100k            | pseries, numeric | 2720 |    0.14 |   2719 |      \-3.30 |      \-0.57 |      \-0.57 |             |       54.04 |      |      \-453.81 |       534.80 |       988.61 |   0.01 |      0.06 |  17.64 |     93.97 |
| cancer\_deaths                   | numeric          | 2992 |    0.06 |   1351 |      823.52 |      260.00 |      260.00 |             |     1378.29 |      |          0.00 |     10343.00 |     10343.00 |   3.07 |     34.35 |  11.81 |     66.00 |
| d\_log\_cancer\_costs\_100k      | pseries, numeric | 2720 |    0.14 |   2716 |             |        0.02 |        0.02 |             |             |      |         \-Inf |          Inf |          Inf |        |           |        |           |
| d\_cancer\_costs\_100k           | pseries, numeric | 2720 |    0.14 |   2721 |   246281.78 |   102938.58 |   102938.58 |             |   899179.78 |      | \-10182282.45 |   9232821.16 |  19415103.61 |   0.73 |      7.82 |  17.29 |     92.14 |
| cancer\_costs\_sum               | numeric          | 3168 |    0.00 |   2989 | 23683879.31 |  9128360.78 |  9128360.78 |             | 37959505.14 |      |          0.00 | 305021959.40 | 305021959.40 |   3.23 |     37.07 |  13.39 |     76.99 |
| d\_lag\_log\_cancer\_costs\_100k | pseries, numeric | 2448 |    0.23 |   2445 |             |        0.02 |        0.02 |             |             |      |         \-Inf |          Inf |          Inf |        |           |        |           |
| d\_lag\_cancer\_costs\_100k      | pseries, numeric | 2448 |    0.23 |   2449 |   231552.02 |    97919.85 |    97919.85 |             |   888467.12 |      | \-10182282.45 |   8727949.37 |  18910231.82 |   0.34 |      3.43 |  15.82 |     79.98 |

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
