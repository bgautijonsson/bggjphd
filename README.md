
<!-- README.md is generated from README.Rmd. Please edit that file -->

# About this page

Information about this package and why it was created can be found at
[my website](https://www.bggj.is/phd)

# Installing the package

The package can be installed with [pak](https://pak.r-lib.org/) or
[remotes](https://remotes.r-lib.org/). I recommend using `pak`, since it
can also install any non-R dependencies that might be required. I’ve
started to move all my package installation to `pak`, and use it for
example to set up my [Docker
container](https://hub.docker.com/r/bgautijonsson/phdocker) that sets up
an R environment along with any needed dependencies for `bggjphd`.

``` r
# using devtools
install.packages("remotes")
remotes::install_github("bgautijonsson/bggjphd")
```

``` r
# using pak
install.packages("pak")
pak::pak("bgautijonsson/bggjphd")
```

# Data

The `bggjphd` package comes with some dataset

``` r
library(bggjphd)
#> 
#> Attaching package: 'bggjphd'
#> The following object is masked from 'package:datasets':
#> 
#>     precip
```

## precip

The `precip` dataset contains maximum hourly precipitation calculated
yearly for each of the locations in the [UKCP Local Projections on a 5km
grid over the UK for
1980-2080](https://catalogue.ceda.ac.uk/uuid/e304987739e04cdc960598fa5e4439d0)
from the CEDA archive.

``` r
skimr::skim(precip)
```

|                                                  |         |
|:-------------------------------------------------|:--------|
| Name                                             | precip  |
| Number of rows                                   | 2723040 |
| Number of columns                                | 3       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |         |
| Column type frequency:                           |         |
| numeric                                          | 3       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |         |
| Group variables                                  | None    |

Data summary

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |     mean |       sd |      p0 |      p25 |      p50 |      p75 |     p100 | hist  |
|:--------------|----------:|--------------:|---------:|---------:|--------:|---------:|---------:|---------:|---------:|:------|
| year          |         0 |             1 |  2030.81 |    32.87 | 1981.00 |  1996.00 |  2030.50 |  2065.00 |  2080.00 | ▇▁▇▁▇ |
| station       |         0 |             1 | 21960.50 | 12678.61 |    1.00 | 10980.75 | 21960.50 | 32940.25 | 43920.00 | ▇▇▇▇▇ |
| precip        |         0 |             1 |    12.88 |     5.90 |    0.79 |     9.05 |    11.49 |    15.14 |   107.88 | ▇▁▁▁▁ |

## stations

The `stations` dataset contains information about the stations at which
the observations from `precip` are recorded.

``` r
skimr::skim(stations)
```

|                                                  |          |
|:-------------------------------------------------|:---------|
| Name                                             | stations |
| Number of rows                                   | 43920    |
| Number of columns                                | 5        |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |          |
| Column type frequency:                           |          |
| numeric                                          | 5        |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |          |
| Group variables                                  | None     |

Data summary

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |     mean |       sd |     p0 |      p25 |      p50 |      p75 |     p100 | hist  |
|:--------------|----------:|--------------:|---------:|---------:|-------:|---------:|---------:|---------:|---------:|:------|
| station       |         0 |             1 | 21960.50 | 12678.76 |   1.00 | 10980.75 | 21960.50 | 32940.25 | 43920.00 | ▇▇▇▇▇ |
| proj_x        |         0 |             1 |    90.50 |    51.96 |   1.00 |    45.75 |    90.50 |   135.25 |   180.00 | ▇▇▇▇▇ |
| proj_y        |         0 |             1 |   122.50 |    70.44 |   1.00 |    61.75 |   122.50 |   183.25 |   244.00 | ▇▇▇▇▇ |
| latitude      |         0 |             1 |    54.98 |     3.16 |  49.31 |    52.25 |    54.98 |    57.71 |    60.53 | ▇▇▇▇▇ |
| longitude     |         0 |             1 |    -4.36 |     4.10 | -12.77 |    -7.86 |    -4.35 |    -0.82 |     3.41 | ▅▇▇▇▆ |

## station_estimates

The `station_estimates` dataset contains parameter estimates and Hessian
matrices from generalized maximum likelihood estimation of GEV models
with trend at each of the locations. The results are stored in
list-columns which need to be unnested for analysis. Inside the list
columns the results are stored in long format.

``` r
station_estimates |> 
  dplyr::select(par) |> 
  tidyr::unnest(par) |> 
  dplyr::group_by(name) |> 
  skimr::skim()
```

|                                                  |                    |
|:-------------------------------------------------|:-------------------|
| Name                                             | dplyr::group_by(…) |
| Number of rows                                   | 175680             |
| Number of columns                                | 2                  |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                    |
| Column type frequency:                           |                    |
| numeric                                          | 1                  |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                    |
| Group variables                                  | name               |

Data summary

**Variable type: numeric**

| skim_variable | name  | n_missing | complete_rate |  mean |   sd |    p0 |   p25 |   p50 |   p75 |  p100 | hist  |
|:--------------|:------|----------:|--------------:|------:|-----:|------:|------:|------:|------:|------:|:------|
| value         | psi   |         0 |             1 |  2.26 | 0.15 |  1.80 |  2.14 |  2.27 |  2.38 |  2.82 | ▁▆▇▃▁ |
| value         | tau   |         0 |             1 | -0.99 | 0.13 | -1.63 | -1.08 | -0.99 | -0.90 | -0.52 | ▁▁▇▇▁ |
| value         | phi   |         0 |             1 |  0.03 | 0.09 | -0.47 | -0.02 |  0.04 |  0.09 |  0.37 | ▁▁▇▇▁ |
| value         | gamma |         0 |             1 |  0.00 | 0.00 |  0.00 |  0.00 |  0.00 |  0.00 |  0.01 | ▁▅▇▂▁ |

``` r
station_estimates |> 
  dplyr::select(hess) |> 
  tidyr::unnest(hess) |> 
  dplyr::group_by(name1, name2) |> 
  skimr::skim()
```

|                                                  |                    |
|:-------------------------------------------------|:-------------------|
| Name                                             | dplyr::group_by(…) |
| Number of rows                                   | 702720             |
| Number of columns                                | 3                  |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                    |
| Column type frequency:                           |                    |
| numeric                                          | 1                  |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                    |
| Group variables                                  | name1, name2       |

Data summary

**Variable type: numeric**

| skim_variable | name1 | name2 | n_missing | complete_rate |       mean |        sd |        p0 |        p25 |        p50 |        p75 |       p100 | hist  |
|:--------------|:------|:------|----------:|--------------:|-----------:|----------:|----------:|-----------:|-----------:|-----------:|-----------:|:------|
| value         | psi   | psi   |         0 |             1 |     525.61 |    158.38 |    164.76 |     411.31 |     500.66 |     609.24 |    2055.00 | ▇▅▁▁▁ |
| value         | psi   | tau   |         0 |             1 |      23.75 |     44.19 |   -235.65 |      -1.95 |      24.97 |      49.89 |     304.88 | ▁▂▇▁▁ |
| value         | psi   | phi   |         0 |             1 |      92.75 |     19.80 |     38.90 |      78.71 |      90.85 |     104.64 |     272.54 | ▆▇▁▁▁ |
| value         | psi   | gamma |         0 |             1 |   20771.41 |   7006.04 |   6549.91 |   15911.09 |   19460.66 |   24063.30 |   87420.99 | ▇▃▁▁▁ |
| value         | tau   | psi   |         0 |             1 |      23.75 |     44.19 |   -235.65 |      -1.95 |      24.97 |      49.89 |     304.88 | ▁▂▇▁▁ |
| value         | tau   | tau   |         0 |             1 |     117.66 |      7.28 |     91.66 |     112.69 |     117.21 |     122.03 |     173.72 | ▁▇▂▁▁ |
| value         | tau   | phi   |         0 |             1 |       3.98 |     16.21 |    -89.96 |      -5.87 |       5.20 |      15.28 |      79.58 | ▁▁▇▅▁ |
| value         | tau   | gamma |         0 |             1 |   -4289.97 |   2078.75 | -19160.63 |   -5464.97 |   -4146.20 |   -2951.75 |    6433.63 | ▁▁▇▆▁ |
| value         | phi   | psi   |         0 |             1 |      92.75 |     19.80 |     38.90 |      78.71 |      90.85 |     104.64 |     272.54 | ▆▇▁▁▁ |
| value         | phi   | tau   |         0 |             1 |       3.98 |     16.21 |    -89.96 |      -5.87 |       5.20 |      15.28 |      79.58 | ▁▁▇▅▁ |
| value         | phi   | phi   |         0 |             1 |     221.71 |     63.53 |     59.76 |     176.47 |     210.82 |     254.49 |     705.14 | ▅▇▁▁▁ |
| value         | phi   | gamma |         0 |             1 |    5605.60 |   1820.66 |    541.58 |    4276.29 |    5412.46 |    6704.33 |   17141.02 | ▂▇▂▁▁ |
| value         | gamma | psi   |         0 |             1 |   20771.41 |   7006.04 |   6549.91 |   15911.09 |   19460.66 |   24063.30 |   87420.99 | ▇▃▁▁▁ |
| value         | gamma | tau   |         0 |             1 |   -4289.97 |   2078.75 | -19160.63 |   -5464.97 |   -4146.20 |   -2951.75 |    6433.63 | ▁▁▇▆▁ |
| value         | gamma | phi   |         0 |             1 |    5605.60 |   1820.66 |    541.58 |    4276.29 |    5412.46 |    6704.33 |   17141.02 | ▂▇▂▁▁ |
| value         | gamma | gamma |         0 |             1 | 1471151.77 | 532303.18 | 349192.83 | 1115369.11 | 1377977.19 | 1713948.25 | 7597034.07 | ▇▂▁▁▁ |

## other data

There are other objects stored as data in `bggjphd`, but these are
mostly kept there for use with the `ms_smooth()` function, and are
binary versions of sparse `Matrix::` matrices.
