[![R-CMD-check](https://github.com/KWB-R/kwb.misa/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/kwb.misa/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/kwb.misa/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/kwb.misa/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/kwb.misa/branch/main/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.misa)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.misa)]()
[![R-Universe_Status_Badge](https://kwb-r.r-universe.dev/badges/kwb.misa)](https://kwb-r.r-universe.dev/)

Assessment of oxygen course in rivers. Assessment is aimed at
reducing critical situations

## Installation

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'kwb.misa' from GitHub
remotes::install_github("KWB-R/kwb.misa")
```
