BSDA
========

### Version 1.02

[![Travis-CI Build Status](https://travis-ci.org/alanarnholt/BSDA.svg?branch=master)](https://travis-ci.org/alanarnholt/BSDA)
[![Downloads from the RStudio CRAN mirror](http://cranlogs.r-pkg.org/badges/BSDA)](http://cran.rstudio.com/package=BSDA)


### Alan T. Arnholt

**BSDA**: Functions and data sets for the text *Basic Statistics and Data Analysis*

Please report any **bugs** or **suggestions** at:
<https://github.com/alanarnholt/BSDA/issues>.

### Installation

The package is available for download from
[CRAN](http://cran.r-project.org/web/packages/BSDA/).

You may download the most recent version using the [devtools](http://github.com/hdaley/devtools) function `install_github()` to install **BSDA** in R.

However, you need to make sure you're set up to develop packages. This is platform specific:

* On Windows, download and install [Rtools](http://http://cran.r-project.org/bin/windows/Rtools/).
* On the Mac, make sure you have [Xcode](https://developer.apple.com/xcode/) installed.
* On Linux, make sure you have the R-dev packages installed.

You can check everything is installed correctly with the `has_devel()` function from the **devtools** package. Type the following at 
the **R** prompt:


```r
install.packages("devtools", dependencies = TRUE)    
devtools::has_devel()
```

If everything is installed correctly, the function will print some output and then return **TRUE**.

To install the **BSDA** package, type the following at the **R** prompt:


```r
devtools::install_github('alanarnholt/BSDA')
```
    
It is possible to install **BSDA** with [GIT](http://git-scm.com/) and the **R CMD build** assuming you have GIT installed and the appropriate tools to build **R** from source.

```bash
git clone https://github.com/alanarnholt/BSDA.git
R CMD build BSDA
R CMD INSTALL BSDA_*.tar.gz
```

