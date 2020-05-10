# asmcjr <img src="https://quantoid.net/files/images/booksticker.png" width="140" align="right" /> <br /> 


This package supports the book ["Analyzing Spatial Models of Choice and Judgment with R" ](https://www.crcpress.com/Analyzing-Spatial-Models-of-Choice-and-Judgment-with-R/Armstrong-II-Bakker-Carroll-Hare-Poole-Rosenthal/p/book/9781466517158).  In its second edition, much of the R code has been streamlined.   This package contains all of the data and functions to replicate the analyses in the book. 

You can install using the `install_github()` function from the `devtools` package.  The package requires compilation, so Windows users will have to install [Rtools](https://cran.r-project.org/bin/windows/Rtools/) first.  For Mac users, you need to  make sure you have already installed latest [gfortran 8.2 for Mojave (macOS 10.14)](https://github.com/fxcoudert/gfortran-for-macOS/releases)) and [Xcode Developer Tools](https://developer.apple.com/support/xcode/).



Therefore to install __asmcjr__, the __devtools__ package must also be installed beforehand. 
```r
install.packages("devtools", dependencies=TRUE)
library(devtools)

devtools::install_github("davidaarmstrong/asmcjr")
library(ooc)
```


