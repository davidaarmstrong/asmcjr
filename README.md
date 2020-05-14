
# asmcjr <img src="https://quantoid.net/files/images/booksticker.png" width="140" align="right" /> <br /> 

[![Build Status](https://travis-ci.com/yl17124/asmcjr.svg?branch=master)](https://travis-ci.com/yl17124/asmcjr)
![R-CMD-check](https://github.com/yl17124/asmcjr/workflows/R-CMD-check/badge.svg?branch=master&event=check_run)

This package supports the book ["Analyzing Spatial Models of Choice and Judgment with R"](https://www.crcpress.com/Analyzing-Spatial-Models-of-Choice-and-Judgment-with-R/Armstrong-II-Bakker-Carroll-Hare-Poole-Rosenthal/p/book/9781466517158).  In its second edition, much of the R code has been streamlined. This package contains all of the data and functions to replicate the analyses in the book. 

You can install using the `install_github()` function from the `devtools` package.  The package requires compilation, so Windows users will have to install [Rtools](https://cran.r-project.org/bin/windows/Rtools/) first.  

For Mac users, you need to  make sure you have already installed latest [GNU Fortran(gfortran 8.2)](https://github.com/fxcoudert/gfortran-for-macOS/releases) and [Xcode Developer Tools](https://developer.apple.com/support/xcode/). In order to use __rjags__ for the Bayesian framework analysis, you need to install [JAGS](https://sourceforge.net/projects/mcmc-jags/files/JAGS/) in advance. Please make sure you have installed [Clang (clang-8.0.0.pkg)](https://cran.r-project.org/bin/macosx/tools/) if you have not updated R to 4.0.0 version. Therefore to install __asmcjr__, the __devtools__ package must also be installed in R beforehand. 

## Installation 
```r
install.packages("devtools", dependencies=TRUE)
library(devtools)

devtools::install_github("yl17124/asmcjr")
library(asmcjr)
```



## Example 1: Running Bayesian Aldrich-Mckelvey Scaling on the French module of the 2009 European Election Study (EES)
```r
library(asmcjr)
library(ggplot2)
data(franceEES2009)
head(franceEES2009, n = 10)
```

<p align="center">
  <img src="https://github.com/yl17124/figures/blob/master/first_example_df1.png">
</p>

```r
example_result_france <- aldmck(franceEES2009, respondent=1, 
                                polarity=2,missing=c(77,88,89), verbose=FALSE)
                                
str(example_result_france)                        
```
<p align="left">
  <img width="650" height="270" src="https://github.com/yl17124/figures/blob/master/first_example_df2.png">
</p>

```r
example_result_graph <- ggplot.resphist(example_result_france, addStim=TRUE, weights="negative", xlab = "Left-Right") +
    theme(legend.position="bottom", aspect.ratio=1) +
    guides(shape = guide_legend(override.aes = list(size = 4),nrow=3)) +
    labs(shape="Party", colour="Party")
print(example_result_graph)
```

<p align="center">
  <img width="500" height="500" src="https://github.com/yl17124/figures/blob/master/first_example_plot.png">
</p>


## Example 2: Running W-NOMINATE Scaling on Taiwan Legislative Roll Calls 
```r
library(wnominate)
library(tidyverse)
library(pscl)
library(ggpubr)
legis_7th_Taiwan <- read_csv("https://raw.githubusercontent.com/yl17124/figures/master/legis_7th_Taiwan.csv")
head(legis_7th_Taiwan, n =10)
```

<p align="center">
  <img src="https://github.com/yl17124/figures/blob/master/second_example_df1.png">
</p>

```r
pscl_df <- rollcall(legis_7th_Taiwan[3:ncol(legis_7th_Taiwan)],
                    yea = 1 , nay = 2, notInLegis = c(3,4,5), legis.names = legis_7th_Taiwan$legis.names,
                    vote.names = colnames(legis_7th_Taiwan[3:ncol(legis_7th_Taiwan)]),
                    desc="The 7th Taiwan Legislative Roll Call")

summary(pscl_df, verbose=FALSE)      
```

<p align="left">
  <img width="520" height="395"  src="https://github.com/yl17124/figures/blob/master/second_example_pscl.png">
</p>


```r
example_result_graph2 <- rownames_to_column(result$legislators, "legis.names") %>%
  left_join(legis_7th_Taiwan[c("legis.names", "party")], by = "legis.names") %>%
  mutate(coord2D.WEIGHT = coord2D*(result$weights[2])/(result$weights[1])) %>%
  ggscatter(x = "coord1D", y = "coord2D.WEIGHT",
            palette = "jco", shape = "party", point = FALSE,
            ellipse = TRUE, ellipse.type = "confidence", size = 4) +
  scale_y_continuous(limits = c(-0.5, 0.5) ) +
  scale_x_continuous(limits = c(-1, 1)) +
  geom_text(aes(label = factor(party), colour = party), size = 3,
            family="Helvetica", face = "bold", show.legend = FALSE, vjust = -0.5) +
  scale_colour_manual(values = c("#0000ff","#008964","#8b0017"), breaks = c("K", "D","N"))  +
  theme(legend.position = "none") +
  labs(y = "2nd Dimension", x = "1st Dimension") 

print(example_result_graph2)
```

<p align="center">
  <img width="540" height="390" src="https://github.com/yl17124/figures/blob/master/first_example_plot2.png">
</p>


## Reference
For citation from this book, run `citation("asmcjr")`. For more learning resources and those who want to learn how to use computational methods to understand the latent properties of your data, please join the course [Scaling Methods for Social Science](https://essexsummerschool.com/summer-school-facts/courses/2020-course-list/3l-ideal-point-estimation-item-response-theory-and-scaling-methods/) at [2020 ESSEX SUMMER SCHOOL](https://essexsummerschool.com/). 
