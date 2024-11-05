# compareCoefs

## Overview  
The compareCoefs package provides a straightforward tool for comparing the coefficients of a single variable across different models. This can be particularly useful for applications such as mediation analysis, where you want to observe if introducing a mediator variable significantly reduces the coefficient of an independent variable. Additionally, it can assist in cases where certain models do not support interaction effects, like those created with the did package. In these scenarios, you can run separate models for different groups and then use compareCoefs to statistically compare the coefficients.  

This function is intended to compare models with the same dependent variable; if you are comparing coefficients from models with different dependent variables, ensure that the coefficients are standardized for meaningful interpretation.  

The method for testing the equality of regression coefficients implemented in compareCoefs is based on the formula presented in Paternoster et al. (1998)  

## Installation 
```r
# install.packages("devtools")  
devtools::install_github("noahhoop/compareCoefs")  
```

## Example
Here’s a quick example of how to use compareCoefs:
```r
m1 <- lm(mpg ~ hp, mtcars)
m2 <- lm(mpg ~ hp + wt, mtcars)

compare_coefs(m1, m2, "hp")
#> 
#> Difference                     z                     p 
#> "-0.0364553310894027"   "-2.68799042649229"         "0.007188346" 
```
References  

Paternoster, Raymond, Robert Brame, Paul Mazerolle, and Alex Piquero. 1998. "Using the Correct Statistical Test for the Equality of Regression Coefficients." Criminology 36(4): 859–66. doi:10.1111/j.1745-9125.1998.tb01268.x.

