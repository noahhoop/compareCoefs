# compareCoefs

Overview  
The compareCoefs package provides a straightforward tool for comparing the coefficients of a single variable across different models. This can be particularly useful for applications such as mediation analysis, where you want to observe if introducing a mediator variable significantly reduces the coefficient of an independent variable. Additionally, it can assist in cases where certain models do not support interaction effects, like those created with the did package. In these scenarios, you can run separate models for different groups and then use compareCoefs to statistically compare the coefficients.  

This function is intended to compare models with the same dependent variable; if you are comparing coefficients from models with different dependent variables, ensure that the coefficients are standardized for meaningful interpretation.  

The method for testing the equality of regression coefficients implemented in compareCoefs is based on the formula presented in Paternoster et al. (1998)  

Installation  
# install.packages("devtools")  
devtools::install_github("yourusername/compareCoefs")  
Example
Here’s a quick example of how to use compareCoefs:

# Assume model1 and model2 are two fitted models with a common predictor variable  
result <- compareCoefs(model1, model2, "your_variable")  
print(result)  
# Output: A vector with the difference in coefficients, z-score, and p-value.  

References  

Paternoster, Raymond, Robert Brame, Paul Mazerolle, and Alex Piquero. 1998. "Using the Correct Statistical Test for the Equality of Regression Coefficients." Criminology 36(4): 859–66. doi:10.1111/j.1745-9125.1998.tb01268.x.

