#' Compare how a coefficient differs between models
#'
#' This function compares the coefficient of one variable in different models.
#' It can be used for mediation analysis to check if a mediator significantly reduces the coefficient of that variable.
#' Another use case is when the model doesn't allow a specification of an interaction effect like the did package.
#' Here you can run the model for different groups and compare the estimates.
#' The function should only be used for comparing models with the same dependent variable unless it is standardized.
#' @param The input are the two model objects and a string for the coefficients name. This should be the same as displayed in the model summary.
#' @return It returns a vector with the difference, z- and p-value.
#' @export

compare_coefs <- function(m1, m2, coef) {
  
  if (class(m1) == class(m2)) {
    
    model_class <- class(m1)
    
    # Use switch to handle different model types
    switch(model_class,
           "lm" = {
             
             m1 <- summary(m1)
             m2 <- summary(m2)
             # first extracting the estimates and standard errors
             b1 <- m1$coefficients[coef, "Estimate"]
             se1 <- m1$coefficients[coef, "Std. Error"]
             b2 <- m2$coefficients[coef, "Estimate"]
             se2 <- m2$coefficients[coef, "Std. Error"]
             # calculate the difference of estimates
             b = b1 - b2
             s1 = se1^2
             s2 = se2^2
             sc = s1 + s2
             # based on the formula of Paternoster et al. 1998
             z = b / sqrt(sc)
             p <- format(2*pnorm(-abs(z)), scientific=FALSE)
             result <- c(Difference = b, z = z, p = p)
             return(result) #returns the difference, z-value and p-value
           },
           "lm_robust" = {
             
             
             m1 <- summary(m1)
             m2 <- summary(m2)
             # first extracting the estimates and standard errors
             b1 <- m1$coefficients[coef, "Estimate"]
             se1 <- m1$coefficients[coef, "Std. Error"]
             b2 <- m2$coefficients[coef, "Estimate"]
             se2 <- m2$coefficients[coef, "Std. Error"]
             # calculate the difference of estimates
             b = b1 - b2
             s1 = se1^2
             s2 = se2^2
             sc = s1 + s2
             # based on the formula of Paternoster et al. 1998
             z = b / sqrt(sc)
             p <- format(2*pnorm(-abs(z)), scientific=FALSE)
             result <- c(Difference = b, z = z, p = p)
             return(result) #returns the difference, z-value and p-value
           },
           "fixest" = {
             b1 <- m1$coeftable[coef, "Estimate"]
             se1 <- m1$coeftable[coef, "Std. Error"]
             b2 <- m2$coeftable[coef, "Estimate"]
             se2 <- m2$coeftable[coef, "Std. Error"]
             # calculate the difference of estimates
             b = b1 - b2
             s1 = se1^2
             s2 = se2^2
             sc = s1 + s2
             # based on the formula of Paternoster et al. 1998
             z = b / sqrt(sc)
             p <- format(2*pnorm(-abs(z)), scientific=FALSE)
             result <- c(Difference = b, z = z, p = p)
             return(result) #returns the difference, z-value and p-value
           },
           "AGGTEobj" = {
             b1 <- m1$overall.att
             se1 <- m1$overall.se
             b2 <- m2$overall.att
             se2 <- m2$overall.se
             # calculate the difference of estimates
             b = b1 - b2
             s1 = se1^2
             s2 = se2^2
             sc = s1 + s2
             # based on the formula of Paternoster et al. 1998
             z = b / sqrt(sc)
             p <- format(2*pnorm(-abs(z)), scientific=FALSE)
             result <- c(Difference = b, z = z, p = p)
             return(result) #returns the difference, z-value and p-value
           },
           {
             # Default case if none of the specified classes match
             stop("Unknown model type. Please provide a model from `lm`, `lm_robust`, or `feols`.")
           }
    )
    
  } else {
    stop("Models are of different class. Use the same function for both models.")
  }
  
}


