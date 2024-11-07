#' Compare how a coefficient differs between models
#'
#' This function compares the coefficient of one variable across two models.
#' It can be used for mediation analysis to check if a mediator significantly reduces the coefficient of that variable.
#' Another use case is when the model doesn't allow a specification of an interaction effect like the did package.
#' Here you can run the model for different groups and compare the estimates.
#' The function should only be used for comparing models with the same dependent variable unless it is standardized.
#'
#' It is compatible with lm(), lm_robust(), feols() and aggte().
#' The formula (based on Paternoster et al. (1998)) for the calculation is:
#' \deqn{Z = \frac{b_1 - b_2}{\sqrt{SE_{b_1}^2 + SE_{b_2}^2}}}
#'
#' @param m1 A model object. The first model to compare.
#' @param m2 A model object. The second model to compare.
#' @param coef A string. The name of the coefficient to compare, as it appears in the model summary.
#' @return A list with the difference in the coefficient, the z-value, and the p-value.
#' @references Paternoster, R., Brame, R., Mazerolle, P., & Piquero, A. (1998). Using the Correct Statistical Test for the Equality of Regression Coefficients. *Criminology, 36*(4), 859–866. https://doi.org/10.1111/j.1745-9125.1998.tb01268.x
#' @export

compare_coefs <- function(m1, m2, coef, ci.lvl = 0.95) {

  if (class(m1) == class(m2)) {

    model_class <- class(m1)

    # Use switch to handle different model types
    switch(model_class,
           "lm" = {

             if (coef %in% names(m1$coefficients) & coef %in% names(m2$coefficients)) {
               sm1 <- summary(m1)
               sm2 <- summary(m2)
               # first extracting the estimates and standard errors
               b1 <- sm1$coefficients[coef, "Estimate"]
               se1 <- sm1$coefficients[coef, "Std. Error"]
               b2 <- sm2$coefficients[coef, "Estimate"]
               se2 <- sm2$coefficients[coef, "Std. Error"]
               # calculate the difference of estimates
               b = b1 - b2
               s1 = se1^2
               s2 = se2^2
               sc = s1 + s2
               # based on the formula of Paternoster et al. 1998
               z = b / sqrt(sc)
               p <- format(2*pnorm(-abs(z)), scientific=FALSE)
               conf.low = b - qnorm(1 - (1 - ci.lvl) / 2)*sqrt(sc)
               conf.high = b + qnorm(1 - (1 - ci.lvl) / 2)*sqrt(sc)
               result <- list(Difference = b,
                              z.value = z,
                              p.value = p,
                              conf.low = conf.low,
                              conf.high = conf.high)
             } else {
               stop("The coefficient name is not presented in both models. Please check if the name is correct. For factor variables make sure that the name includes the variable and factor name as shown in the model summary.")
             }

           },
           "lm_robust" = {



             if (coef %in% names(m1$coefficients) & coef %in% names(m2$coefficients)) {

               sm1 <- summary(m1)
               sm2 <- summary(m2)
               # first extracting the estimates and standard errors
               b1 <- sm1$coefficients[coef, "Estimate"]
               se1 <- sm1$coefficients[coef, "Std. Error"]
               b2 <- sm2$coefficients[coef, "Estimate"]
               se2 <- sm2$coefficients[coef, "Std. Error"]
               # calculate the difference of estimates
               b = b1 - b2
               s1 = se1^2
               s2 = se2^2
               sc = s1 + s2
               # based on the formula of Paternoster et al. 1998
               z = b / sqrt(sc)
               p <- format(2*pnorm(-abs(z)), scientific=FALSE)
               conf.low = b - qnorm(1 - (1 - ci.lvl) / 2)*sqrt(sc)
               conf.high = b + qnorm(1 - (1 - ci.lvl) / 2)*sqrt(sc)
               result <- list(Difference = b,
                              z.value = z,
                              p.value = p,
                              conf.low = conf.low,
                              conf.high = conf.high)
             } else {
               stop("The coefficient name is not presented in both models. Please check if the name is correct. For factor variables make sure that the name includes the variable and factor name as shown in the model summary.")
             }

           },
           "fixest" = {
             if (coef %in% names(m1$coefficients) & coef %in% names(m2$coefficients)) {

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
               conf.low = b - qnorm(1 - (1 - ci.lvl) / 2)*sqrt(sc)
               conf.high = b + qnorm(1 - (1 - ci.lvl) / 2)*sqrt(sc)
               result <- list(Difference = b,
                              z.value = z,
                              p.value = p,
                              conf.low = conf.low,
                              conf.high = conf.high)
             } else {
               stop("The coefficient name is not presented in both models. Please check if the name is correct. For factor variables make sure that the name includes the variable and factor name as shown in the model summary.")
             }
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
             conf.low = b - qnorm(1 - (1 - ci.lvl) / 2)*sqrt(sc)
             conf.high = b + qnorm(1 - (1 - ci.lvl) / 2)*sqrt(sc)
             result <- list(Difference = b,
                            z.value = z,
                            p.value = p,
                            conf.low = conf.low,
                            conf.high = conf.high)
           },
           {
             # Default case if none of the specified classes match
             stop("Unknown model type. Please provide a model from `lm`, `lm_robust`, `feols` or `aggte`.")
           }
    )

    class(result) <- "coefComparison"
    return(result)

  } else {
    stop("Models are of different class. Use the same function for both models.")
  }

}
