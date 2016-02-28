library(datasets)
library(ggplot2)


covariate_comparison <- function(){
  a <- summary(lm(mpg ~ am, data=mtcars))$coef[2]
  
  rbind(
    c('baseline', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec', 'vs', 'gear', 'carb'),
    round(c((a-a)/a,
            100 * (a - summary(lm(mpg ~ am + cyl, data=mtcars))$coef[2])/a,
            100 * (a - summary(lm(mpg ~ am + disp, data=mtcars))$coef[2])/a,
            100 * (a - summary(lm(mpg ~ am + hp, data=mtcars))$coef[2])/a,
            100 * (a - summary(lm(mpg ~ am + drat, data=mtcars))$coef[2])/a,
            100 * (a - summary(lm(mpg ~ am + wt, data=mtcars))$coef[2])/a,
            100 * (a - summary(lm(mpg ~ am + qsec, data=mtcars))$coef[2])/a,
            100 * (a - summary(lm(mpg ~ am + vs, data=mtcars))$coef[2])/a,
            100 * (a - summary(lm(mpg ~ am + gear, data=mtcars))$coef[2])/a,
            100 * (a - summary(lm(mpg ~ am + carb, data=mtcars))$coef[2])/a
    ), digits=2)
  )
}


mtcars_model_selection <- function(){
  # variables from covariate_comparison > 25%
  fit1 <- lm(mpg ~ am, data=mtcars)
  fit2 <- lm(mpg ~ am + cyl, data=mtcars)
  fit3 <- lm(mpg ~ am + cyl + disp, data=mtcars)
  fit4 <- lm(mpg ~ am + cyl + disp + hp, data=mtcars)
  fit5 <- lm(mpg ~ am + cyl + disp + hp + drat, data=mtcars)
  fit6 <- lm(mpg ~ am + cyl + disp + hp + drat + wt, data=mtcars)
  anova(fit1, fit2, fit3, fit4, fit5, fit6)
}

mtcars_final_model <- function(){
  fit1 <- lm(mpg ~ am, data=mtcars)
  fit7 <- lm(mpg ~ am + cyl + hp + wt, data=mtcars)
  anova(fit1, fit7)
}

resid_plot_linear <- function(){
  data(mtcars)
  # alternatively: http://rstudio-pubs-static.s3.amazonaws.com/22370_39a6a8e6f4b447239dc6d363b9c99e0d.html
  # mtcars$am <- as.factor(mtcars$am)
  # levels(mtcars$am) <- c("Automatic", "Manual")
  mtcars <- mutate(mtcars, am=factor(ifelse(am==1, 'automatic', 'manual')))
  fit_lm <- lm(mpg ~ am + cyl + hp + wt, data=mtcars)
  df <- data.frame(e=resid(fit_lm), yhat = predict(fit_lm))
  g <- ggplot(df, aes(yhat, e), title="foo") + 
    geom_point() + 
    geom_smooth(method="loess") +
    ggtitle("Residuals Vs. Fitted (linear model)") +
    xlab("Fitted Miles per Gallon (mpg)") +
    ylab("Residuals (mpg)")
  
  print(g)
  fit_lm
}



