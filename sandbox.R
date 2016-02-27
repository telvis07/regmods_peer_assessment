library(datasets)
library(ggplot2)

resid_plot_linear <- function(){
  data(mtcars)
  mtcars <- mutate(mtcars, am=factor(ifelse(am==1, 'automatic', 'manual')))
  fit_lm <- lm(log(mpg) ~ ., data=mtcars)
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

resid_plot_pois <- function(){
  data(mtcars)
  mtcars <- mutate(mtcars, am=factor(ifelse(am==1, 'automatic', 'manual')))
  fit_pois <- glm(formula= mpg ~ ., data=mtcars, family=poisson)
  
  df <- data.frame(e=resid(fit_pois), yhat = predict(fit_pois))
  
  g <- ggplot(df, aes(yhat, e), title="foo") + 
    geom_point() + 
    geom_smooth(method="loess") +
    ggtitle("Residuals Vs. Fitted (poisson model)") +
    xlab("Fitted Miles per Gallon (log(mpg))") +
    ylab("Residuals (log(mpg))")
  
  print(g)
  fit_pois
}

