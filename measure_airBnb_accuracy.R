library(data.table); library(dplyr); library(plotly)




# Reverse log transform 'log_price'predictor variable to measure accuracy in a relevant format
test_gbm_results$actual_rental_price <- exp(test_gbm_results$log_price)
test_gbm_results$predicted_rental_price <- exp(test_gbm_results$predict)

test_gbm_results$error <- test_gbm_results$predicted_rental_price - test_gbm_results$actual_rental_price


accuracy_summary <- setDT(test_gbm_results)[,
                                            list(
                                              avg_rental_price = round(mean(actual_rental_price)), # used as a reference point when comparing ME, MAE, and RMSE
                                              mean_error = round(mean(error)),
                                              mae = round(mean(abs(error))),
                                              rmse = round(sqrt(mean(error^2)))
                                            )]

# Identify 95% confidence intervals
ci<-function(x){
  df<-length(x)-1 # degrees of freedom
  t<-qt(0.975,df=df) # t-statistic
  m<-mean(x) # sample mean (x-bar)
  sd<-sd(x) # sigma
  n<-length(x) # number of observations
  se<-sd/sqrt(n) # standard error of the mean
  return(c(upper = m+t*se, lower = m-t*se))
}

confidenceInt <- ci(test_gbm_results$error)

# Visualize distribution of error with 95% CI ranges
f1 <- list(
  family="Arial, sans-serif",
  size=18,
  color="black")

fx1 <- list(
  family="Arial, sans-serif",
  titlefont = f1,
  rangemode="tozero",
  title = "Prediction Error ($)"
)

fy1 <- list(
  family="Arial, sans-serif",
  titlefont = f1,
  rangemode="tozero",
  title = "Volume"
)

plot_ly(test_gbm_results, x=~error, type="histogram") %>%
  layout(xaxis=fx1,
         yaxis=fy1) %>%
  add_segments(x=confidenceInt[2], xend=confidenceInt[2], y=0, yend=1400, name='2.5th Percentile') %>%
  add_segments(x=confidenceInt[1], xend=confidenceInt[1], y=0, yend=1400, name='97.5th Percentile')