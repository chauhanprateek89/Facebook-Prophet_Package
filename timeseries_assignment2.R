if(!require("wikipediatrend")) install.packages("wikipediatrend")
if(!require("prophet")) install.packages("prophet")
if(!require("dplyr")) install.packages("dplyr")
if(!require("ggplot2")) install.packages("ggplot2")



rpl <- wp_trend("R_(programming_language)", file="rpl.csv", from = "2008-01-30", to = "2017-04-30")
ggplot(rpl, aes(x=date, y=count, color=wp_year(date))) + 
  geom_line() + 
  stat_smooth(method = "lm", formula = y ~ poly(x, 22), color="#CD0000a0", size=1.2) +
  theme_bw()
head(rpl)

df <- read.csv('example_wp_R.csv')
plot(df)
# ggplot(df, aes(x=date, y=count, color=wp_year(as.Date(date)))) + 
#   geom_line() + 
#   stat_smooth(method = "lm", formula = y ~ poly(x, 22), color="#CD0000a0", size=1.2) +
#   theme_bw()
df$y <- log(df$y)
m <- prophet(df)
future <- make_future_dataframe(m, periods = 365)
tail(future)
forecast <- predict(m,future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m,forecast)
prophet_plot_components(m, forecast)




df <- read.csv('example_wp_R.csv')
df$y <- log(df$y)
df$cap <- 8.5
m <- prophet(df, growth = 'logistic')
future <- make_future_dataframe(m, periods = 1826)
future$cap <- 8.5
fcst <- predict(m, future)
plot(m, fcst)


#Outliers
#There are two main ways that outliers can affect Prophet forecasts. 
#Here we make a forecast on the logged Wikipedia visits to the R page from before, but with a block of bad data:

df <- read.csv('example_wp_R_outliers1.csv')
df$y <- log(df$y)
m <- prophet(df)
future <- make_future_dataframe(m, periods = 1096)
forecast <- predict(m, future)
plot(m, forecast)

#The trend forecast seems reasonable, but the uncertainty intervals seem way too wide. 
#Prophet is able to handle the outliers in the history, but only by fitting them with trend changes. 
#The uncertainty model then expects future trend changes of similar magnitude.
#The best way to handle outliers is to remove them - Prophet has no problem with missing data. 
#If you set their values to NA in the history but leave the dates in future, then Prophet will give you 
#a prediction for their values.


outliers <- (as.Date(df$ds) > as.Date('2010-01-01')
             & as.Date(df$ds) < as.Date('2011-01-01'))
df$y[outliers] = NA
m <- prophet(df)
forecast <- predict(m, future)
plot(m, forecast)
