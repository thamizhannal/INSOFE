library(prophet)
library(dplyr)
path = "/home/tparamas/INSOFE_CPEE/project/BSE100/ITC.csv"
#df <- read.csv('/home/tparamas/INSOFE_CPEE/project/example_wp_peyton_manning.csv') %>%  mutate(y = log(y))
df <- read.csv(path) %>%  mutate(y = log(y))
str(df)
View(df)
#
df=stock_data
colnames(df) = c("ds","y")
m <- prophet(df)

future <- make_future_dataframe(m, periods = 7)
tail(future)
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m, forecast)

prophet_plot_components(m, forecast)
