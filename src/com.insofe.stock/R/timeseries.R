# loading libraries
library(ggplot2)
library(ggfortify) # Extended ggplot2

# print all the plots in pdf file on script name
draw_all_plots = function(stock_data, stock_ts, ts_decom,
                          stock_ts_diff1, arima_model, forecast_arima,
                          auto_arima_model,forecast_auto_arima,
                          hw_mdl, forecast_hw,
                          nnet_model,nnet_forecast,
                          script_name,h,script_out_path) {
  title = paste("Last 5 Year ",script_name,"Stock Price history")
  plot_ts = autoplot(object=ts(stock_data$adjclose, frequency = 365,
                     start=c(2012,1)),lab="Year", ylab="Price", main=title)

  title = paste(script_name," Stock Trend")
  decom_trend = autoplot(object=ts_decom$trend ,lab="Year", ylab="Price", main=title)

  title = paste(script_name," Stock Seasonal")
  decom_sea = autoplot(object=ts_decom$seasonal ,lab="Year", ylab="Price", main=title)

  title = paste(script_name," Stock Random Noise")
  decom_rnd = autoplot(ts_decom$random,xlab="Year", ylab="Price",title)

  # To apply acf and pacf
  title =  paste(script_name," Stock Auto Correlation ")
  plot_acf=autoplot.acf(acf(stock_ts, lag.max=12), main=title)
  title =  paste(script_name," Stock Partial Auto Correlation ")
  plot_pacf=autoplot.acf( pacf(stock_ts, lag.max=12), main=title)

  title = paste("After differencing, ", script_name," Stock Price")
  plot.ts(stock_ts_diff1)
  acf(stock_ts_diff1, lag.max=12)
  pacf(stock_ts_diff1, lag.max=12)

  title = paste(script_name," Stock Price Forecasts using Arima ", h ," days")
  plot_forecast=autoplot.forecast(forecast_arima, xlab="Year", ylab="Price", main=title)
  title = paste(script_name," Stock Price Forecasts using Auto Arima ", h ," days")
  plot_auto_arima_forecast=autoplot.forecast(forecast_auto_arima, xlab="Year", ylab="Price", main=title)
  title = paste(script_name," Stock Price Forecasts using Holt-Winter ", h ," days")
  plot_hw_forecast=autoplot.forecast(forecast_hw, xlab="Year", ylab="Price", main=title)
  title = paste(script_name," Stock Price Forecasts using Neural Network ", h ," days")
  plot_nnet_forecast=autoplot.forecast(nnet_forecast, xlab="Year", ylab="Price", main=title)

  pdf(script_out_path)
  print(plot_ts)
  print(decom_trend)
  print(decom_sea)
  print(decom_rnd)
  print(plot_acf)
  print(plot_pacf)

  plot.ts(stock_ts_diff1)
  acf(stock_ts_diff1, lag.max=12)
  pacf(stock_ts_diff1, lag.max=12)

  tsdisplay(residuals(arima_model))
  print(plot_forecast)

  print(plot_auto_arima_forecast)
  tsdisplay(residuals(auto_arima_model))
  print(plot_hw_forecast)
  tsdisplay(residuals(hw_mdl))
  print(plot_nnet_forecast)
  tsdisplay(residuals(nnet_model))
  dev.off()
}

# converting numerical values into time series format.
stock_data_ts = function(stock_data, freq=365, script_name) {
  stock_ts = ts(stock_data$adjclose, frequency = freq, start=c(2012,1))
  #Plotting
  #plot.ts(stock_ts)
  title = paste("Last 5 Year ",script_name,"Stock Price")
  #auto_plot(stock_ts,xlab="Year", ylab="Price",title)
  #autoplot(object=ts(stock_data$adjclose, frequency = 365,
  #                   start=c(2012,1)),lab="Year", ylab="Price", main=title)
  stock_ts
}

# auto_plot = function(ts,xlab=xlab, ylab=ylab,title) {
#   autoplot( object = ts,
#     #object=ts(ts, frequency = 365, start=c(2012,1)),
#            xlab=xlab, ylab=ylab , main=title)
#   print(title)
# }

# Decompose Time Series and plot trend, seasonal and random noise components
stock_ts_decom = function(stock_ts,ts_plot=FALSE, script_name) {
  ts_decom <- decompose(stock_ts)
  # if (ts_plot) {
  #   print("Ploting...")
  #
  #
  #   par(mfrow=c(4,1))
  #   #plot.ts(stock_ts)
  #   title = paste("Last 5 Year ",script_name,"Stock Price")
  #   autoplot(object=stock_ts ,lab="Year", ylab="Price", main=title)
  #
  #
  #
  #   #plot.ts(stocks_ts_decom$seasonal)
  #   #plot.ts(stocks_ts_decom$random)
  # }
  ts_decom
}

plot_acf_pacf = function (stock_ts, lag=12, script_name) {
  # To apply acf and pacf
  par(mfrow=c(1,3))
  plot.ts(stock_ts)
  acf(stock_ts, lag.max=lag)
  pacf(stock_ts, lag.max=lag)
}
stock_ts_diff = function(stock_ts, diff=1,plot=FALSE, script_name) {
  ts_diff <- diff(stock_ts, differences=diff)
  #  normal diff,acf,pacf
  if (plot) {
    par(mfrow=c(3,1))
    plot.ts(stock_ts)
    plot.ts(ts_diff)
  }
  ts_diff
}

run_manual_arima = function (stock_ts, order=c(0,0,0), seasonal=c(0,0,0)) {
  arima_model <- Arima(stock_ts,order=order, seasonal=seasonal)
  arima_model
}

run_auto_arima = function(stock_ts) {
  auto_arima_model <- auto.arima(stock_ts,ic='aic')
  auto_arima_model
}

model_forecast = function(object,h = 365 , plot=TRUE) {
  ts_forecast = forecast(object = object, h=h)
  if (plot)
    plot(ts_forecast)
  ts_forecast
}

display_residual = function(model) {
  tsdisplay(residuals(model))
}
box_test = function(model, lag=16, fitdf=4, type="Ljung") {
  box_test = Box.test(residuals(model), lag=lag, fitdf=fitdf, type=type)
  box_test
}

run_box_test = function(model) {
  p_val = 100
  lag = 1
  pvalues = array()
  for(j in 1:min(20)) {
    pvalues[j] <- Box.test(residuals(model), type="Ljung-Box", lag=j)$p.value
    val = as.numeric(pvalues[j])
    if ( (val < p_val) && (val > 0))  {
      p_val = val
      lag = j
    }
  }
  return (list("p"= p_val,"lag"=lag))
}


eval_results = function(arima_model,forecast_arima,stock_test_data,row_idx=1) {
  # plot residuals - TODO
  display_residual(arima_model)

  # Run Box-Test and Identify lag for small p-value
  res_box_test = run_box_test(arima_model)
  sprintf("Box-Test Result: small p-value:%f found at lag=%d",res_box_test$p,res_box_test$lag)

  # Run box-Ljunk test
  #run_box_test(arima_model, lag = lag)
  acc = accuracy(forecast_arima, stock_test_data$adjclose)
  print(acc)
  aic = arima_model$aic
  res=arima_model$residuals
  p_value = res_box_test$p
  sprintf("AIC=%f ",aic)
  print(aic)
  #row_idx = row_idx+1
  result$TrainMAPE[row_idx] = acc[1,5]
  result$TestMAPE[row_idx] = acc[2,5]
  result$AIC[row_idx] = aic
  result$Pvalue[row_idx] = res_box_test$p
  result$lag[row_idx] = res_box_test$lag
  result
}
