############################  MAIN PROGRAM ###########################
rm(list=ls(all=TRUE))
# Loading Library
library(fpp)
# TODO: change it to absolute path of com.insofe.stock/R
#path = "/home/tparamas/INSOFE_CPEE/project/source_code/com.insofe.stock/R"
setwd(paste(getwd(),"R",sep="/"))
print(getwd)
# Include source from R files
source("preprocess.R")
source("timeseries.R")

src_path = paste(getwd(),"data",sep="/")

# Script return calculator
script_return = data.frame(matrix(c(1,2,3,4,5),nrow=5,ncol=5))
colnames(script_return) =  c("script", "buy_price","curr_price","invest_period(yrs)", "CARG")

# Script forecast calculator
script_forecast = data.frame(matrix(c(1,2,3,4,5),nrow=5,ncol=5))
colnames(script_return) =  c("script", "buy_price","curr_price","invest_period(yrs)", "CARG")


all_script_list = c("BAJFINANCE.csv", "CADILAHC.csv", "HDFC.csv", "IOC.csv", "LT.csv",
                    "RELIANCE.csv", "TATAMOTORS.csv", "TITAN.csv", "BHARATFORG.csv",
                    "HAVELLS.csv", " HINDZINC.csv", "ITC.csv", "MOTHERSUMI.csv", "SHREECEM.csv",
                    "TORNTPHARM.csv", "BRITANNIA.csv", "HDFCBANK.csv", "INFY.csv", "KOTAKBANK.csv",
                    "PIDILITIND.csv", "TCS.csv")

script_list = c("TCS.csv") #,"HDFC.csv", "HDFCBANK.csv","INFY.csv",
#"BRITANNIA.csv" )

# For Strong Model metrics
result = data.frame(matrix(c(0),nrow=3,ncol=5 ))
colnames(result) = c("TrainMAPE","TestMAPE", "AIC","Pvalue","lag")
rnames=c("Arima","AutoArima", "HoltWinter")
rownames(result) = rnames
row_idx=0

h=5
# For storing forecast metrics
res_forecast = data.frame(matrix(c(0),nrow=h,ncol=5))
colnames(res_forecast) = c("date", "actual", "arima", "auto_arima", "HoltWinter")

invest_period = 5
idx = 0
for(script in script_list) {

  idx = idx + 1

  script_path = paste(src_path,script,sep="/")

  # 1. Load and Preprocess Stock Data
  stock_data = proc_stock_data(script_path)
  buy_price = stock_data[stock_data$date==as.Date("2012-01-02"),]$adjclose
  print(buy_price)
  curr_price = stock_data[stock_data$date==as.Date("2016-12-31"),]$adjclose
  print(curr_price)
  CARG = ((curr_price/buy_price)^(1/invest_period)-1)*100
  script_name = strsplit(script,".csv")[[1]]
  script_return$script[idx] = script_name
  script_return$buy_price[idx] = buy_price
  script_return$curr_price[idx] = curr_price
  script_return$invest_period[idx] = invest_period
  script_return$CARG[idx] = CARG


  #//proc_stock_data = stock_data

  #. 2. Get TS data format
  stock_ts = stock_data_ts(stock_data, 365)
  stock_ts_decom(stock_ts, TRUE)
  plot_acf_pacf(stock_ts)
  #  ndiffs() -Arima-order diff
  # To determine the appropriate number of first differences required for a non-seasonal TS.
  ndiff_valaue= ndiffs(stock_ts)

  # Arima - seasonal diff component.
  # determining whether seasonal differencing is required is nsdiffs()
  nsdiff = nsdiffs(stock_ts)

  stock_ts_diff1= stock_ts_diff(stock_ts, diff=ndiff_valaue, plot=TRUE)
  plot_acf_pacf(stock_ts_diff1)

  ############## Prepare Test Data ####################
  stock_test_data = proc_stock_test_data(script_path,
                                         from =as.Date("2017/01/02"),
                                         to=as.Date("2017/01/06"))
  res_forecast$actual = stock_test_data$adjclose
  res_forecast$date = stock_test_data$date

  ######################## RUN MODELS #########################################################
  ######################## 1. Run manual arima model ##########################################
  arima_model = run_manual_arima(stock_ts,
                                 order = c(0,ndiff_valaue,0),
                                 seasonal = c(0,nsdiff,0) )
  arima_model
  forecast_arima = model_forecast(object = arima_model,h=h)
  forecast_arima
  res_forecast$arima = forecast_arima$mean#$upper[,2]

  # plot residuals - TODO
  display_residual(arima_model)
  # Run Box-Test and Identify lag for small p-value
  res_box_test = run_box_test(arima_model)
  sprintf("Box-Test Result: small p-value:%f found at lag=%d",res_box_test$p,res_box_test$lag)
  acc = accuracy(forecast_arima, stock_test_data$adjclose)
  #print(acc)
  #aic = arima_model$aic
  #res=arima_model$residuals
  #p_value = res_box_test$p
  #sprintf("AIC=%f ",aic)
  #print(aic)
  row_idx = idx
  result$TrainMAPE[row_idx] = acc[1,5]
  result$TestMAPE[row_idx] = acc[2,5]
  result$AIC[row_idx] = arima_model$aic
  result$Pvalue[row_idx] = res_box_test$p
  result$lag[row_idx] = res_box_test$lag

  ## TODO:To check residuals are normally distributed


  #eval_results(arima_model,forecast_arima,stock_test_data,row_idx=1)


  #############
  auto_arima_model = run_auto_arima(stock_ts)
  forecast_auto_arima = model_forecast(object = auto_arima_model,h=h)
  res_forecast$auto_arima = forecast_auto_arima$mean #upper[,2]

  # plot residuals - TODO
  display_residual(forecast_auto_arima)
  # Run Box-Test and Identify lag for small p-value
  res_box_test = run_box_test(forecast_auto_arima)
  sprintf("Box-Test Result: small p-value:%f found at lag=%d",res_box_test$p,res_box_test$lag)
  acc = accuracy(forecast_auto_arima, stock_test_data$adjclose)

  row_idx = idx + 1
  result$TrainMAPE[row_idx] = acc[1,5]
  result$TestMAPE[row_idx] = acc[2,5]
  result$AIC[row_idx] = auto_arima_model$aic
  result$Pvalue[row_idx] = res_box_test$p
  result$lag[row_idx] = res_box_test$lag

  #eval_results(auto_arima_model,forecast_arima,stock_test_data,row_idx=2)

  stock_ts_freq7 = stock_data_ts(stock_data, freq=7 )
  hw_mdl = hw(stock_ts_freq7,  seasonal = "additive")
  summary(hw_mdl)
  forecast_hw = model_forecast(object = hw_mdl,h=h)
  forecast_hw
  res_forecast$HoltWinter = forecast_hw$mean#$upper[,2]
  # plot residuals - TODO
  display_residual(hw_mdl)
  # Run Box-Test and Identify lag for small p-value
  res_box_test = run_box_test(hw_mdl)
  sprintf("Box-Test Result: small p-value:%f found at lag=%d",res_box_test$p,res_box_test$lag)
  acc = accuracy(forecast_hw, stock_test_data$adjclose)


  row_idx = idx + 2
  result$TrainMAPE[row_idx] = acc[1,5]
  result$TestMAPE[row_idx] = acc[2,5]
  result$AIC[row_idx] = hw_mdl$
  result$Pvalue[row_idx] = res_box_test$p
  result$lag[row_idx] = res_box_test$lag
  #eval_results(hw_mdl,forecast_arima,stock_test_data,row_idx=2)

}

#####
print("script_return")
print(script_return)
print("Model Metrics")
print(result)
print("Forecast Metrics")
print(res_forecast)
