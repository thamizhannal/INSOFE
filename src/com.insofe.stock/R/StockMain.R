############################  MAIN PROGRAM ###########################

# Loading Library
library(fpp)
# TODO: change it to absolute path of com.insofe.stock/R
path = "/home/tparamas/INSOFE_CPEE/project/source_code/com.insofe.stock/R"
setwd(path)

# Include source from R files
source("preprocess.R")
source("timeseries.R")


src_path = paste(path,"data",sep="/")

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

invest_period = 5
idx = 1
#for(script in script_list) {
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
  idx = idx + 1

  proc_stock_data = stock_data
  
  #. 2. Get TS data format
  stock_ts = stock_ts(proc_stock_data, 365)
  stock_ts_decom(stock_ts, TRUE)
  plot_acf_pacf(stock_ts)
  ndiff_valaue= ndiffs(stock_ts)
  stock_ts_diff1= stock_ts_diff(stock_ts, diff=ndiff_valaue, plot=TRUE)
  plot_acf_pacf(stock_ts_diff1)
  ######################## RUN MODELS #########################################################
  ######################## 1. Run manual arima model ##########################################
  arima_model = run_manual_arima(stock_ts, order = c(0,0,0), seasonal = c(0,ndiff_valaue,0) )
  forecast_arima = model_forecast(object = arima_model,h=5)
  forecast_arima
  # plot residuals
  display_residual(arima_model)
  
  # Run Box-Test and Identify lag for small p-value 
  res_box_test = run_box_test(arima_model)
  sprintf("Box-Test Result: small p-value:%f found at lag=%d",res_box_test$p,res_box_test$lag)
  
  # Run box-Ljunk test
  #run_box_test(arima_model, lag = lag)
  stock_test_data = proc_stock_test_data(script_path,from =as.Date("2017/01/02"),to=as.Date("2017/01/06"))
  acc = accuracy(forecast_arima, stock_test_data$adjclose)
  
  aic = arima_model$aic
  res=arima_model$residuals
  p_value = res_box_test$p
  

  ######################### 2. Run Auto.Arima Model ##############################################
  auto_arima_model = run_auto_arima(stock_ts)
  forecast_arima = model_forecast(object = auto_arima_model,h=5)
  forecast_arima
  # plot residuals
  display_residual(auto_arima_model)
  # Run box-Ljunk test
  run_box_test(auto_arima_model)
  accuracy(auto_arima_model)
  ######################## 3. Holt-Winter model ##################################################
  
  stock_ts_freq7 = stock_ts(proc_stock_data, freq=7 )
  hw_mdl = hw(stock_ts_freq7,  seasonal = "additive")
  summary(hw_mdl)
  
  forecast_arima = model_forecast(object = hw_mdl,h=7)
  forecast_arima
  # plot residuals
  display_residual(hw_mdl)
  # Run box-Ljunk test
  box_test=run_box_test(hw_mdl)
  box_test
  ##############################################################################################
  
#}

#csv_file =load_csv(path)
#stock_data = extract_stock_adjclose_sorted(csv_file)
#imputed_stock_data = impute_stock_data(stock_data)


