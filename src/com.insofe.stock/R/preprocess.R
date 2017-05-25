# Include library
library(zoo) # imputation - last observed carry forward.
library(dplyr)

# Loading CSV File
load_csv = function(script_path) {
  # 1. Read the data into R
  raw_stock_data = read.csv(script_path, header = T)

  # 2. Look at the summary of all the variables and convert the following variables as factors
  summary(raw_stock_data)
  #str(raw_stock_data)
  raw_stock_data
}

# Extract data and adj close price
extract_stock_adjclose_sorted = function(raw_stock_data) {
  stock_dates = raw_stock_data$Date
  stock_prices = raw_stock_data$Adj.Close
  stocks_data = data.frame(as.Date(stock_dates),stock_prices)
  colnames(stocks_data) <- c("date","adjclose")
  tmp_stocks_data <- stocks_data[order(stocks_data$date),]
  tmp_stocks_data
}

# Imputation Function
# 1. Generate continous stock price including weekends
# 2. Impute Missing stock price value for week ends with Friday adjclose price
impute_stock_data = function(stocks_data, from = as.Date("2012/01/02"),to=as.Date("2016/12/31"))
{
  # Generate Continues data from 2012-2016, including market holiday for smooth TS processing
  continous_dates= (seq(from=from, to=to, by="day"))
  continous_adjclose = seq(from=0,to=0, by = 1)
  continous_stocks_data = data.frame(continous_dates,continous_adjclose)
  colnames(continous_stocks_data) = c("date","adjclose")
  #str(continous_stocks_data)

  merged_stock_data = merge(continous_stocks_data,stocks_data, by="date",all.x = TRUE )
  stock_data = data.frame(merged_stock_data$date, merged_stock_data$adjclose.y )

  # Impute stock price NA on weekends with last obersered values such as Friday
  imputed_stocks_data = na.locf(stock_data)
  colnames(imputed_stocks_data) = c("date","adjclose")

  stock_data = data.frame(as.Date(imputed_stocks_data$date),
                          as.numeric(imputed_stocks_data$adjclose))
  colnames(stock_data) = c("date","adjclose")
  #View(stock_data)
  stock_data
}
proc_stock_data = function (path ) {
  csv_file =load_csv(path)
  stock_data = extract_stock_adjclose_sorted(csv_file)
  imputed_stock_data = impute_stock_data(stock_data)
  imputed_stock_data
}

proc_stock_test_data = function (path,from =as.Date("2017/01/02"),to=as.Date("2017/01/31") ) {
  csv_file =load_csv(path)
  stocks_data = extract_stock_adjclose_sorted(csv_file)
  imputed_stock_data = impute_stock_data(stocks_data,from =from,to=to)
  imputed_stock_data
}

#stock_data("/home/tparamas/INSOFE_CPEE/project/BSE100/ITC.csv")
