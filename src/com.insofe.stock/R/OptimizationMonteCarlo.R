#
# MONTE CHARLO SIMULATION - PORTFOLIO OPTIMIZATION
#
rm(list=ls(all=TRUE))
# Include source from R files
source("R/preprocess.R")
source("R/Utils.R")

print(paste("Current Working directory:",getwd()))
data_path = paste(getwd(),"data",sep="/")

all_script_list = c("BAJFINANCE.csv", "CADILAHC.csv", "HDFC.csv", "IOC.csv", "LT.csv",
                    "RELIANCE.csv", "TATAMOTORS.csv", "TITAN.csv", "BHARATFORG.csv",
                    "HAVELLS.csv", "HINDZINC.csv", "ITC.csv", "MOTHERSUMI.csv",
                    "SHREECEM.csv", "TORNTPHARM.csv", "BRITANNIA.csv", "HDFCBANK.csv", "INFY.csv",
                    "PIDILITIND.csv", "TCS.csv")
start_date = "2012-01-02"
# Historical Stock price End date
end_date = "2016-12-31"
# Investment period in years, default = 5 years starting from 2012 to 2016.
invest_period = 5
comp_all_stocks_return = function(all_script_list) {
  stocks_ret = list()
  for(script in all_script_list) {
    script_path = paste(data_path,script,sep="/")
    script_name = sub(".csv","",script)
    stock_data = proc_stock_data(script_path)
    ret = comp_stock_return(script, stock_data, start_date, end_date, invest_period)
    stocks_ret[script_name]<-ret$CARG
  }
  return(stocks_ret)
}
stocks_return = comp_all_stocks_return(all_script_list)
# Pick Top best performin stocks by CARG
port_stocks_data <- unlist(stocks_return[order(unlist(stocks_return), decreasing=TRUE)][1:5])
stocks_data <- data.frame(stocks = names(port_stocks_data),returns = round(port_stocks_data,2))

amount = 150000
stopping_criteria = 10000
weight_limit = 1
1/5

# Min inviestment is 10% on each stocks in portfolio
min_investment = 0.10
# Max inviestment is not more than 30% on each stock in portfolio
max_investment = 0.30

# Max return initialzed with 15% return and later this gets computed by algoritham
max_portfolio_return = (amount*0.15)

# Equal allocation for all stocks for naive model
allocation = c(0.20, 0.20, 0.20, 0.20, 0.20)
# optimized allocation computed using algorithm and initialized with naive model
optimzed_allocation = allocation

naive_portfolio_ret = function() {

  # Calculate portfolio actual return amount for equal contribution on each stock.
  portfolio_return = (stocks_data$returns %*%
                        (allocation * amount))/100
  # Calculate portfolio return in % for equal contribution on each stock.
  portfolio_return_in_percentage = (portfolio_return/amount)*100

  print(paste("Naive Portfolio Return computation"))
  print("Top stocks return data:")
  print(stocks_data)
  print(paste("Stock Allocation:"))
  print(allocation)
  print(paste("portfolio return:", portfolio_return))
  print(paste("portfolio return in percentage:", portfolio_return_in_percentage))
}


optimized_portfolio_allocation_fn<-function(simulation){

  set.seed(1234)

  for(i in 1:simulation){
    #initialize stock allocation
    stockalloc = numeric()
    nums = runif(5, min = 0.1, max = 0.28)
    tot = round(sum(nums),2)
    if(tot>1) {
      d = (tot-1)/5
      stockalloc = nums -d
    } else if (tot < 1){
      d = (1-tot)/5
      stockalloc = nums + d
    }
    else {
      stockalloc=nums
    }
    portfolio_return = (stocks_data$returns %*%
                          (stockalloc * amount))/100
    #returns<-stocks_data$returns %*% (stockalloc * stockInves)
    if(portfolio_return > max_portfolio_return  ){
      max_portfolio_return <- portfolio_return[,1]
      optimzed_allocation <-stockalloc
    }
  }
  print(paste("####################################################"))
  cat("optimized Allocation ", optimzed_allocation,
      "with returns = ", max_portfolio_return, "\n")

  print(paste("Monte Carlo Portfolio Optimization"))
  print(paste("Simulation Iteration:", simulation))
  print("Top stocks return data:")
  print(stocks_data)
  print(paste("Optimized Stock Allocation:"))
  print(optimzed_allocation)
  print("Invested Period:")
  print(invest_period)
  print(paste("portfolio return:", round(max_portfolio_return,2)))
  portfolio_return_in_percentage = round(((max_portfolio_return/amount)*100),2)
  print(paste("portfolio return in percentage:",portfolio_return_in_percentage ))
  print(paste("##############################################################"))
  return(list(max_portfolio_return,optimzed_allocation))
}


naive_portfolio_ret()

simulations = c(1000,10000,100000)
# instead for loop use sapply
start = Sys.time()
time = sapply(simulations, optimized_portfolio_allocation_fn)
end = Sys.time() - start
end
#############################################################################################
# OUTPUT
########### BENCH MARK MODEL RETURN ##################
# > naive_portfolio_ret()
# [1] "Naive Portfolio Return computation"
# [1] "Top stocks return data:"
# stocks returns
# MOTHERSUMI MOTHERSUMI   55.98
# BAJFINANCE BAJFINANCE   54.34
# SHREECEM     SHREECEM   47.91
# BRITANNIA   BRITANNIA   47.14
# TORNTPHARM TORNTPHARM   40.49
# [1] "Stock Allocation:"
# [1] 0.2 0.2 0.2 0.2 0.2
# [1] "portfolio return: 73758"
# [1] "portfolio return in percentage: 49.172"
#
# #################### MONTE CARLO SIMULATION #################
# [1] "####################################################"
# optimized Allocation  0.2806153 0.2733009 0.1350734 0.1666344 0.1489449 with returns =  76375.95
# [1] "Monte Carlo Portfolio Optimization"
# [1] "Simulation Iteration: 1000"
# [1] "Top stocks return data:"
# stocks returns
# MOTHERSUMI MOTHERSUMI   55.98
# BAJFINANCE BAJFINANCE   54.34
# SHREECEM     SHREECEM   47.91
# BRITANNIA   BRITANNIA   47.14
# TORNTPHARM TORNTPHARM   40.49
# [1] "Optimized Stock Allocation:"
# [1] 0.2806153 0.2733009 0.1350734 0.1666344 0.1489449
# [1] "Invested Period:"
# [1] 5
# [1] "portfolio return: 76375.95"
# [1] "portfolio return in percentage: 50.92"
# [1] "##############################################################"
# [1] "####################################################"
# optimized Allocation  0.2773343 0.2740724 0.2081766 0.139142 0.1041057 with returns =  76749.6
# [1] "Monte Carlo Portfolio Optimization"
# [1] "Simulation Iteration: 10000"
# [1] "Top stocks return data:"
# stocks returns
# MOTHERSUMI MOTHERSUMI   55.98
# BAJFINANCE BAJFINANCE   54.34
# SHREECEM     SHREECEM   47.91
# BRITANNIA   BRITANNIA   47.14
# TORNTPHARM TORNTPHARM   40.49
# [1] "Optimized Stock Allocation:"
# [1] 0.2773343 0.2740724 0.2081766 0.1391420 0.1041057
# [1] "Invested Period:"
# [1] 5
# [1] "portfolio return: 76749.6"
# [1] "portfolio return in percentage: 51.17"
# [1] "##############################################################"
# [1] "####################################################"
# optimized Allocation  0.2860507 0.2976422 0.1722426 0.1222493 0.1255165 with returns =  76926.2
# [1] "Monte Carlo Portfolio Optimization"
# [1] "Simulation Iteration: 1e+05"
# [1] "Top stocks return data:"
# stocks returns
# MOTHERSUMI MOTHERSUMI   55.98
# BAJFINANCE BAJFINANCE   54.34
# SHREECEM     SHREECEM   47.91
# BRITANNIA   BRITANNIA   47.14
# TORNTPHARM TORNTPHARM   40.49
# [1] "Optimized Stock Allocation:"
# [1] 0.2860507 0.2976422 0.1722426 0.1222493 0.1255165
# [1] "Invested Period:"
# [1] 5
# [1] "portfolio return: 76926.2"
# [1] "portfolio return in percentage: 51.28"
#############################################################################################
