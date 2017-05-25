# Project: STOCK PRICE PREDICTION AND PORTFOLIO OPTIMIZATION
# Name: Thamizhannal Paramasivam
# Batch No: 22
#
# Note: Load this project as R project in RStudio and Run it.
############ STOCK PRICE PREDICTION ############################
rm(list=ls(all=TRUE))
# Input Data: Downloaded 20 stocks from Nifty50
# Train Data: Duration: start_date: 2012-01-01 end_date: 2016-12-31
# Test Data: Duration: start_date: 2017-01-02 end_date: 2017-01-14
# Output: Please refer output folder for
# 1. <script_name>.pdf - ITC.pdf - contains all the plots about ITC
# 2. <script_name__forecast_metrics.Rda> - ITC_forecast_metrics.Rda all the forecast metrics
# 3. <script_name_ITC_models_metrics.Rda> - ITC_models_metrics.Rda contains model metrics
# 4. all_script_return.Rda - contains all stocks return for 5 years.

source("R/StockMainProg.R")

########### PORTFOLIO OPTIMIZATION ##############################
# METHOD-1: BENCH MARK portfolio return
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
# portfolio return in percentage: 49.1

# METHOD-2: MonteCarlo Simulation
# assumptions:
# start_date = "2012-01-02"
# end_date = "2016-12-31"
# min_investment = 0.10 # 10% per eacg stock
# Max inviestment is not more than 30% on each stock in portfolio
# max_investment = 0.30


rm(list=ls(all=TRUE))
# Run Portfolio Optimization using MonteCarlo Simulation
source("R/OptimizationMonteCarlo.R")
# OUTPUT:
# Portfolio return for 1 year
# Invested Amount: 15000
# Optimized Stock Allocation: 0.286 0.2970 0.170 0.120 0.125
# portfolio return: 76926.2
# portfolio return in percentage: 51.28
# Simulation Iteration: 1e+05
#"Top stocks return data:"
# stocks returns
# MOTHERSUMI MOTHERSUMI   55.98
# BAJFINANCE BAJFINANCE   54.34
# SHREECEM     SHREECEM   47.91
# BRITANNIA   BRITANNIA   47.14
# TORNTPHARM TORNTPHARM   40.49

## METHOD-3: SIMULATED ANNEALING OPTIMIZATION SOLUTIONS
# ASSUMPTIONS:
# Portfolio return for 1 year
# start_date = "2012-01-02"
# end_date = "2016-12-31"
# min_investment = 10% per each stock
# Max inviestment is not more than 30% on each stock in portfolio
# max_investment = 30% per each stock
# max_iterations = 100000
# Investment amount = 150000
# Choosen Top5 stocks returns
# MOTHERSUMI MOTHERSUMI   55.98
# BAJFINANCE BAJFINANCE   54.34
# SHREECEM     SHREECEM   47.91
# BRITANNIA   BRITANNIA   47.14
# TORNTPHARM TORNTPHARM   40.49

rm(list=ls(all=TRUE))
# Run Portfolio Optimization using SA
source("R/OptimizationSimulatedAnnealing.R")
# OUTPUT:
# Invested Amount: 15000
# Optimized Stock Allocation: 0.33 0.23 0.10 0.12 0.20
# portfolio return: 76443.23
# portfolio return in percentage: 51.3
# Total returns =  76946.61
##############################################################

