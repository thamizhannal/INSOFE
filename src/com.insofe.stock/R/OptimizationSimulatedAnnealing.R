rm(list=ls(all=TRUE))

# Include source from R files
source("R/preprocess.R")
source("R/Utils.R")
#source("R/const.R")

print(paste("Current Working directory:",getwd()))

# Location of stock data folder
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
min_investment = 0.10
amount = 150000
stopping_criteria = 8000
weight_limit = 1

1/5

# Min inviestment is 10% on each stocks in portfolio
min_investment = 0.10
# Max inviestment is not more than 30% on each stock in portfolio
max_investment = 0.30

# Max return initialzed with 15% return and later this gets computed by algoritham
max_portfolio_return = (amount*0.15)


init_soln_fn <- function(){
  # Let us initiate solution with almost equal weights to make it total 1
  initial_soln = c(0.20, 0.20, 0.20, 0.20, 0.20)
  return(initial_soln)
}



# Purterbation function: randomly select a point and do operation
purterb_fn = function(solution){
  idx = sample(1:length(solution), 2)
  purt_solution = solution
  x = solution[idx[1]]
  y = solution[idx[2]]

  # Taking the diff of 1st stock weight and 0.10 and take half of it
  # Subtract that value and the same value to the secnd stock weight
  # (As total should be 1)

  diff = ((solution[idx[1]] - min_investment)/2)
  purt_solution[idx[1]] = solution[idx[1]] - diff
  purt_solution[idx[2]] = solution[idx[2]] + diff

  return(purt_solution)
}

# Evaluation function.
evaluate_fn = function(stocks_data, weight_limit, solution, amount) {
  #single_script_invest = (solution * rep(amount, length(solution)))

  #total_return = stocks_data$returns %*% single_script_invest

  # Calculate portfolio actual return amount for equal contribution on each stock.
  portfolio_return = (stocks_data$returns %*%
                        (solution * amount))/100
  # Calculate portfolio return in % for equal contribution on each stock.
  #portfolio_return_in_percentage = (portfolio_return/amount)*100

  if (round(sum(solution), 1) != weight_limit)
    return(0)
  else
    return(portfolio_return)
}
run_simulated_annealing_algo = function(max_iterations, amount){
  cat("Max iterations =", max_iterations, "\n")
  # Generate a random solution
  initial_soln = init_soln_fn()
  initial_investment_amount = evaluate_fn(stocks_data , weight_limit, initial_soln, amount)
  print(paste("Initial Solution:"))
  print(initial_soln)

  base_soln = initial_soln
  base_val = initial_investment_amount
  counter = 0

  # solution vs available
  cat(paste("Initial investment amount is : ", base_val, "\n"))
  global_val = base_val
  global_solu = base_soln

  for (i in 1:max_iterations) {
    # Purterbation
    next_soln = purterb_fn(base_soln)
    next_val = evaluate_fn(stocks_data, weight_limit, next_soln, amount)
    if(any(next_soln > min_investment) == FALSE){
      return(0)
    }else{
      counter = counter + 1
      if(next_val > base_val){
        base_solu = next_soln
        base_val = next_val
      }
      else{
        # Accept with acceptence probability
        acceptance_prob = runif(1, 0, 1)
        if(acceptance_prob > 0.5){
          base_soln = next_soln
          base_val = next_val
        }
      }
    }
    if(global_val <= base_val){
      global_val = base_val
      global_solu = base_soln
    }
    i = counter
    # solution
    cat("Returns in ", i, "iteration is : ", base_val,"\n")
  }
  cat("\n","Returns in ", i, "iteration is : ", global_val,"global_val:",global_solu,"\n")


  print(paste("Simulated Anealing - Portfolio Optimization"))
  print(paste("Simulation Iteration:", max_iterations))
  print("Top stocks return data:")
  print(stocks_data)
  print(paste("Optimized Stock Allocation:"))
  print(round(global_solu,2))
  print("Invested Period:")
  print(invest_period)
  print(paste("portfolio return:", round(global_val,2)))
  portfolio_return_in_percentage = round(((global_val/amount)*100),2)
  print(paste("portfolio return in percentage:",portfolio_return_in_percentage ))

  return(list(global_solu, global_val))
}

execute_main_fn = function(stocks_data, max_iterations, amount){
  set.seed(1234)
  solution_list = run_simulated_annealing_algo(max_iterations, amount)
  final_solution = as.numeric(solution_list[[1]])
  final_solution_value = solution_list[[2]]
  stocks_data$finalSolution = final_solution

  cat("Total returns = ", final_solution_value,"\n")
  return(stocks_data)
}

result = execute_main_fn(stocks_data, max_iterations = 100000, amount = 150000)
print("SIMULATED ANNEALING OPTIMIZATION SOLUTIONS")
print(result)

###############################################################################################
# OUTPUT
# Returns in  10000 iteration is :  76443.23 global_val: 0.3295739 0.1944862 0.1980905 0.1018766 0.1759728
# [1] "Simulated Anealing - Portfolio Optimization"
# [1] "Simulation Iteration: 10000"
# [1] "Top stocks return data:"
# stocks returns
# MOTHERSUMI MOTHERSUMI   55.98
# BAJFINANCE BAJFINANCE   54.34
# SHREECEM     SHREECEM   47.91
# BRITANNIA   BRITANNIA   47.14
# TORNTPHARM TORNTPHARM   40.49
# [1] "Optimized Stock Allocation:"
# [1] 0.33 0.19 0.20 0.10 0.18
# [1] "Invested Period:"
# [1] 5
# [1] "portfolio return: 76443.23"
# [1] "portfolio return in percentage: 50.96"
# Total returns =  76443.23

###############################################################
# Returns in  1e+05 iteration is :  76946.61 global_val: 0.3328701 0.2335364 0.1016767 0.1219498 0.209967
# [1] "Simulated Anealing - Portfolio Optimization"
# [1] "Simulation Iteration: 1e+05"
# [1] "Top stocks return data:"
# stocks returns
# MOTHERSUMI MOTHERSUMI   55.98
# BAJFINANCE BAJFINANCE   54.34
# SHREECEM     SHREECEM   47.91
# BRITANNIA   BRITANNIA   47.14
# TORNTPHARM TORNTPHARM   40.49
# [1] "Optimized Stock Allocation:"
# [1] 0.33 0.23 0.10 0.12 0.21
# [1] "Invested Period:"
# [1] 5
# [1] "portfolio return: 76946.61"
# [1] "portfolio return in percentage: 51.3"
# Total returns =  76946.61
#
#
# [1] "SIMULATED ANNEALING OPTIMIZATION SOLUTIONS"
# > print(result)
# stocks returns finalSolution
# MOTHERSUMI MOTHERSUMI   55.98     0.3328701
# BAJFINANCE BAJFINANCE   54.34     0.2335364
# SHREECEM     SHREECEM   47.91     0.1016767
# BRITANNIA   BRITANNIA   47.14     0.1219498
# TORNTPHARM TORNTPHARM   40.49     0.2099670
#
###############################################################################################
