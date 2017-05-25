# Utility function


comp_stock_return = function(script, stock_data, start_date, end_date, invest_period) {
  buy_price = stock_data[stock_data$date==as.Date(start_date),]$adjclose
  curr_price = stock_data[stock_data$date==as.Date(end_date),]$adjclose
  CARG = ((curr_price/buy_price)^(1/invest_period)-1)*100
  script_name = sub(pattern = ".csv", replacement = "", x=script)
  return (list("name"= script_name,"buy"=buy_price,"curr"=curr_price,"CARG"=CARG,"period"=invest_period))
}

update_script_return = function (ret , script_return, idx) {
  script_return$script[idx] = ret$name
  script_return$buy_price[idx] = ret$buy
  script_return$curr_price[idx] = ret$curr
  script_return$invest_period[idx] = ret$period
  script_return$CARG[idx] = ret$CARG
  script_return
}

update_res_metrics = function(all_models_metrics, aic, acc, res_box_test, row_idx) {
  all_models_metrics$TrainMAPE[row_idx] = acc[1,5]
  all_models_metrics$TestMAPE[row_idx] = acc[2,5]
  all_models_metrics$AIC[row_idx] = aic
  all_models_metrics$Pvalue[row_idx] = res_box_test$p
  all_models_metrics$lag[row_idx] = res_box_test$lag
  all_models_metrics
}


