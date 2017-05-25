
all_script_list = c("BAJFINANCE.csv", "CADILAHC.csv", "HDFC.csv", "IOC.csv", "LT.csv",
                    "RELIANCE.csv", "TATAMOTORS.csv", "TITAN.csv", "BHARATFORG.csv",
                    "HAVELLS.csv", "HINDZINC.csv", "ITC.csv", "MOTHERSUMI.csv",
                    "SHREECEM.csv", "TORNTPHARM.csv", "BRITANNIA.csv", "HDFCBANK.csv", "INFY.csv",
                    "PIDILITIND.csv", "TCS.csv")

all_script_list = c("TCS.csv","HDFC.csv", "HDFCBANK.csv","INFY.csv","BRITANNIA.csv" )

#all_script_list = c("BRITANNIA.csv","ITC.csv","HINDZINC.csv")
# Script forecast calculator

# Number of days to forecast a stock price
h=5
# Investment period in years, default = 5 years starting from 2012 to 2016.
invest_period = 5

# Historical Stock price Stating date
#start_date = "2012-01-02"
start_date = "2016-01-01"
# Historical Stock price End date
end_date = "2016-12-31"

# Script index
idx = 0
