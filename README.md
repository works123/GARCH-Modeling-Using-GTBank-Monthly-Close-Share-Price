# Investigating GARCH-Modeling - monthly loan_to_deposit ratio, monthly 91 day treasury bill data, GTBank share price 
This is a repository to enable me investigate using GARCH modeling.  I investigated the following time series
a. Nigerian Deposit Money Bank loan-to-deposit time series (DMB_data)
b. Nigerian 91 day treasury bill monthly  rate time series (tb91_data)
c. GTBank (Nigerian Bank) monthly closing share price time series (gtbank data)

While I adequately modelled  the DMB_data and tb91_data using ARIMA modeling, modeling the share price (gtb_data) was a different challenge.  I attempted modeling it in ARIMA and GARCH but still see alot of opportunity to effectively model the stock time series. 

I would appreciate it if you could investigate this data and provide a more appropraite model for the time series and provide some guidance on how you achieved this.

The data provided are have available publicly on different websites 
1. GTBank daily share price data (17 Feb 2003 - 11 August 2017)
2. GTBank month close share price data (Feb 2003 - March 2017)
3. Nigerian Deposit Money Bank loan-to-deposit data
4. Nigerian 91 day treasury bill monthly  rate data

I have also provided the R script I used in attempting to model the Time series derived from modeling the GTBank month close share price time series.

