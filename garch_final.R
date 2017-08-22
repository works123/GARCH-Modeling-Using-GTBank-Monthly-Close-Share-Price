
library(fGarch)
library(forecast)
library(fpp)
library(zoo)
library(xts)

library (ggplot2)
library(gridExtra)
library(ggfortify) 


#data source
dmb_data <- read.csv("DMB_data.csv")

#convert data to time series
ts_data <- ts(dmb_data[,(3:5)],frequency=12,start=c(2007,1))



#loan_to_deposit
loan_to_deposit <- ts_data[,1]

#view data
loan_to_deposit



#plot the time series
x <- autoplot(loan_to_deposit)
x +  ggtitle('Nigeria Deposit Money Bank (DMB) Monthly Loan-to-Deposit Ratio') + xlab('Year') + ylab('loan-to-deposit ratio')


#ARIMA model to model the time series
arima_model <- auto.arima(loan_to_deposit)

summary(arima_model)

ts_model_arima <- ts.union(loan_to_deposit,arima_model=fitted(arima_model))

#plot the ARIMA model- ARIMA(0,1,0)(1,0,1)[12] and original time series

arima_plot <- autoplot(ts_model_arima)
plot_data <- arima_plot$data
plot_series_arima<- ggplot(data=plot_data,aes(x = Index, y = value, color = plot_group))+ 
        geom_line() +
        ylab("Loan_to_Deposit Ratio") +
        xlab("Year") +
        labs(color= "plot type")+
        ggtitle("Plot of Time Series and ARIMA model- ARIMA(0,1,0)(1,0,1)[12]") + 
        scale_color_manual(labels=c("arima model","actual_data"),  values=c("red","black"))

     

#investigating the residuals
arima_residual <- residuals(arima_model)

#Residual plot
residual_plot <- autoplot(arima_residual, ylab="residual", xlab="Year",main="Plot of Residual")

acf_plot <- autoplot(acf(arima_residual, plot = FALSE),ylab = "ACF", main ="ACF Residual Plot"  )
pacf_plot <- autoplot(pacf(arima_residual, plot = FALSE),ylab = "PACF", main ="PACF Residual Plot")

gridExtra::grid.arrange(plot_series_arima,residual_plot,acf_plot,pacf_plot)

#check if there is still autocorrelation in the residuals
Box.test(arima_residual^2,lag=12,type='Ljung')
Box.test(arima_residual^2,lag=20,type='Ljung')
Box.test(arima_residual^2, lag= 14, type='Ljung')
Box.test(arima_residual^2, lag= 1, type='Ljung')

#checking for ARCH effect
box_result <- Box.test(arima_residual^2,lag=20,type='Ljung')

t_result <- t.test(arima_residual^2)

box_result
t_result


#ploting the ACF of the arima_residual^2
res_sq_acf <- autoplot(acf(arima_residual^2, plot = FALSE),ylab = "ACF", main ="ACF square of the Residual Plot"  )
res_sq_pacf <- autoplot(pacf(arima_residual^2, plot = FALSE),ylab = "PACF", main ="PACF square of the Residual Plot"  )

gridExtra::grid.arrange(res_sq_acf,res_sq_pacf)

#testing for ARCH from loan_to-deposit ratio model residuals require no further modeling is required as ACF and Ljung Box shows that ARIMA model is adequate

################## Data 2 ##########################################
#Investigating the 91 Treasury bill and prime lending rate

data2 <- read.csv("tb_primelending.csv",dec = ".",header = TRUE,na.strings = '.')

#converting to time series
ts_data2 <- ts(data2[,2:3], start=c(1961,1), frequency=12)

#extract time series from Jan 2002 when values existed for Treasury Bills
ts_data2_filtered <- window(ts_data2,start =c(2002,1))

arima_model_tb91 <- auto.arima(ts_data2_filtered[,2])

summary(arima_model_tb91)

autoplot(ts_data2_filtered )

ts_tb91_model_arima <- ts.union(ts_data2_filtered[,2],arima_model=fitted(arima_model_tb91))

arima_tb91_plot <- autoplot(ts_tb91_model_arima)
plot_tb91_data <- arima_tb91_plot$data
plot_tb91_series_arima <- ggplot(data=plot_tb91_data  ,aes(x = Index, y = value, color = plot_group))+ 
        geom_line() +
        ylab("91-day Treasury Bill Rate ") +
        xlab("Year") +
        labs(color= "plot type")+
        ggtitle("91-day Treasury Bill Monthly Rate Time Series and ARIMA(3,1,1) ")+
        scale_color_manual(labels=c("arima model","actual_data"),  values=c("red","black"))
      

residual_tb91 <- residuals(arima_model_tb91)

#Residual plot
residual_tb91_plot <- autoplot(residual_tb91, ylab="residual", xlab="Year",main="Plot of Residual")

acf_tb91_plot <- autoplot(acf(residual_tb91, plot = FALSE),ylab = "ACF", main ="ACF Residual Plot"  )
pacf_tb91_plot <- autoplot(pacf(residual_tb91, plot = FALSE),ylab = "PACF", main ="PACF Residual Plot" )

gridExtra::grid.arrange(plot_tb91_series_arima,residual_tb91_plot ,acf_tb91_plot,pacf_tb91_plot)


#check if there is still autocorrelation in the residuals
Box.test(residual_tb91,lag=12,type='Ljung')

#checking for ARCH effect
box_result_tb91 <- Box.test(residual_tb91^2,lag=12,type='Ljung')

t_result_tb91 <- t.test(residual_tb91^2)

box_result_tb91
t_result_tb91


#checking for ARCH effect
Box.test(residual_tb91^2,lag=20,type='Ljung')

acf_tb91sq_plot <- autoplot(acf(residual_tb91^2, plot = FALSE),ylab = "ACF", main ="ACF Residual square  Plot"  )
pacf_tb91sq_plot <- autoplot(pacf(residual_tb91^2, plot = FALSE),ylab = "PACF", main ="PACF Residual square Plot" )

gridExtra::grid.arrange(acf_tb91sq_plot, pacf_tb91sq_plot)

#ACF plot and Ljung box text also shows that ARIMA model is adequate. Box test
#shows we cannot reject the null hypothesis that the autocorrelation in this residual is zero

############### Data 3 Daily GTBank stock price ########################
#data source
gtb_data <- read.csv("gtb_daily_price.csv")

#view data
head(gtb_data,10)

#remove duplicate
gtb_data$X <- NULL
gtb_data<- unique(gtb_data)

#the following dates had two records the first entry would for these dates would be discarded and second kept in the data 
#1. 4/9/2009
#2. 6/5/2009
#3. 12/14/2009

gtb_data$id <- 1:nrow(gtb_data)
gtb_data[c(1506,1507,1549,1550,1634,1635),]



#The first entry for the duplicate records were removed
gtb_data_new <- gtb_data[!(gtb_data$id %in% c(1506,1549,1634)),]



head(gtb_data_new,10)

#create a date type field from the gtb_data$Date field
gtb_data_new$pDate <- as.Date(gtb_data_new$Date, '%m/%d/%Y')

#Dropping the original data field
gtb_data_new$Date <- NULL


#plot  daily price
daily_plot<- ggplot(gtb_data_new,aes(pDate,Price)) + 
        geom_line() +
        ylab("Daily Price") +
        xlab("Year") +
        ggtitle("GTB Daily Stock Price: 17-Feb-2003 to 04-Aug-2017")

#plot daily returns of the daily price
plot_change<-ggplot(gtb_data_new,aes(pDate,Change)) + 
        geom_line() +
        ylab("Daily Return") +
        xlab("Year") +
        ggtitle("GTB Daily Stock Price Returns: 17-Feb-2003 to 04-Aug-2017")       

#converting Daily Returns to time series
gtb_daily_returns <- xts(gtb_data_new$Change, order.by=gtb_data_new$pDate)

#converting Daily Price to time series
gtb_daily_price <- xts(log(gtb_data_new$Price), order.by=gtb_data_new$pDate)



#converting Daily Price to Monthly Closing Price time series
gtb_monthly_price <- to.monthly(gtb_daily_price, OHLC=FALSE)

#taken the last 4 months out of the time series to test prediction
# April, May, June,July taken out
gtb_monthly_price_train <- gtb_monthly_price["/2017-03"]



head(gtb_monthly_price_train,5)

####Monthly Time Series
#plot monthly closing price
monthly_plot <- autoplot(gtb_monthly_price_train) + 
        ylab("log(Price)") +
        xlab("Year") +
        ggtitle("GTB Monthly Closing Price: Feb-2003 to Mar-2017")

gridExtra::grid.arrange(daily_plot,monthly_plot)
####


####Monthly time Series
#checking if the Monthly Price time series is stationary
adf.test(gtb_monthly_price_train, alternative="stationary")
ndiffs(gtb_monthly_price_train)

gtb_monthly_price_diff <- diff(gtb_monthly_price_train)



#plot diff
#plot monthly closing price
gtb_monthly_price_diff[1,]<-0 # address the first value which is NA after differencing
diff_plot <- autoplot(gtb_monthly_price_diff) + 
        ylab("Difference") +
        xlab("Year") +
        ggtitle("First Order Difference of Monthly log(Closing Price)")

#requires differencing to make it sationary
acf_1st_order_plot <- autoplot(acf(coredata(gtb_monthly_price_diff), plot = FALSE),ylab = "ACF", main ="ACF Plot of Differenced Monthly Closing Price Time Series"  )
pacf_1st_order_plot <- autoplot(pacf(coredata(gtb_monthly_price_diff), plot = FALSE),ylab = "PACF", main ="PACF Plot of Differenced Monthly Closing Price time Series" )

gridExtra::grid.arrange(diff_plot,acf_1st_order_plot,pacf_1st_order_plot)


#ARIMA model
gtb_monthly_arima_model <- auto.arima(gtb_monthly_price_diff)

summary(gtb_monthly_arima_model)

#plot ARIMA
fitted_monthly_arima <- fitted(gtb_monthly_arima_model)
fitted_monthly_arima <- ts(fitted_monthly_arima,start=c(2003,2),frequency = 12)
plot_fitted_monthly <- autoplot(fitted_monthly_arima, colour="blue")+
        xlab("Year") +
        ylab("Fitted Model") +
        ggtitle("Fitted Model")

#Plot ARIMA and difference time series
monthly_fitted_series <- ts.union(gtb_monthly_price_diff,fitted_monthly_arima)

#Plot ARIMA(0,0,2)(1,0,1)[12] and Monthly Time Series
#plot the ARIMA model- ARIMA(0,0,2)(1,0,1)[12] and original time series

arima_monthly_plot <- autoplot(monthly_fitted_series)
plot_monthly_data <- arima_monthly_plot$data
gtb_series_arima_plot <- ggplot(data=plot_monthly_data,aes(x = Index, y = value, color = plot_group))+ 
        geom_line() +
        ylab("log(Monthly Price) Difference") +     labs(color= "plot type")+
        ggtitle("Plot of Time Series and ARIMA model- (0,0,2)(1,0,1)[12]")+
        scale_color_manual(labels=c("arima model","data"),  values=c("red","black")) +
        xlab("Year") 
#ACF and PACF plot of the residual (monthly price)
residual_monthly_arima <- residuals(gtb_monthly_arima_model)
residual_monthly_arima <-ts(residual_monthly_arima,start=c(2003,2),frequency = 12)

aa<-autoplot(residual_monthly_arima,ylab='log(Monthly Price)', main="Residual Plot")
bb <- autoplot(acf(residual_monthly_arima, plot = FALSE),ylab = "ACF", main ="ACF Plot of Residual - Monthly Price Time Series"  )
cc <- autoplot(pacf(residual_monthly_arima, plot = FALSE),ylab = "PACF", main ="PACF Plot of of Residual - Monthly Price Time Series" )

gtb_series_arima_plot

gridExtra::grid.arrange(gtb_series_arima_plot ,aa,bb,cc)



#Verifying GARCH effect
lag_result <- NULL
for (j in 1:24){
        test <- Box.test(residual_monthly_arima^2, type="Ljung",lag = j)
        temp <- c(j,test$p.value)
        lag_result <- rbind(lag_result,round(temp,4))
}
colnames(lag_result)<- c("lag","p-value")
rownames(lag_result) <- NULL
lag_result
dd<- autoplot(acf(residual_monthly_arima^2, plot = FALSE),ylab = "ACF", main ="ACF Plot of Residual Square - Monthly Price Time Series"  )
ee<- autoplot(pacf(residual_monthly_arima^2, plot = FALSE),ylab = "PACF", main ="PACF Plot of of Residual Square - Monthly Price Time Series" )

gridExtra::grid.arrange(aa,bb,cc,dd,ee)


#GARCH MODEL

garch_model_report <- NULL
for (y in 0:10){
        for (x in 0:10){
                temp_row <- NULL
                if((x==0 & y==0)){
                       x=1 
                }
                temp_garch <- garch(residual_monthly_arima,order=c(x,y), trace=F)
                N <- length(residual_monthly_arima)
                temp_AIC <- AIC(temp_garch)
                temp_logLik <- logLik(temp_garch)
                
                temp_AICc <- -2 * temp_logLik + 2 * (y + 1) *(N/(N-y-2))
                temp_AICcc <- -2 * temp_logLik + 2 * (y + 2) *(N/(N-y-3))
                temp_garchmodel <- paste("(",x,",",y,")")
                
                temp_row <- cbind(temp_garchmodel,temp_AIC,temp_logLik, temp_AICc,temp_AICcc)
                
                garch_model_report <- rbind(garch_model_report,temp_row)
                
                temp_row <- NULL
                
                
        }
}
colnames(garch_model_report) <- c("garch_model","AIC","loglik","AICc","AICcc")

#Generate the AIC, AICc and log likelihood for the different GARCH model
garch_model_report


#posible best garch model 
#selected the GARCH model GARCh(10,10) with the highest AICc, the generally recommended GARCH(1,1)
#When observed that all the coofficient in both GARCH(10,10) and GARCH(1,1) had significant p -value I decided to consider ARCH(1)
garch_monthly_10_10 <- garch(residual_monthly_arima,order=c(10,10), trace=F)
garch_monthly_1_1 <- garch(residual_monthly_arima,order=c(1,1), trace=F)
garch_monthly_0_1 <- garch(residual_monthly_arima,order=c(0,1), trace=F) #best option, arch(1)


summary(garch_monthly_10_10)
summary(garch_monthly_1_1)
summary(garch_monthly_0_1)



plot_residual_10_10<- autoplot(garch_monthly_10_10$residuals, main="residuals of GARCH(10,10)")

plot_residual_1_1 <- autoplot(garch_monthly_1_1$residuals, main="residuals of GARCH(1,1)")
plot_residual_0_1<- autoplot(garch_monthly_0_1$residuals, main="residuals of ARCH(1)")

gridExtra::grid.arrange(aa,plot_residual_10_10,plot_residual_1_1,plot_residual_0_1)


#Test GTB monthly time series April 2017 - Aug test_series <- gtb_monthly_price['2017-04/2017']
time_series_diff <- diff(gtb_monthly_price)
test_series_diff <- time_series_diff['2017-04/2017-07']


#Generate 1-step forecast, 100-step forecast, and plot of forecast:
#forecast_monthly_gtb_step7 <- forecast(gtb_monthly_arima_model,7,level=95)
#plot(forecast_monthly_gtb_step7 )

#Compute ht, conditional variance:
ht.garch01<- garch_monthly_0_1$fit[,1]^2 #ARCH(1)
ht.garch11 <- garch_monthly_1_1$fit[,1]^2 #GARCH(1,1)
ht.garch1010 <- garch_monthly_10_10$fit[,1]^2 #GARCH(1,1)

conditional_plot_garch01 <- autoplot(ht.garch01,main='Conditional variances ARCH(1)')
conditional_plot_garch11 <- autoplot(ht.garch11,main='Conditional variances GARCH(1,1)')
conditional_plot_garch1010 <- autoplot(ht.garch1010,main ='Conditional variances GARCH(10,10)')

gridExtra::grid.arrange(conditional_plot_garch01,conditional_plot_garch11,conditional_plot_garch1010)         


#Generate plot of Log Price, 95% Upper and Lower limit
 conf_int_plot  <- function (garch_model_conditional_sd,description) {
      
        #garch_model_conditional_sd <- ht.garch01
        #description <- " and ARCH(1)"
        fit212 <-ts(fitted.values(gtb_monthly_arima_model),start=c(2003,1),frequency=12)
        low<- fit212-1.96*sqrt(garch_model_conditional_sd)
        high<- fit212+1.96*sqrt(garch_model_conditional_sd)
        mid <- (low+high)/2
        #create a time series 
        fit212_low_high <- ts.union(fit212,low,high,gtb_monthly_price_diff,mid)
        
        fit212_low_high_plot <- autoplot(fit212_low_high[,1:3])
        fit212_low_high_plot_data <- fit212_low_high_plot$data
        title_message <- paste(" model- ARIMA(0,1,0)(1,0,1)[12] ",description," and the 95% confidence Interval")
                
        plot_fit212_low_high <- ggplot(data=fit212_low_high_plot_data,aes(x = Index, y = value, color = plot_group))+ 
                geom_line() +
                ylab("Price(first order difference)") +
                xlab("Year") +
                labs(color= "plot type")+
                ggtitle(title_message) +
               scale_color_manual(labels=c("arima model","high","low"),  values=c("black","red","blue"))
        
        series_low_high_plot <- autoplot(fit212_low_high[,2:4])
        series_low_high_plot_plot_data <- series_low_high_plot$data
        title_message <- paste(" GTB first Order Diff. Time series ",description," and the 95% confidence Interval")
        
        plot_series_low_high_plot <- ggplot(data=series_low_high_plot_plot_data,aes(x = Index, y = value, color = plot_group))+ 
                geom_line() +
                ylab("Price(first order difference)") +
                xlab("Year") +
                labs(color= "plot type")+
                ggtitle(title_message) +
                scale_color_manual(labels=c("actual","high","low"),  values=c("black","red","blue"))
      
        series_mid_plot <- autoplot(fit212_low_high[,4:5])
        series_mid_plot_data <- series_mid_plot$data
        title_message <- paste(" GTB first Order Diff. Time series ",description," and the Mean of low and high")
        
        plot_series_mid <- ggplot(data=series_mid_plot_data,aes(x = Index, y = value, color = plot_group))+ 
                geom_line() +
                ylab("Price(first order difference)") +
                xlab("Year") +
                labs(color= "plot type")+
                ggtitle(title_message) +
               scale_color_manual(labels=c("actual","mean"),  values=c("black","red"))
        
         
      print(plot_fit212_low_high)
      print(plot_series_low_high_plot)
      print( plot_series_mid)

      
      
 }


#print 
result1 <- conf_int_plot(ht.garch01," and ARCH(1)")
result2 <- conf_int_plot(ht.garch11," and GARCH(1,1)")
result3 <- conf_int_plot(ht.garch1010," and GARCH(10,10)")



#Plot the qq plot for the different models

par(mfrow=c(2,3))
qqnorm(residual_monthly_arima,main="ARIMA")
qqline(residual_monthly_arima)

qqnorm(garch_monthly_10_10$residuals,main="GARCH(10,10) ")
qqline(garch_monthly_10_10$residuals)


qqnorm(garch_monthly_1_1$residuals, main="GARCH(1,1)")
qqline(garch_monthly_1_1$residuals)


qqnorm(garch_monthly_0_1$residuals, main="ARCH(1)")
qqline(garch_monthly_0_1$residuals)


archres <- residual_monthly_arima/sqrt(ht.garch01)
qqnorm(archres,main='ARIMA + ARCH(1) ')
qqline(archres)



