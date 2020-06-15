# Price Forecasting

In this project, we examine the sum of the adjusted closing stock prices of mundane commodities, wheat and corn, for every trading day from the beginning of January 2015 to the end of September 2019. The data for the two stocks, Teucrium Wheat (WEAT) and Teucrium Corn Fund (CORN), is provided by Yahoo Finance; more information can be found here:    
* https://finance.yahoo.com/quote/WEAT?p=WEAT
* https://finance.yahoo.com/quote/CORN?p=CORN  

The primary objective is to predict the aggregate stock price of WEAT and CORN for the first few trading days in the fourth quarter of 2019. To accomplish this task, I implemented various time series analysis methods such as SARIMA and VST in R. In total, five candidate models were of interest and the model that yielded the smallest RMSE value in the cross validation and model selection process was used for prediction.
