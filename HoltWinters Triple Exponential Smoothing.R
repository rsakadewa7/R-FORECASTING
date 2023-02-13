#library
library(RODBC)
library(MLmetrics)
library(forecast)
library(ggplot2)

#connecting with local dsn
SQL = odbcConnect("LocalDSN")

#importing data
data = sqlQuery(SQL,"SELECT  B.commencement_year AS year, B.commencement_month AS month, COUNT(DISTINCT A.policy_id) AS total_policies_per_month 
FROM FactSales A
INNER JOIN DimCommencement_View AS B
ON A.commencement_view_id = B.commencement_view_id
WHERE B.commencement_date BETWEEN CONVERT(DATETIME,'2016-09-01T00:00:00.000') AND CONVERT(DATETIME,'2022-09-30T23:59:59.997')
GROUP BY B.commencement_month, B.commencement_year 
ORDER BY  B.commencement_year ASC")

#convert data to time series
total_policies_actual = ts(data$total_policies_per_month, start=c(2016,9), end=c(2022,9), frequency = 12)

plot(total_policies_actual)

hw_total_policies = HoltWinters(total_policies_actual, alpha = 0.1, beta = 0.2, gamma = 0.7, seasonal = "additive",
                                l.start = 101.25, b.start = 2.354,
                                s.start = c(-35.250, -44.250, 12.750, 6.750, -53.250, -8.250,
                                            37.750, 12.750, 58.750, 0.750, 8.750, 2.750))

#create data frame
df_data <- data.frame(Bulan_dan_Tahun = seq(from = as.Date("2016-09-01"), 
                                  to = as.Date("2022-09-01"), by = "month"), total_policies_per_month = data$total_policies_per_month)

forecast_present <- data.frame(Bulan_dan_Tahun  = seq(from = as.Date("2017-09-01"),
                                           to = as.Date("2022-09-01"), by = "month"), Forecast = hw_total_policies$fitted[,1])

merge_present <- merge(x = df_data, y = forecast_present, by = "Bulan_dan_Tahun")

#predict
future <- predict(hw_total_policies, n.ahead = 12)

forecast_future <- data.frame(Bulan_dan_Tahun  = seq(from = as.Date("2022-10-01"),
                                          to = as.Date("2023-09-01"), by = "month"), Forecast = future[,1])

#check accuracy
mape <- data.frame(mape = MAPE(merge_present$Forecast, merge_present$total_policies_per_month)*100)
mape

#plotting
ggplot() + geom_line(data = merge_present, aes(Bulan_dan_Tahun,total_policies_per_month, color = "Data Aktual"), linewidth = 1) +
  geom_line(data = merge_present, aes(Bulan_dan_Tahun, Forecast, color = "Forecast Sekarang"), linewidth = 1, linetype = "twodash") +
  geom_line(data = forecast_future, aes(Bulan_dan_Tahun, Forecast, color = "Forecast Mendatang"), linewidth = 1, linetype = "twodash") +
  scale_color_manual(name = "Legenda",
                     values = c("Data Aktual" = "#363445", "Forecast Sekarang" = "#d2980d",
                                "Forecast Mendatang" = "#6cd4c5")) +
  labs(x = "Bulan per Tahun ", y ="Jumlah Polis") +
  ggtitle("Forecast Jumlah Polis") 


##################################################################







#predict 6 month ahead
hw_predict = forecast(hw_total_policies, n.ahead = 6)
plot(hw_predict)

#accuracy_check
accuracy(hw_total_policies$fitted[,1], total_policies_actual)

##################

#fitted value
hw_total_policies$fitted

#plot trend and level
plot(hw_total_policies$fitted[,3], ylab = "trend")
plot(hw_total_policies$fitted[,2], ylab = "level")

hw_total_policies$fitted[,2]

##################





                    





#ukuran kesalahan
hw_total_policies$SSE
mse = hw_total_policies$SSE/NROW(hw_total_policies$fitted)
rmse = sqrt(mse)
mape = mean(abs(hw_total_policies$fitted[,1])/hw_total_policies, na.rm = TRUE)*100


#installing forecast packages
install.packages("forecast")

library(forecast)
stlf(total_policies_actual)
###

