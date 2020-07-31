#*********************************************************************************************#
#*********************************************************************************************#
#           CAPSTONE PROJECT - FORECASTING NET ELECTRICITY GENERATION IN USA                  # 
#*********************************************************************************************#
#*********************************************************************************************#

## USE FORECAST LIBRARY

install.packages("forecast")
library(forecast)
library(zoo)


## CREATE DATA FRAME

# Set working directory for locating data file
setwd("/Users/amrutha/Desktop/CAPSTONE PROJECT")

# Create data frame
Electricity.data <- read.csv("Net_electricity_generation_monthly.csv")

# To See the first 6 records of the file
head(Electricity.data)
tail(Electricity.data)
# To set scale in plots
max(Electricity.data$NET_GENERATION)
min(Electricity.data$NET_GENERATION)


## CREATE TIME SERIES DATA SET USING ts() FUNCTION

# Function ts() takes three arguments: start, end, and freq
# With monthly data, frequency (freq) of periods in a season (year) is 12 
# Arguments start and end are pairs: (season number, period number)
netgen.ts <- ts(Electricity.data$NET_GENERATION, 
                start = c(2001, 1), end = c(2020, 4), freq = 12)

head(netgen.ts)
tail(netgen.ts,4)
min(netgen.ts)
max(netgen.ts)

#*********************************************************************************************#
# 1. TIME PLOTS
#*********************************************************************************************#

## Use plot() to plot time series data  
plot(netgen.ts, 
     xlab = "Time", ylab = "NetGeneration (in thousand mwh)", 
     ylim = c(250000, 450000), main = "Net Electricity Generation in USA", col = "blue")


## ZOOM-IN PLOT OF TIME SERIES DATA

# Create zoom-in plot for 3 years from 2010 through 2013
netgen.ts.3yrs <- window(netgen.ts, start = c(2010, 1), end = c(2013, 12))
plot(netgen.ts.3yrs, 
     xlab = "Time", ylab = "NetGeneration (in thousand mwh)", 
     ylim = c (250000, 450000), main = "Net Electricity Generation in USA for 3 Years", 
     col = "blue")
autoplot(netgen.ts.3yrs, ylab = "NetGeneration (in thousand mwh)", 
         main = "Net Electricity Generation in USA for 3 Years", col = "blue", lwd = 1)

#*********************************************************************************************#
# 2. AUTOCORRELATION
#*********************************************************************************************#

# Use acf() function to identify and plot autocorrrelation for different lags (up to max of 12)
autocor <-Acf(netgen.ts, lag.max = 12, main = "Autocorrelation for Net Electricity Generation in USA")

# Display autocorrelatiion coefficients for various lags
Lag <- round(autocor$lag, 0)
ACF <- round(autocor$acf, 3)
data.frame(Lag, ACF)

# Use stl() function to plot times series components of the original data 
# The plot includes original data, trend, seasonal, and reminder (level and noise component)
netgen.stl <- stl(netgen.ts, s.window = "periodic")
autoplot(netgen.stl, main = "Net Electricity Generation in USA Time Series Component")

#*********************************************************************************************#
# 3. TEST FOR PREDICTABILITY OF NET ELECTRICITY GENERATION TIME SERIES DATA
#*********************************************************************************************#

## APPROACH 1 - Fit an Autoregressive AR(1) model to time series and test the hypothesis that  
## the slope coefficient beta1 != 1 (Ho: b1 =1 vs. H1: b1 ≠1)

# Use Arima() function to fit AR(1) model for regression residulas
# The ARIMA model of order = c(1,0,0) gives an AR(1) model
netgen.ar1<- Arima(netgen.ts, order = c(1,0,0))
summary(netgen.ar1)
# Result: 1. Beta1 = 0.5894 which is very less than 1
# 2. se=0.0530 which means there is not very high chance that this particular coefficient can 
# be equal to 1 which would designate it to be a random walk.
# 3. Hence, Net Electricity Generation in USA data is not a random walk & it can be forecasted

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## APPROACH 2 – Differencing - 1st order differencing b/w data & data lagged by 1 period

# Create differenced Net Electricity Generation in USA data using (lag-1)
diff.netgen.ts <- diff(netgen.ts, lag = 1)
diff.netgen.ts

# Use Acf() function to identify autocorrealtion for the model residuals 
# and plot autocorrrelation for different lags (up to maximum of 12)
Acf(diff.netgen.ts, lag.max = 12, 
    main = "Autocorrelation for Differenced Net Electricity Generation in USA Data")
# Result: 1. ACF plot shows the auto-correltion coefficients are statistically significant, 
# (i.e they are beyond the 2 horizontal threshold levels) which is a very indicative that the
# Net Electricity Generation in USA data is not a random walk & it can be forecasted

#*********************************************************************************************#
# 4. PARTITION
#*********************************************************************************************#

## CREATE DATA PARTITION.

# Define the number of months in the training and validation sets, nTrain & nValid respectively
nValid <- 52
nTrain <- length(netgen.ts) - nValid
train.ts <- window(netgen.ts, start = c(2001, 1), end = c(2001, nTrain))
valid.ts <- window(netgen.ts, start = c(2001, nTrain + 1), 
                   end = c(2001, nTrain + nValid))
train.ts
valid.ts
tail(train.ts)
head(valid.ts)

## PLOT DATA PARTITION

# Plot the time series data and visualize partitions 
plot(train.ts, 
     xlab = "Time", ylab = "Net Generation (in thousand mwh)", ylim = c(250000, 450000), bty = "l",
     xaxt = "n", xlim = c(2001, 2022.25), main = "Data Partition Graph for Net Electricity Generation", lwd = 2) 
axis(1, at = seq(2001, 2022, 1), labels = format(seq(2001, 2022, 1)))
lines(valid.ts, col = "black", lty = 1, lwd = 2)

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals
lines(c(2020.25 - 4.25, 2020.25 - 4.25), c(0, 450000))
lines(c(2020.25, 2020.25), c(0, 450000))
text(2007.25, 440000, "Training")
text(2018.25, 440000, "Validation")
text(2021.75, 440000, "Future")
arrows(2020.25 - 4.25, 425000, 2000.6, 425000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25 - 4.25, 425000, 2020.25, 425000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25, 425000, 2022.75, 425000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#*********************************************************************************************#
# 5. BASELINE MODELS - NAIVE & SEASONAL NAIVE FORECASTS
#*********************************************************************************************#

## IDENTIFY NAIVE AND SEASONAL NAIVE FORECAST

# Use naive() to make naive forecast (netgen.naive.pred) for validation data 
# Use snaive() to make seasonal naive forecast (netgen.snaive.pred) for validation data 
netgen.naive.pred <- naive(train.ts, h = nValid)
netgen.snaive.pred <- snaive(train.ts, h = nValid)

# plot the predictions for naive forecast
plot(netgen.naive.pred$mean, 
     xlab = "Time", ylab = "Net Generation (in thousand mwh)", ylim = c(250000, 450000), bty = "l",
     xaxt = "n", xlim = c(2001, 2022.25), main = "Naive Forecast", col = "blue", lwd =2) 
axis(1, at = seq(2001, 2022, 1), labels = format(seq(2001, 2022, 1)))
lines(netgen.naive.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)

# plot the predictions for seasonal naive forecast
plot(netgen.snaive.pred$mean, 
     xlab = "Time", ylab = "Net Generation (in thousand mwh)", ylim = c(250000, 450000), bty = "l",
     xaxt = "n", xlim = c(2001, 2022.25), main = "Seasonal Naive Forecast", col = "blue", lwd =2) 
axis(1, at = seq(2001, 2022, 1), labels = format(seq(2001, 2022, 1)))
lines(netgen.snaive.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals
lines(c(2020.25 - 4.25, 2020.25 - 4.25), c(0, 450000))
lines(c(2020.25, 2020.25), c(0, 450000))
text(2007.25, 440000, "Training")
text(2018.25, 440000, "Validation")
text(2021.75, 440000, "Future")
arrows(2020.25 - 4.25, 425000, 2000.6, 425000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25 - 4.25, 425000, 2020.25, 425000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25, 425000, 2022.75, 425000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

## IDENTIFY FORECAST ACCURACY FOR NAIVE & SEASONAL NAIVE FORECASTS

# Use accuracy() function to identify common accuracy measures
# Use round() function to round accuracy measures to three decimal digits
round(accuracy(netgen.naive.pred$mean, valid.ts), 3)
round(accuracy(netgen.snaive.pred$mean, valid.ts), 3)

#*********************************************************************************************#
# 6. MOVING AVERAGE MODELS
#*********************************************************************************************#

## MODEL 1 : CREATE CENTERED MA FOR VARIOUS WINDOWS (NUMBER OF PERIODS)

# Create centered moving average with window k = k = 4, 5, 12
ma.centered_4 <- ma(netgen.ts, order = 4)
ma.centered_5 <- ma(netgen.ts, order = 5)
ma.centered_12 <- ma(netgen.ts, order = 12)

# Combine netgen.ts and ma.centered in one data table
# The data is unequal in length (232 data points in netgen.ts
# vs. 221 data points in ma.centered_12). Thus, it is required 
# to add NA (not applicalbe) to the first 6 & last 6 periods
# the first centered ma in period 7 (July 2001) 
ma.center_4 <- c(rep(NA, length(netgen.ts) - length(ma.centered_4)), ma.centered_4)
ma.center_5 <- c(rep(NA, length(netgen.ts) - length(ma.centered_5)), ma.centered_5)
ma.center_12 <- c(rep(NA, length(netgen.ts) - length(ma.centered_12)), ma.centered_12)

ma_centered_tab <- cbind(netgen.ts, ma.center_4, ma.center_5, ma.center_12)
ma_centered_tab

# Use accuracy() function to identify common accuracy measures
# Use round() function to round accuracy measures to three decimal digits
round(accuracy(ma.center_4, netgen.ts), 3)
round(accuracy(ma.center_5, netgen.ts), 3)
round(accuracy(ma.center_12, netgen.ts), 3)

## Create centered MA forecast for 12 periods into the future
ma.centered_12.pred <- forecast(ma.centered_12, h=12, level = 0)
ma.centered_12.pred

## Plot original data and centered MA
plot(netgen.ts, 
     xlab = "Time", ylab = "Net Generation (in thousand mwh)", ylim = c(250000, 460000), bty = "l",
     xaxt = "n", xlim = c(2001, 2022.25), main = "Centered Moving Average") 
axis(1, at = seq(2001, 2022, 1), labels = format(seq(2001, 2022, 1)) )
lines(ma.centered_4, col = "brown", lwd = 2)
lines(ma.centered_5, col = "green", lwd = 2)
lines(ma.centered_12, col = "blue", lwd = 2)
legend(2002,460000, legend = c("Net Generation", "Centered MA, k=4", "Centered MA, k=5",
                               "Centered MA, k=12"), 
       col = c("black", "brown" , "green", "blue"), 
       lty = c(1, 1, 1), lwd =c(1, 2, 2), bty = "n")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## MODEL 2 : CREATE TRAILING MA FOR VARIOUS WINDOWS (NUMBER OF PERIODS)

# Create trailing moving average with window (number of periods) k = 4, 5, 12
# In rollmean(), use argument align = "right" to calculate a trailing MA
ma.trailing_4 <- rollmean(train.ts, k = 4, align = "right")
ma.trailing_5 <- rollmean(train.ts, k = 5, align = "right")
ma.trailing_12 <- rollmean(train.ts, k = 12, align = "right")

# Use head() function to show the 1st 6 results and tail() function to show the last 6 results
head(ma.trailing_12)
tail(ma.trailing_12)

# Combine netgen.ts and ma.trailing in one data table
# The data is unequal in length (232 data points in netgen.ts vs. 221 data points in ma.trailing_12) 
# Thus, it is required to add NA (not applicalbe) to the first 11 periods before the first trailing ma in period 12 (Dec 2001)
ma.trail_4 <- c(rep(NA, length(train.ts) - length(ma.trailing_4)), ma.trailing_4)
ma.trail_5 <- c(rep(NA, length(train.ts) - length(ma.trailing_5)), ma.trailing_5)
ma.trail_12 <- c(rep(NA, length(train.ts) - length(ma.trailing_12)), ma.trailing_12)

ma_trailing_tab <- cbind(train.ts, ma.trail_4, ma.trail_5, ma.trail_12)
ma_trailing_tab

# Obtain the last MA in the trailing period (last.ma) and
# create forecast for the validation data (ma.trailing.pred)
last.ma_4 <- tail(ma.trailing_4, 1)
ma.trailing_4.pred <- ts(rep(last.ma_4, nValid), start = c(2001, nTrain + 1),
                         end = c(2001, nTrain + nValid), freq = 12)
last.ma_5 <- tail(ma.trailing_5, 1)
ma.trailing_5.pred <- ts(rep(last.ma_5, nValid), start = c(2001, nTrain + 1),
                         end = c(2001, nTrain + nValid), freq = 12)
last.ma_12 <- tail(ma.trailing_12, 1)
ma.trailing_12.pred <- ts(rep(last.ma_12, nValid), start = c(2001, nTrain + 1),
                          end = c(2001, nTrain + nValid), freq = 12)

# Use accuracy() function to identify common accuracy measures
# Use round() function to round accuracy measures to three decimal digits
round(accuracy(ma.trailing_4.pred, valid.ts),3)
round(accuracy(ma.trailing_5.pred, valid.ts),3)
round(accuracy(ma.trailing_12.pred, valid.ts),3)

## Plot original data and trailing MA
plot(train.ts, 
     xlab = "Time", ylab = "Net Generation (in thousand mwh)", ylim = c(250000, 480000), bty = "l",
     xaxt = "n", xlim = c(2001, 2022.25), main = "Trailing Moving Average") 
axis(1, at = seq(2001, 2022, 1), labels = format(seq(2001, 2022, 1)) )
lines(ma.trailing_4, col = "brown", lwd = 2, lty = 1)
lines(ma.trailing_5, col = "green", lwd = 2, lty = 1)
lines(ma.trailing_12, col = "blue", lwd = 2, lty = 1)
lines(ma.trailing_4.pred, col = "brown", lwd = 2, lty = 5)
lines(ma.trailing_5.pred, col = "green", lwd = 2, lty = 5)
lines(ma.trailing_12.pred, col = "blue", lwd = 2, lty = 5)
lines(valid.ts)
legend(2002,480000, legend = c("Net Generation", "Trailing MA, k=4", "Trailing MA, k=5",
                               "Trailing MA, k=12"),
       col = c("black", "brown", "green", "blue"), 
       lty = c(1, 1, 5), lwd =c(1, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows describing training, validation, 
# and future prediction intervals.
lines(c(2020.25 - 4.25, 2020.25 - 4.25), c(0, 480000))
lines(c(2020.25, 2020.25), c(0, 480000))
text(2007.25, 480000, "Training")
text(2018.25, 480000, "Validation")
text(2021.75, 480000, "Future")
arrows(2020.25 - 4.25, 475000, 2000.6, 475000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25 - 4.25, 475000, 2020.25, 475000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25, 475000, 2022.75, 475000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## GENERATE PLOT FOR ORIGINAL DATA, CENTERED MA, and TRAILING MA.

# Plot original data, centered and trailing MA.
plot(netgen.ts, 
     xlab = "Time", ylab = "Net Generation (in thousand mwh)", ylim = c(250000, 450000), bty = "l",
     xaxt = "n", xlim = c(2001, 2022.25), main = "Centered,& Trailing Moving Average") 
axis(1, at = seq(2001, 2022, 1), labels = format(seq(2001, 2022, 1)) )
lines(ma.centered_12, col = "brown", lwd = 2)
lines(ma.trailing_12, col = "blue", lwd = 2, lty = 5)
legend(2002,450000, legend = c("Net Generation", "Centered MA, k=12",
                               "Trailing MA, k= 12"), 
       col = c("black", "brown", "blue"), 
       lty = c(1, 1, 5), lwd =c(1, 2, 2), bty = "n")

#*********************************************************************************************#
# 7. EXPONENTIAL SMOOTHING MODELS
#*********************************************************************************************#

## 7A - SIMPLE EXPONENTIAL SMOOTHING WITH PARTITIONED DATA

# MODEL 1 : SIMPLE EXPONENTIAL SMOOTHING WITH ADDITIVE ERROR AND ALPHA = 0.2
# Use ets() function with model = "ANN", i.e., additive error(A), no trend (N) & no seasonality (N)
# Use alpha = 0.2 to fit SES over the training period
ses.part <- ets(train.ts, model = "ANN", alpha = 0.2)
ses.part
# Using forecast() function to make predictions for validation period (nValid)
ses.part.pred <- forecast(ses.part, h = nValid, level = 0)
ses.part.pred

# MODEL 2 : SIMPLE EXPONENTIAL SMOOTHING WITH ADDITIVE ERROR AND OPTIMAL ALPHA
ses.popt <- ets(train.ts, model = "ANN")
ses.popt
# Using forecast() function to make predictions for validation period (nValid) 
ses.popt.pred <- forecast(ses.popt, h = nValid, level = 0)
ses.popt.pred

# MODEL 3 : SIMPLE EXPONENTIAL SMOOTHING WITH MULTIPLICATIVE ERROR AND ALPHA = 0.2
# Use ets() function with model = "MNN", i.e., multiplicative error(A), no trend (N) & no seasonality (N)
# Use alpha = 0.2 to fit SES over the training period
ses.part_M <- ets(train.ts, model = "MNN", alpha = 0.2)
ses.part_M
# Using forecast() function to make predictions for validation period (nValid)
ses.part.pred_M <- forecast(ses.part_M, h = nValid, level = 0)
ses.part.pred_M

# MODEL 4 : SIMPLE EXPONENTIAL SMOOTHING WITH MULTIPLICATIVE ERROR AND OPTIMAL ALPHA
ses.popt_M <- ets(train.ts, model = "MNN")
ses.popt_M
# Using forecast() function to make predictions for validation period (nValid) 
ses.popt.pred_M <- forecast(ses.popt_M, h = nValid, level = 0)
ses.popt.pred_M

# COMPARE VALIDATION ACCURACY FOR THE FOUR SES MODELS 
round(accuracy(ses.part.pred, valid.ts), 3)
round(accuracy(ses.popt.pred, valid.ts),3)
round(accuracy(ses.part.pred_M, valid.ts), 3)
round(accuracy(ses.popt.pred_M, valid.ts),3)

## PLOT ORIGINAL DATA AND SES VALIDATION FORECAST.

# Plot ses predictions for original data, optimal alpha.
plot(ses.popt.pred, 
     xlab = "Time", ylab = "Net Generation (in thousand mwh)", ylim = c(250000, 460000), bty = "l",
     xaxt = "n", xlim = c(2001, 2022.25), lwd = 2,
     main = "SES Model with Additive Error & Optimal Alpha", flty = 2) 
axis(1, at = seq(2001, 2022, 1), labels = format(seq(2001, 2022, 1)))
lines(ses.popt.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts)
legend(2002,450000, legend = c("Net Generation Time Series", "SES with ANN & Optimal Alpha"), 
       col = c("black", "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020.25 - 4.25, 2020.25 - 4.25), c(0, 460000))
lines(c(2020.25, 2020.25), c(0, 460000))
text(2007.25, 460000, "Training")
text(2018.25, 460000, "Validation")
text(2021.75, 460000, "Future")
arrows(2020.25 - 4.25, 454000, 2000.6, 454000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25 - 4.25, 454000, 2020.25, 454000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25, 454000, 2022.75, 454000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## 7B - ADVANCED EXPONENTIAL SMOOTHING METHODS WITH PARTITIONED DATA

# I - HOLT'S EXPONENTIAL SMOOTHING WITH PARTITIONED DATA & OPTIMAL PARAMETERS FOR ALPHA & BETA.

# MODEL 1 - Create Holt's exponenthial smoothing for Net Electricity Generation in USA data.
# Use ets() function with model = "AAN", i.e., additive error(A), additive trend (A), & no seasonality (N). 
# Use optimal alpha and beta to fit Holt's over the training period.
h.AAN <- ets(train.ts, model = "AAN")
h.AAN
# Use forecast() function to make predictions using this Holt's model for validation period (nValid). 
h.AAN.pred <- forecast(h.AAN, h = nValid, level = 0)
h.AAN.pred

# MODEL 2 - MAN i.e., multiplicative error(M), additive trend (A), & no seasonality Holt's Model
h.MAN <- ets(train.ts, model = "MAN")
h.MAN
# Use forecast() function to make predictions using this Holt's model for validation period (nValid). 
h.MAN.pred <- forecast(h.MAN, h = nValid, level = 0)
h.MAN.pred

# MODEL 3 - MMN i.e., multiplicative error(M), multiplicative trend (M), & no seasonality Holt's Model
h.MMN <- ets(train.ts, model = "MMN")
h.MMN
# Use forecast() function to make predictions using this Holt's model for validation period (nValid). 
h.MMN.pred <- forecast(h.MMN, h = nValid, level = 0)
h.MMN.pred

# MODEL 4 - ZZN i.e, optimal error(M), optimal trend (M), & no seasonality Holt's Mode
h.ZZN <- ets(train.ts, model = "ZZN")
h.ZZN
# Use forecast() function to make predictions using this Holt's model with validation period (nValid). 
h.ZZN.pred <- forecast(h.ZZN, h = nValid, level = 0)
h.ZZN.pred

## COMPARE ACCURACY OF FOUR HOLT'S MODELS
round(accuracy(h.AAN.pred, valid.ts), 3)
round(accuracy(h.MAN.pred, valid.ts), 3)
round(accuracy(h.MMN.pred, valid.ts), 3)
round(accuracy(h.ZZN.pred, valid.ts), 3)

# Plot Holt's predictions for original data, optimal alpha and beta.
plot(h.ZZN.pred, 
     xlab = "Time", ylab = "Net Generation (in thousand mwh)", ylim = c(250000, 450000), bty = "l",
     xaxt = "n", xlim = c(2001, 2022.25), lwd = 2,
     main = "Holt's ZZN Model with Optimal Parameters", flty = 2) 
axis(1, at = seq(2001, 2022, 1), labels = format(seq(2001, 2022, 1)))
lines(h.ZZN.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020.25 - 4.25, 2020.25 - 4.25), c(0, 450000))
lines(c(2020.25, 2020.25), c(0, 450000))
text(2007.25, 440000, "Training")
text(2018.25, 440000, "Validation")
text(2021.75, 440000, "Future")
arrows(2020.25 - 4.25, 425000, 2000.6, 425000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25 - 4.25, 425000, 2020.25, 425000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25, 425000, 2022.75, 425000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

# II - HOLT-WINTER'S EXPONENTIAL SMOOTHING WITH PARTITIONED DATA & OPTIMAL PARAMETERS FOR ALPHA, BETA, AND GAMMA.

# MODEL 1 - Create Holt-Winter's exponenthial smoothing (HW) for Net Electricity Generation in USA data.
# Use ets() function with model = "AAA", i.e., additive error(A), additive trend (A), & additive seasonality (A). 
# Use optimal alpha, beta, & gamma to fit HW over the training period.
hw.AAA <- ets(train.ts, model = "AAA")
hw.AAA
# Use forecast() function to make predictions using this HW model for validation period (nValid). 
hw.AAA.pred <- forecast(hw.AAA, h = nValid, level = 0)
hw.AAA.pred

# MODEL 2 - MAA i.e., multiplicative error(M), additive trend (A), & additive seasonality (A) HW MODEL
hw.MAA <- ets(train.ts, model = "MAA")
hw.MAA
# Use forecast() function to make predictions using this HW model for validation period (nValid).
hw.MAA.pred <- forecast(hw.MAA, h = nValid, level = 0)
hw.MAA.pred

# MODEL 3 - MAM i.e., multiplicative error(M), additive trend (A), & multiplicative seasonality (M) HW MODEL
hw.MAM <- ets(train.ts, model = "MAM")
hw.MAM
# Use forecast() function to make predictions using this HW model for validation period (nValid).
hw.MAM.pred <- forecast(hw.MAM, h = nValid, level = 0)
hw.MAM.pred

# MODEL 4 - MMM i.e., multiplicative error(M), multiplicative trend (M), & multiplicative seasonality (M) HW MODEL
hw.MMM <- ets(train.ts, model = "MMM")
hw.MMM
# Use forecast() function to make predictions using this HW model for validation period (nValid).
hw.MMM.pred <- forecast(hw.MMM, h = nValid, level = 0)
hw.MMM.pred

# MODEL 5 - ZZZ i.e.,ALL OPTIMAL HW MODEL
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ
# Use forecast() function to make predictions using this HW model with validation period (nValid). 
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred

## COMPARE ACCURACY OF FIVE HOLT-WINTER'S MODELS
round(accuracy(hw.AAA.pred, valid.ts), 3)
round(accuracy(hw.MAA.pred, valid.ts), 3)
round(accuracy(hw.MAM.pred, valid.ts), 3)
round(accuracy(hw.MMM.pred, valid.ts), 3)
round(accuracy(hw.ZZZ.pred, valid.ts), 3)

# Plot ZZZ HW's predictions for original data and optimal alpha, beta and gamma.
plot(hw.ZZZ.pred, 
     xlab = "Time", ylab = "Net Generation (in thousand mwh)", ylim = c(250000, 450000), bty = "l",
     xaxt = "n", xlim = c(2001, 2022.25), lwd = 2,
     main = "Holt-Winter's Model with Automated Selection of Model Parameters", flty = 2) 
axis(1, at = seq(2001, 2022, 1), labels = format(seq(2001, 2022, 1)))
lines(hw.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020.25 - 4.25, 2020.25 - 4.25), c(0, 450000))
lines(c(2020.25, 2020.25), c(0, 450000))
text(2007.25, 440000, "Training")
text(2018.25, 440000, "Validation")
text(2021.75, 440000, "Future")
arrows(2020.25 - 4.25, 425000, 2000.6, 425000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25 - 4.25, 425000, 2020.25, 425000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25, 425000, 2022.75, 425000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#*********************************************************************************************#
# 8. REGRESSION MODELS
#*********************************************************************************************#

## MODEL 1 : FIT REGRESSION MODEL WITH LINEAR TREND. 

# Use tslm() function (time series linear model) to create regression model with linear trend.
# ~ Means what are the parameters we are including in the tslm function
train.lin <- tslm(train.ts ~ trend)
summary(train.lin)
# Apply forecast() function to make forecast for validation period.
train.lin.pred <- forecast(train.lin, h = nValid, level = 0)
train.lin.pred
# Use accuracy() function to identify common accuracy measures with rounded values to 3 decimals.
round(accuracy(train.lin.pred, valid.ts), 3)

# plot ts data, linear trend and foreccast for validation period.
plot(train.lin.pred, 
     xlab = "Time", ylab = "Net Generation (in thousand mwh)", ylim = c(250000, 450000), bty = "l",
     xaxt = "n", xlim = c(2001, 2022.25), main = "Regression Model with Linear Trend", flty = 2) 
axis(1, at = seq(2001, 2022, 1), labels = format(seq(2001, 2022, 1)))
lines(train.lin.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lty = 1)
legend(2002,450000, legend = c("Net Generation Time Series", "Linear Regression for Training Data",
                               "Linear Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020.25 - 4.25, 2020.25 - 4.25), c(0, 450000))
lines(c(2020.25, 2020.25), c(0, 450000))
text(2007.25, 450000, "Training")
text(2018.25, 450000, "Validation")
text(2021.75, 450000, "Future")
arrows(2020.25 - 4.25, 445000, 2000.6, 445000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25 - 4.25, 445000, 2020.25, 445000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25, 445000, 2022.75, 445000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## MODEL 2 : FIT REGRESSION MODEL WITH EXPONENTIAL TREND. 

# Use tslm() function to create regression model with exponential trend.
# If lambda = 0, tslm function applies Box-Cox transformation for log(y) - exponetial trend to convert into natural logarithm.
# If lambda = 1, tslm function will just have a linear trend (the same as the original regression with linear trend, train.lin).
train.expo <- tslm(train.ts ~ trend, lambda = 0)
summary(train.expo)
# Apply forecast() function to make forecast using exponential trend for validation period. 
train.expo.pred <- forecast(train.expo, h = nValid, level = 0)
train.expo.pred

# plot ts data, exponential and linear trends, and respective forecasts for validation period.
# $fitted - training period, $mean - validation period
plot(train.expo.pred, 
     xlab = "Time", ylab = "Net Generation (in thousand mwh)", ylim = c(250000, 450000), bty = "l",
     xaxt = "n", xlim = c(2001, 2022.25), main = "Linear and Exponential Regression Trends") 
axis(1, at = seq(2001, 2022, 1), labels = format(seq(2001, 2022, 1)))
lines(train.expo.pred$fitted, col = "blue", lwd = 2)
lines(train.lin.pred$fitted, col = "brown", lwd = 2, lty = 3)
lines(train.lin.pred$mean, col = "brown", lwd = 2, lty = 3)
lines(valid.ts, col = "black", lty = 1)
lines(train.ts, col = "black", lty = 1)
legend(2002,450000, legend = c("Net Generation Time Series", 
                               "Linear Trend for Training and Validation Data",
                               "Exponentail Trend for Training and Validdation Data"), 
       col = c("black", "blue" , "brown"), 
       lty = c(1, 1, 3), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020.25 - 4.25, 2020.25 - 4.25), c(0, 450000))
lines(c(2020.25, 2020.25), c(0, 450000))
text(2007.25, 450000, "Training")
text(2018.25, 450000, "Validation")
text(2021.75, 450000, "Future")
arrows(2020.25 - 4.25, 445000, 2000.6, 445000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25 - 4.25, 445000, 2020.25, 445000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25, 445000, 2022.75, 445000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use accuracy() function to identify common accuracy measures for regression model
round(accuracy(train.lin.pred, valid.ts), 3)
round(accuracy(train.expo.pred, valid.ts), 3)

# mape is calculated as the average of (actual - predicted) / abs(actual). 
# This means that the function will return -Inf, Inf, or NaN if actual is zero. 
# Due to the instability at or near zero, smape or mase are often used as alternatives.

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## MODEL 3 : FIT REGRESSION MODEL WITH QUADRATIC (POLYNOMIAL) TREND. 

# Use tslm() function to create quadratic (polynomial) trend model.
train.quad <- tslm(train.ts ~ trend + I(trend^2))
summary(train.quad)
# Apply forecast() function to make predictions for ts data in validation set.  
train.quad.pred <- forecast(train.quad, h = nValid, level = 0)
train.quad.pred

# plot ts data, regression with quadratic trend and forecast for validation period.
plot(train.quad.pred, 
     xlab = "Time", ylab = "Net Generation (in thousand mwh)", ylim = c(250000, 450000), bty = "l",
     xaxt = "n", xlim = c(2001, 2022.25), main = "Quadratic Trend for Training and Validation Data", 
     flty = 2) 
axis(1, at = seq(2001, 2022, 1), labels = format(seq(2001, 2022, 1)))
lines(train.quad.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lty = 1)
legend(2002,450000, legend = c("Net Generation Time Series", "Quadratic Trend for Training Data",
                               "Quadratic Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020.25 - 4.25, 2020.25 - 4.25), c(0, 450000))
lines(c(2020.25, 2020.25), c(0, 450000))
text(2007.25, 450000, "Training")
text(2018.25, 450000, "Validation")
text(2021.75, 450000, "Future")
arrows(2020.25 - 4.25, 445000, 2000.6, 445000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25 - 4.25, 445000, 2020.25, 445000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25, 445000, 2022.75, 445000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use accuracy() function to identify common accuracy measures
# for regression models with linear trend and quandratic (polynomial) trend.
round(accuracy(train.lin.pred, valid.ts), 3)
round(accuracy(train.expo.pred, valid.ts), 3)
round(accuracy(train.quad.pred, valid.ts), 3)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## MODEL 4 : FIT REGRESSION MODEL WITH QUADRATIC TREND AND SEASONALITY. 

# Use tslm() function to create quadratic trend and seasonal model.
train.qtrend.season <- tslm(train.ts ~ trend + I(trend^2) + season)
# See summary of quadratic trend and seasonality model and asociated parameters.
summary(train.qtrend.season)
# Apply forecast() function to make predictions for validation set.  
train.qtrend.season.pred <- forecast(train.qtrend.season, h = nValid, level = 0)
train.qtrend.season.pred

# 4a.Plot ts data, trend and seasonality data, and predictions for validation period.
plot(train.qtrend.season.pred, 
     xlab = "Time", ylab = "Net Generation (in thousand mwh)", ylim = c(250000, 450000), bty = "l",
     xaxt = "n", xlim = c(2001, 2022.25), main = "Model with Quadratic Trend and Monthly Seasonality", 
     flty = 5, lwd = 2) 
axis(1, at = seq(2001, 2022, 1), labels = format(seq(2001, 2022, 1)))
lines(train.qtrend.season.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lty = 1, lwd = 2)
legend(2002,450000, legend = c("Net Generation Time Series", 
                               "Trend and Seasonality Model for Training Data",
                               "Trend and Seasonality Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020.25 - 4.25, 2020.25 - 4.25), c(0, 450000))
lines(c(2020.25, 2020.25), c(0, 450000))
text(2007.25, 450000, "Training")
text(2018.25, 450000, "Validation")
text(2021.75, 450000, "Future")
arrows(2020.25 - 4.25, 445000, 2000.6, 445000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25 - 4.25, 445000, 2020.25, 445000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25, 445000, 2022.75, 445000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# 4b. Plot residuals of predictions with trend and seasonality.
train.qtrend.season.pred$residuals
min(train.qtrend.season.pred$residuals)
max(train.qtrend.season.pred$residuals)

valid.ts - train.qtrend.season.pred$mean
min(valid.ts - train.qtrend.season.pred$mean)
max(valid.ts - train.qtrend.season.pred$mean)

plot(train.qtrend.season.pred$residuals, 
     xlab = "Time", ylab = "Net Generation (in thousand mwh)", ylim = c(-50000, 80000), bty = "l",
     xaxt = "n", xlim = c(2001, 2022.25), main = "Residuals for Trend and Seasonality Model", 
     col = "brown", lwd = 2) 
axis(1, at = seq(2001, 2022, 1), labels = format(seq(2001, 2022, 1)))
lines(valid.ts - train.qtrend.season.pred$mean, col = "brown", lty = 1, lwd=2)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020.25 - 4.25, 2020.25 - 4.25), c(-50000, 80000))
lines(c(2020.25, 2020.25), c(-50000, 80000))
text(2007.25, 65000, "Training")
text(2018.25, 65000, "Validation")
text(2021.75, 65000, "Future")
arrows(2020.25 - 4.25, 60000, 2000.6, 60000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25 - 4.25, 60000, 2020.25, 60000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25, 60000, 2022.75, 60000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use accuracy() function to identify common accuracy measures
# for various regression models: (1)linear trend, (2) Exponential trend, (3) Quadratic trend,  
# and (4) quadratic trend and seasonality.
round(accuracy(train.lin.pred, valid.ts),3)
round(accuracy(train.expo.pred, valid.ts), 3)
round(accuracy(train.quad.pred, valid.ts),3)
round(accuracy(train.qtrend.season.pred, valid.ts),3)
# All comparisons with partitioning is done only with the validation period
# Model 4 has the lowest MAPE & RMSE. Definitely this model 4 with seasonality is better one

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## MODEL 5 : FIT REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY. 

# Use tslm() function to create linear trend and seasonal model.
train.lintrend.season <- tslm(train.ts ~ trend  + season)
# See summary of linear trend and seasonality model and asociated parameters.
summary(train.lintrend.season)
# Apply forecast() function to make predictions for validation set.  
train.lintrend.season.pred <- forecast(train.lintrend.season, h = nValid, level = 0)
train.lintrend.season.pred

# 5a.Plot ts data, trend and seasonality data, and predictions for validation period.
plot(train.lintrend.season.pred, 
     xlab = "Time", ylab = "Net Generation (in thousand mwh)", ylim = c(250000, 450000), bty = "l",
     xaxt = "n", xlim = c(2001, 2022.25), main = "Model with Linear Trend and Monthly Seasonality", 
     flty = 5, lwd = 2) 
axis(1, at = seq(2001, 2022, 1), labels = format(seq(2001, 2022, 1)))
lines(train.lintrend.season.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lty = 1, lwd = 2)
legend(2002,450000, legend = c("Net Generation Time Series", 
                               "Trend and Seasonality Model for Training Data",
                               "Trend and Seasonality Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020.25 - 4.25, 2020.25 - 4.25), c(0, 450000))
lines(c(2020.25, 2020.25), c(0, 450000))
text(2007.25, 450000, "Training")
text(2018.25, 450000, "Validation")
text(2021.75, 450000, "Future")
arrows(2020.25 - 4.25, 445000, 2000.6, 445000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25 - 4.25, 445000, 2020.25, 445000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25, 445000, 2022.75, 445000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# 5b. Plot residuals of predictions with trend and seasonality.
train.lintrend.season.pred$residuals
min(train.lintrend.season.pred$residuals)
max(train.lintrend.season.pred$residuals)

valid.ts - train.lintrend.season.pred$mean
min(valid.ts - train.lintrend.season.pred$mean)
max(valid.ts - train.lintrend.season.pred$mean)

plot(train.lintrend.season.pred$residuals, 
     xlab = "Time", ylab = "Net Generation (in thousand mwh)", ylim = c(-50000, 50000), bty = "l",
     xaxt = "n", xlim = c(2001, 2022.25), main = "Residuals for Trend and Seasonality Model", 
     col = "brown", lwd = 2) 
axis(1, at = seq(2001, 2022, 1), labels = format(seq(2001, 2022, 1)))
lines(valid.ts - train.lintrend.season.pred$mean, col = "brown", lty = 1, lwd=2)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020.25 - 4.25, 2020.25 - 4.25), c(-50000, 50000))
lines(c(2020.25, 2020.25), c(-50000, 50000))
text(2007.25, 45000, "Training")
text(2018.25, 45000, "Validation")
text(2021.75, 45000, "Future")
arrows(2020.25 - 4.25, 40000, 2000.6, 40000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25 - 4.25, 40000, 2020.25, 40000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25, 40000, 2022.75, 40000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use accuracy() function to identify common accuracy measures
# for various regression models: (1)linear trend, (2) Exponential trend, (3) Quadratic trend,  
# (4) quadratic trend and seasonality and (5) Linear trend and seasonality
round(accuracy(train.lin.pred, valid.ts),3)
round(accuracy(train.expo.pred, valid.ts), 3)
round(accuracy(train.quad.pred, valid.ts),3)
round(accuracy(train.qtrend.season.pred, valid.ts),3)
round(accuracy(train.lintrend.season.pred, valid.ts),3)

# All comparisons with partitioning is done only with the validation period
# Model 5 has the lowest MAPE & RMSE. Definitely this model 5 is better one

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## MODEL 6 : FIT REGRESSION MODEL WITH EXPONENTIAL TREND AND SEASONALITY. 

# Use tslm() function to create exponential trend and seasonal model.
train.expotrend.season <- tslm(train.ts ~ trend + season, lambda = 0)
# See summary of exponential trend and seasonality model and asociated parameters.
summary(train.expotrend.season)
# Apply forecast() function to make predictions for validation set.  
train.expotrend.season.pred <- forecast(train.expotrend.season, h = nValid, level = 0)
train.expotrend.season.pred

# 6a.Plot ts data, trend and seasonality data, and predictions for validation period.
plot(train.expotrend.season.pred, 
     xlab = "Time", ylab = "Net Generation (in thousand mwh)", ylim = c(250000, 450000), bty = "l",
     xaxt = "n", xlim = c(2001, 2022.25), main = "Model with Exponential Trend and Monthly Seasonality", 
     flty = 5, lwd = 2) 
axis(1, at = seq(2001, 2022, 1), labels = format(seq(2001, 2022, 1)))
lines(train.expotrend.season.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lty = 1, lwd = 2)
lines(train.ts, col = "black", lty = 1, lwd = 2)
legend(2002,450000, legend = c("Net Generation Time Series", 
                               "Trend and Seasonality Model for Training Data",
                               "Trend and Seasonality Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020.25 - 4.25, 2020.25 - 4.25), c(0, 450000))
lines(c(2020.25, 2020.25), c(0, 450000))
text(2007.25, 450000, "Training")
text(2018.25, 450000, "Validation")
text(2021.75, 450000, "Future")
arrows(2020.25 - 4.25, 445000, 2000.6, 445000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25 - 4.25, 445000, 2020.25, 445000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25, 445000, 2022.75, 445000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# 6b. Plot residuals of predictions with trend and seasonality.
train.expotrend.season.pred$residuals
min(train.expotrend.season.pred$residuals)
max(train.expotrend.season.pred$residuals)

valid.ts - train.expotrend.season.pred$mean
min(valid.ts - train.expotrend.season.pred$mean)
max(valid.ts - train.expotrend.season.pred$mean)

plot(train.expotrend.season.pred$residuals, 
     xlab = "Time", ylab = "Net Generation (in thousand mwh)", ylim = c(-50000, 50000), bty = "l",
     xaxt = "n", xlim = c(2001, 2022.25), main = "Residuals for Quadratic Trend and Seasonality Model", 
     col = "brown", lwd = 2) 
axis(1, at = seq(2001, 2022, 1), labels = format(seq(2001, 2022, 1)))
lines(valid.ts - train.expotrend.season.pred$mean, col = "brown", lty = 1, lwd=2)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020.25 - 4.25, 2020.25 - 4.25), c(-50000, 50000))
lines(c(2020.25, 2020.25), c(-50000, 50000))
text(2007.25, 45000, "Training")
text(2018.25, 45000, "Validation")
text(2021.75, 45000, "Future")
arrows(2020.25 - 4.25, 40000, 2000.6, 40000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25 - 4.25, 40000, 2020.25, 40000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25, 40000, 2022.75, 40000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use accuracy() function to identify common accuracy measures
# for various regression models: (1)linear trend, (2) Exponential trend, (3) Quadratic trend,  
# (4) quadratic trend and seasonality (5) Linear trend and seasonality
# and (6) Exponential trend and seasonality
round(accuracy(train.lin.pred, valid.ts),3)
round(accuracy(train.expo.pred, valid.ts), 3)
round(accuracy(train.quad.pred, valid.ts),3)
round(accuracy(train.season.pred, valid.ts),3)
round(accuracy(train.qtrend.season.pred, valid.ts),3)
round(accuracy(train.lintrend.season.pred, valid.ts),3)
round(accuracy(train.expotrend.season.pred, valid.ts),3)
# All comparisons with partitioning is done only with the validation period
# Model 6 has the lowest MAPE & RMSE. Definitely this model 6 is better one

#*********************************************************************************************#
# 9. ARIMA (Autoregressive Integrated Moving Average) MODELS
#*********************************************************************************************#

## MODEL 1 : FIT ARIMA(2,1,2)(1,1,2) MODEL - Randomly selected values for (p,d,q)(P,D,Q)

# Use Arima() function to fit ARIMA(2,1,2)(1,1,2) model for net electricity generation data.
# We need not specify m : number of seasons because train.ts itself gets to know its a monthly seasonality.
train.arima <- Arima(train.ts, order = c(2,1,2), seasonal = c(1,1,2))
summary(train.arima)
# RESULT: 1. ar1 & ar2 - 2 autoregressive variables for level & trend
# 2. ma1 & ma2 - 2 MA variables for level & trend
# 3. sar1 - 1 seasonal autoregressive variable for seasonal component
# 4. sma1 & sma2 - 2 MA variables for modelling residuals for seasonal component
# 5. 1st order differencing for seasonal components

# Apply forecast() function to make predictions for ts with ARIMA(2,1,2)(1,1,2) model in validation set.    
train.arima.pred <- forecast(train.arima, h = nValid, level = 0)
train.arima.pred

# Plot ts data, ARIMA model, and predictions for validation period.
plot(train.arima.pred, 
     xlab = "Time", ylab = "Net Generation (in thousand mwh)", ylim = c(250000, 450000), bty = "l",
     xaxt = "n", xlim = c(2001, 2022.25),
     main = "ARIMA(2,1,2)(1,1,2)[12] Model", lwd = 2, flty = 5) 
axis(1, at = seq(2001, 2022, 1), labels = format(seq(2001, 2022, 1)))
lines(train.arima.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(2002,450000, legend = c("Net Generation Time Series", "ARIMA Model for Training Period",
                               "ARIMA Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020.25 - 4.25, 2020.25 - 4.25), c(0, 450000))
lines(c(2020.25, 2020.25), c(0, 450000))
text(2007.25, 450000, "Training")
text(2018.25, 450000, "Validation")
text(2021.75, 450000, "Future")
arrows(2020.25 - 4.25, 445000, 2000.6, 445000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25 - 4.25, 445000, 2020.25, 445000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25, 445000, 2022.75, 445000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
# PLOT RESULT: 1. This model is much more precise interms of measuring historical data as opposed to previous models.
# 2. It really fits well into the historical data in both training & validation periods.

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## MODEL 2 : FIT AUTO ARIMA MODEL

# Use auto.arima() function to fit ARIMA model for Net Electricity Generation.
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)
# RESULT: Gives optimal ARIMA model of the order (1,0,1)(1,1,2)[12]
# 1. ar1 - 1 autoregressive variables for level & trend
# 2. ma1 - 1 MA variables for level & trend
# 3. 0 differencing for level & trend
# 3. sar1 - 1 seasonal autoregressive variable for seasonal component
# 4. sma1 & sma2 - 2 MA variables for modelling residuals for seasonal component
# 5. 1st differencing for seasonal component & 12 seasons
# 6. Model does not have any intercept as it is a 1st order differencing model , 
# so we just have the difference between the successive data points (yt - yt-1)

# Apply forecast() function to make predictions for ts with auto ARIMA model in validation set.  
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred

# Plot ts data, trend and seasonality data, and predictions for validation period.
plot(train.auto.arima.pred, 
     xlab = "Time", ylab = "Net Generation (in thousand mwh)", ylim = c(250000, 450000), bty = "l",
     xaxt = "n", xlim = c(2001, 2022.25), 
     main = "Auto ARIMA Model", lwd = 2, flty = 5) 
axis(1, at = seq(2001, 2022, 1), labels = format(seq(2001, 2022, 1)))
lines(train.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(2002,450000, legend = c("Net Generation Time Series", "Auto ARIMA Model for Training Period",
                               "Auto ARIMA Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020.25 - 4.25, 2020.25 - 4.25), c(0, 450000))
lines(c(2020.25, 2020.25), c(0, 450000))
text(2007.25, 450000, "Training")
text(2018.25, 450000, "Validation")
text(2021.75, 450000, "Future")
arrows(2020.25 - 4.25, 445000, 2000.6, 445000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25 - 4.25, 445000, 2020.25, 445000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25, 445000, 2022.75, 445000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

# Use accuracy() function to identify common accuracy measures for validation period forecast
# (1) ARIMA(2,1,2)(1,1,2) model and (2) Auto ARIMA model.
round(accuracy(train.arima.pred, valid.ts), 3)
round(accuracy(train.auto.arima.pred, valid.ts), 3)

#*********************************************************************************************#
# 10. MODEL SELECTION FOR THE IMPLEMENTATION
#*********************************************************************************************#

# VALIDATION ACCURACY MEASURES
round(accuracy(netgen.naive.pred$mean, valid.ts), 3)
round(accuracy(netgen.snaive.pred$mean, valid.ts), 3)
round(accuracy(ses.popt.pred, valid.ts),3)
round(accuracy(h.ZZN.pred, valid.ts), 3)
round(accuracy(hw.ZZZ.pred, valid.ts), 3)
round(accuracy(train.expotrend.season.pred, valid.ts),3)
round(accuracy(train.auto.arima.pred, valid.ts), 3)

#*********************************************************************************************#
# 11. MODEL IMPLEMENTATION - FORECASTING FOR FUTURE PERIODS USING ENTIRE DATASET
#*********************************************************************************************#

# Re-Run the above chosen time series models on the entire Net Electricity Generation dataset

# MODEL 1 : SIMPLE EXPONENTIAL SMOOTHING WITH ADDITIVE ERROR AND OPTIMAL ALPHA

ses.popt.imp <- ets(netgen.ts, model = "ANN")
ses.popt.imp
# Using forecast() function to make predictions for future 12 periods 
ses.popt.pred.imp <- forecast(ses.popt.imp, h = 12, level = c(80, 95))
ses.popt.pred.imp
# Model Accuracy
round(accuracy(ses.popt.pred.imp$fitted, netgen.ts),3)

## PLOT ORIGINAL DATA AND SES FUTURE FORECAST.

# Plot ts data, SES model and predictions for future 12 periods.
plot(netgen.ts, 
     xlab = "Time", ylab = "Net Generation (in thousand mwh)", ylim = c(250000, 450000), bty = "l",
     xaxt = "n", xlim = c(2001, 2022.25), lwd = 2,
     main = "SES Model with Additive Error & Optimal Alpha for Entire Dataset") 
axis(1, at = seq(2001, 2022, 1), labels = format(seq(2001, 2022, 1)))
lines(ses.popt.imp$fitted, col = "blue", lwd = 2)
lines(ses.popt.pred.imp$mean, col = "blue", lty = 5, lwd = 2)
legend(2002,450000, legend = c("Net Generation Time Series", "SES Model for Training Data",
                               "SES Model Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# plot vertical lines and horizontal arrows describing training and future prediction intervals.
lines(c(2020.25, 2020.25), c(0, 450000))
text(2007.25, 450000, "Training")
text(2021.75, 450000, "Future")
arrows(2020.25, 445000, 2000.6, 445000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25, 445000, 2022.75, 445000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

# MODEL 2 - HOLT'S MODEL ZZN i.e, optimal error(M), optimal trend (M), & no seasonality

h.ZZN.imp <- ets(netgen.ts, model = "ZZN")
h.ZZN.imp
# Use forecast() function to make predictions using this Holt's model for future 12 periods. 
h.ZZN.pred.imp <- forecast(h.ZZN.imp, h = 12, level = c(80, 95))
h.ZZN.pred.imp
# Model Accuracy
round(accuracy(h.ZZN.pred.imp$fitted, netgen.ts), 3)

# Plot ts data, Holt's model and predictions for future 12 periods.
plot(netgen.ts, 
     xlab = "Time", ylab = "Net Generation (in thousand mwh)", ylim = c(250000, 450000), bty = "l",
     xaxt = "n", xlim = c(2001, 2022.25), lwd = 2,
     main = "Holt's ZZN Model with Optimal Parameters for Entire Dataset") 
axis(1, at = seq(2001, 2022, 1), labels = format(seq(2001, 2022, 1)))
lines(h.ZZN.imp$fitted, col = "blue", lwd = 2)
lines(h.ZZN.pred.imp$mean, col = "blue", lty = 5, lwd = 2)
legend(2002,450000, legend = c("Net Generation Time Series", "Holt's Model for Training Data",
                               "Holt's Model Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# plot vertical lines and horizontal arrows describing training and future prediction intervals.
lines(c(2020.25, 2020.25), c(0, 450000))
text(2007.25, 450000, "Training")
text(2021.75, 450000, "Future")
arrows(2020.25, 445000, 2000.6, 445000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25, 445000, 2022.75, 445000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

# MODEL 3 - HOLT-WINTER'S MODEL ZZZ i.e.,ALL OPTIMAL HW MODEL FOR ENTIRE DATASET

hw.ZZZ.imp <- ets(netgen.ts, model = "ZZZ")
hw.ZZZ.imp
# Use forecast() function to make predictions using this HW model for future 12 periods.
hw.ZZZ.pred.imp <- forecast(hw.ZZZ.imp, h = 12, level = c(80, 95))
hw.ZZZ.pred.imp
# Model Accuracy
round(accuracy(hw.ZZZ.pred.imp$fitted, netgen.ts), 3)

# Plot ts data, Holt-Winter's model and predictions for future 12 periods.
plot(hw.ZZZ.pred.imp, 
     xlab = "Time", ylab = "Net Generation (in thousand mwh)", ylim = c(250000, 450000), bty = "l",
     xaxt = "n", xlim = c(2001, 2022.25), lwd = 2,
     main = "Holt-Winter's ZZZ Model with Optimal Parameters for Entire Dataset",flty = 5) 
axis(1, at = seq(2001, 2022, 1), labels = format(seq(2001, 2022, 1)))
lines(hw.ZZZ.imp$fitted, col = "blue", lwd = 2)
lines(hw.ZZZ.pred.imp$mean, col = "blue", lty = 5, lwd = 2)
legend(2002,450000, legend = c("Net Generation Time Series", "Holt-Winter's Model for Training Data",
                               "Holt-Winter's Model Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# plot vertical lines and horizontal arrows describing training and future prediction intervals.
lines(c(2020.25, 2020.25), c(0, 450000))
text(2007.25, 450000, "Training")
text(2021.75, 450000, "Future")
arrows(2020.25, 445000, 2000.6, 445000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25, 445000, 2022.75, 445000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

# MODEL 4 - REGRESSION MODEL WITH EXPONENTIAL TREND AND SEASONALITY FOR ENTIRE DATASET.

train.expotrend.season.imp <- tslm(netgen.ts ~ trend + season, lambda = 0)
summary(train.expotrend.season.imp)
# Apply forecast() function to make predictions using regression model for future 12 periods.
train.expotrend.season.pred.imp <- forecast(train.expotrend.season.imp, h = 12, level = c(80, 95))
train.expotrend.season.pred.imp
# Model Accuracy
round(accuracy(train.expotrend.season.pred.imp$fitted, netgen.ts),3)

# Plot ts data, Regression model and predictions for future 12 periods.
plot(train.expotrend.season.pred.imp,
     xlab = "Time", ylab = "Net Generation (in thousand mwh)", ylim = c(250000, 450000), bty = "l",
     xaxt = "n", xlim = c(2001, 2022.25), lwd = 2,
     main = "Regression Model with Exponential Trend and Seasonality for Entire Dataset",flty = 5) 
axis(1, at = seq(2001, 2022, 1), labels = format(seq(2001, 2022, 1)))
lines(netgen.ts)
lines(train.expotrend.season.imp$fitted, col = "blue", lwd = 2)
lines(train.expotrend.season.pred.imp$mean, col = "blue", lty = 5, lwd = 2)
legend(2002,450000, legend = c("Net Generation Time Series", "Regression Model for Training Data",
                               "Regression Model Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# plot vertical lines and horizontal arrows describing training and future prediction intervals.
lines(c(2020.25, 2020.25), c(0, 450000))
text(2007.25, 450000, "Training")
text(2021.75, 450000, "Future")
arrows(2020.25, 445000, 2000.6, 445000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25, 445000, 2022.75, 445000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

# MODEL 5 - AUTO ARIMA MODEL FOR ENTIRE DATASET. 

train.auto.arima.imp <- auto.arima(netgen.ts)
summary(train.auto.arima.imp)
# Apply forecast() function to make predictions using this Arima model for future 12 periods.
train.auto.arima.pred.imp <- forecast(train.auto.arima.imp, h = 12, level = c(80, 95))
train.auto.arima.pred.imp
# Model Accuracy 
round(accuracy(train.auto.arima.pred.imp$fitted, netgen.ts), 3)

# Plot ts data, AUTO ARIMA model, and predictions for future 12 periods.
plot(train.auto.arima.pred.imp, 
     xlab = "Time", ylab = "Net Generation (in thousand mwh)", ylim = c(250000, 450000), bty = "l",
     xaxt = "n", xlim = c(2001, 2022.25), lwd = 2,
     main = "Auto ARIMA Model for Entire Dataset", flty = 5) 
axis(1, at = seq(2001, 2022, 1), labels = format(seq(2001, 2022, 1)))
lines(train.auto.arima.imp$fitted, col = "blue", lwd = 2)
lines(train.auto.arima.pred.imp$mean, col = "blue", lty = 5, lwd = 2)
legend(2002,450000, legend = c("Net Generation Time Series", "Auto ARIMA Model for Training Data",
                               "Auto ARIMA Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# plot vertical lines and horizontal arrows describing training and future prediction intervals.
lines(c(2020.25, 2020.25), c(0, 450000))
text(2007.25, 450000, "Training")
text(2021.75, 450000, "Future")
arrows(2020.25, 445000, 2000.6, 445000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25, 445000, 2022.75, 445000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

# PERFORMANCE MEASURES OF FORECASTING MODELS WITH ENTIRE NET ELECTRICITY GENERATION DATASET

round(accuracy((naive(netgen.ts))$fitted, netgen.ts), 3)
round(accuracy((snaive(netgen.ts))$fitted, netgen.ts), 3)
round(accuracy(ses.popt.pred.imp$fitted, netgen.ts),3)
round(accuracy(h.ZZN.pred.imp$fitted, netgen.ts), 3)
round(accuracy(hw.ZZZ.pred.imp$fitted, netgen.ts), 3)
round(accuracy(train.expotrend.season.pred.imp$fitted, netgen.ts),3)
round(accuracy(train.auto.arima.pred.imp$fitted, netgen.ts), 3)