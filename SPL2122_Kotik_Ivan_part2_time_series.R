################################################################################
################## Statistical Programming Languages 2021/22 ###################
##################               Take-home Exam              ###################
##################                                           ###################
##################   	  Sigbert Klinke, Eva-Maria Maier,   ###################
##################       		  Alexander Volkmann         ###################
################################################################################

#-------------------------------------------------------------------------------
# Surname: Kotik
# Name: Ivan
#-------------------------------------------------------------------------------

### Exercise 3 -----------------------------------------------------------------

#   a) Read the data file rappahannock.csv into R.

Sys.setlocale("LC_TIME", "German")
df <- read.csv("C://Users//qwerty//Downloads//rappahannock.csv", encoding = "UTF-8")

#   b) Convert the variable time to a posixct or Date variable
#   converting "Mai 1958" to "01.05.58"
df$time <- sub("Mär 19", "01.03.", df$time)
df$time <- sub("Mai 19", "01.05.", df$time)
df$time <- sub("Okt 19", "01.10.", df$time)
df$time <- sub("Dez 19", "01.12.", df$time)
#   avoiding the 1970 starting count by converting to POSIXlt and subtracting the year by 100
time_new <- as.POSIXlt(df$time, tryFormats = "%d.%m.%y")
time_new$year <- time_new$year - 100
#   saving back into the dataframe
df$time <- as.Date(time_new)

#   c) Compute a exponential trend for streamflow on time. How good is your trend?
#   Do you think that this trend estimation makes sense?

#   changing the commas to dots for adequate numeric work
df$streamflow <- sub(",", ".", df$streamflow)
df$streamflow <- as.numeric(df$streamflow)

#   computing the exponental model
dfts <- ts(df$streamflow, start = c(1911, 1), frequency = 12)
tv <- as.numeric(dfts)
df_new <- data.frame(t = 1:length(tv), xt = tv)
model <- lm(log(df_new$xt) ~ df_new$t)
summary(model)
#   commentary: the trend dosen't do much both visually and looking at the insignificant p value
#   and low R^2. This trend estimation does not make sense.


#   d) Plot the time series and the trend from exercise c). Make sure that titles, axes, labels and so on have
#   useful names and are clearly readable.

#   creating a TS for nice dates


#   plotting the TS and the trend w/ abline
plot(dfts,
     main = "Monthly streamflow measurements at Rappahannock River near Fredericksburg, 1911-1960",
     xlab = "time",
     ylab ="streamflow meassurment, gal. per min")
abline(model, col="red")


#   decomposition
decom <- decompose(dfts)
plot(decom)
