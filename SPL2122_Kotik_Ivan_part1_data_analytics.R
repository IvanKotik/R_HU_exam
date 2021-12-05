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

### Exercise 2 -----------------------------------------------------------------

# a) Reading the csv file into a variable
datafile <- read.csv("C://Users//qwerty//Downloads//mortgage.csv")


# b) Rename the variable interest to Fixed interest rate.
names(datafile)[6] <- "Fixed interest rate"


# c) What is the number of missing values in tdiff
datafile[datafile[, "tdiff"] == "", "tdiff"]  # the value is 0 as can be seen by the result "numeric(0)"


# d) Compute the 3. quartile and the interquartile range of the variable networth.
#    Create a table with absolute frequencies of the variable rate.

#    Calculating quartiles (also can be done by summary(datafile$networth)[2] (...))
quantile(datafile$networth)[2] # 0.3585
quantile(datafile$networth)[4]-quantile(datafile$networth)[2] # 7.199

#    The absolute frequencies table
Absfreq <- table(datafile$rate)


# e) What is the number of observations in liability which are less or equal than 7.03?
#    What is the percentage of observations in rate which have the value adjustable?
length(datafile[datafile[, "liability"] <= 7.03, "liability"])
Absfreq["adjustable"]/(sum(Absfreq))  # 41.02564% of the values in "RATE" have the value "ADJUSTABLE"


# f) Compute the Contingency coeffient between the variables first and coborrower.
#    (Hint: To determine the chi^2 value use chisq.test(...)$statistic.)
chi <- chisq.test(datafile$first, datafile$coborrower)
sqrt(chi$statistic / (chi$statistic + nrow(datafile)))  # the Contingency coeffient is 0.1483548


# g) Create a new categorical variable liquid_cat from liquid with 1 if the observed value is smaller equal 0.71,
#    2 if the observed value is in (0:71; 5:01], and 3 if the observed value is larger than 5.01.
#    Comment about the cut values; are they choosen sensibly?
#    Create a contingency table from liquid_cat and coborrower with the relative frequencies

#    creating the liquid_cat variable
for (i in (1 : nrow(datafile))){
  if ((datafile$liquid[i] <= 0.71) == TRUE){
    datafile$liquid_cat[i] <- 1
  } else if (((0.71 < datafile$liquid[i]) & (datafile$liquid[i] <= 5.01)) == TRUE){
    datafile$liquid_cat[i] <- 2
  } else if ((5.01 < datafile$liquid[i]) == TRUE){
    datafile$liquid_cat[i] <- 3
  }
}
datafile$liquid_cat <- factor(datafile$liquid_cat)

#    checking the distribution of the liquid variable
summary(datafile$liquid)
boxplot(datafile$liquid, horizontal = TRUE)
#    checking the categorical variable
summary(factor(datafile$liquid_cat))
boxplot(datafile$liquid ~ datafile$liquid_cat, horizontal = TRUE)
#    commentary: it seems that the chosen cut values are on the right way to provide an informative grouping
#    of the liquid variable due to a good balance between number of observations inside groups and the
#    underlying distributions of those observations if the task is to find 3 categorical groups

#    creating a contingency table
table(datafile$liquid_cat, datafile$coborrower)


# h) Compute the mean and the variance of the variable liquid for each subgroup defined by coborrower
data.frame("mean liquid" = c(mean(datafile[datafile[, "coborrower"] == "yes", "liquid"]),
            mean(datafile[datafile[, "coborrower"] == "no", "liquid"])),
            "liquid var" = c(var(datafile[datafile[, "coborrower"] == "yes", "liquid"]),
            var(datafile[datafile[, "coborrower"] == "no", "liquid"])),
            row.names = c("coborrower yes", " coborrower no"))


# i) Run a statistical test on the percentage of yes in married. The null hypothesis should be
#    H0 : Pi = 0:4. Verbalize the null hypothesis. What does the p-value tell you?
datafile[datafile["married"] == "yes", "married"]
prop.test(length(datafile[datafile["married"] == "yes", "married"]), nrow(datafile), p = 0.4)
#    The null hypothesis of the test would be stating that there is no significant differnce in
#    proportions between the observed group (yes-to-total) and the proportion 0.4-to-1;
#    Alternatively this can be stated that the test checks if the proportion in the group is equal to
#    the value 0.4. The test results with the Chi squared statistic of 12.505 with 1 df, which
#    yields a 0.0004059 p-value, which gives us reason to reject the null hypothesis of no difference
#    between the group proportion and 0.4 with a critical value at least as small as 0.005.
#    Conclusion: the percentage of yes is not 40%.


# j) Run a simple linear regression with liability (dependent variable) and liquid (independent variable).
#    How good is your regression? What does the regression slope tell you? Do you think that this
#    regression makes sense?

model <- lm(datafile$liability ~ datafile$liquid)
summary(model)
#    There regression seems to be a poor fit to accurately predict short term liabilities by liquid assets
#    due to a fairly small R-squared/Adjusted R-squared values
#    The regression slope, as seen by model$coefficients[2] indicates, that with every unit increase of
#    liquid assets there is a 0.8242 unit increase of short term liabilities.
#    The regression indicates there is sufficient evidence to reject the hypothesis that the relationship
#    between liquid assets and short term liabilities is just due to chance.