##################################################
#
# QMB 6912 Capstone Project in Business Analytics
#
# Blinder-Oaxaca Decomposition
#
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business Administration
# University of Central Florida
#
# March 29, 2021
#
##################################################
#
# Tractor_Blinder_Oaxaca gives examples of the
#   Blinder-Oaxaca decomposition.
#
##################################################

##################################################
# Preparing the Workspace
##################################################

# Clear workspace.
rm(list=ls(all=TRUE))

# Load package for the Blinder-Oaxaca decomposition.
library(oaxaca)


##################################################
# Setting the Parameters
##################################################


# Set path for working directory.
setwd("C:/Users/le279259/Documents/Teaching/QMB6911_Summer_2021/SoftwareDemo/QMB6911S21/Nonparametrics")

getwd()


##################################################
# Loading the Data
##################################################

tractor_sales <- read.csv('TRACTOR7.csv')

# Inspect the contents.
summary(tractor_sales)
# Make sure there are no problems with the data.


##################################################
# Generating New Variables
##################################################

#--------------------------------------------------
# Standard transformations for this regression model
#--------------------------------------------------

hist(tractor_sales[, 'saleprice'])
# Notice that there are some very large values.
# Consider taking logs to bring outliers closer to the others.

tractor_sales[, 'log_saleprice'] <- log(tractor_sales[, 'saleprice'])

# Now plot the histogram for log of saleprice:
hist(tractor_sales[, 'log_saleprice'])
# Much better behaved. Looks almost normal.


# Create a variable squared_horsepower
# to investigate quadratic relationship of sale price to horsepower.
tractor_sales[, 'squared_horsepower'] <- tractor_sales[, 'horsepower']^2

#--------------------------------------------------
# Data transformations for the oaxaca function
#--------------------------------------------------

# Variables d1 + d2 + d3 + ... in argument 'formula'
# must indicate membership in mutually exclusive categories.

# d_var_list <- c('diesel', 'fwd', 'manual', 'cab')
# tractor_sales[, 'DFMC'] <- ...

# In what follows, I will use the dummy variables
# as covariates.

##################################################
# Linear Regression Models
##################################################

#--------------------------------------------------
# Best model from last week
# Model 7: Linear model for log of dollar sale price
# With quadratic form for horsepower
# Omit seasonal indicators
#--------------------------------------------------


# Estimate the regression model.
lm_model_7 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + squared_horsepower +
                   age + enghours +
                   diesel + fwd + manual + johndeere + cab)

# Output the results to screen.
summary(lm_model_7)

#--------------------------------------------------
# Estimate separately for John Deere tractors
#--------------------------------------------------

# Model for John Deere tractors:

# Estimate the regression model.
lm_model_jd <- lm(data = tractor_sales[tractor_sales[, 'johndeere'] == 1, ],
                  formula = log_saleprice ~ horsepower + squared_horsepower +
                    age + enghours +
                    diesel + fwd + manual + cab)

# Output the results to screen.
summary(lm_model_jd)


# Model excluding John Deere tractors:

# Estimate the regression model.
lm_model_njd <- lm(data = tractor_sales[tractor_sales[, 'johndeere'] == 0, ],
                  formula = log_saleprice ~ horsepower + squared_horsepower +
                    age + enghours +
                    diesel + fwd + manual + cab)

# Output the results to screen.
summary(lm_model_njd)


##################################################
# Blinder-Oaxaca Decomposition
##################################################

# tractor_oaxaca <- oaxaca(data = tractor_sales,
#                          formula = log_saleprice ~
#                            horsepower + squared_horsepower +
#                            age + enghours |
#                            johndeere |
#                            diesel) # + fwd + manual + cab)

tractor_oaxaca <- oaxaca(data = tractor_sales,
                         formula = log_saleprice ~
                           horsepower + squared_horsepower +
                           age + enghours +
                           diesel + fwd + manual + cab |
                           johndeere)

plot(tractor_oaxaca)

# Try again with fewer variables.
# Cut out diesel and manual.
tractor_oaxaca <- oaxaca(data = tractor_sales,
                         formula = log_saleprice ~
                           horsepower + squared_horsepower +
                           age + enghours +
                           # diesel + fwd + manual + cab |
                           fwd + cab |
                           johndeere)

# Plot with interactions to separate unexplained variance
# into coefficients and interaction.
plot(tractor_oaxaca)


# Interactions do not appear to be too important,
# so plot with all unexplained variation together.
plot(tractor_oaxaca, decomposition = "twofold",
     group.weight = -1)


##################################################
# Making sense of the differences
##################################################

# Cross-tabulate binary variables by John Deere brand.
# diesel + fwd + manual + cab
table(tractor_sales[, 'johndeere'],
      tractor_sales[, 'diesel'], useNA = 'ifany')
table(tractor_sales[, 'johndeere'],
      tractor_sales[, 'fwd'], useNA = 'ifany')
table(tractor_sales[, 'johndeere'],
      tractor_sales[, 'manual'], useNA = 'ifany')
table(tractor_sales[, 'johndeere'],
      tractor_sales[, 'cab'], useNA = 'ifany')


# Summarize continuous variables by John Deere brand.
cts_var_list <- c('horsepower', 'age', 'enghours')

summary(tractor_sales[tractor_sales[, 'johndeere'] == 1, cts_var_list])

summary(tractor_sales[tractor_sales[, 'johndeere'] == 0, cts_var_list])

# We might have done this sort of analysis first.


##################################################
# End
##################################################

# Test: Try switching directions.
tractor_sales[, 'not_johndeere'] <- 1 - tractor_sales[, 'johndeere']


tractor_oaxaca_njd <- oaxaca(data = tractor_sales,
                         formula = log_saleprice ~
                            horsepower + squared_horsepower +
                            age + enghours +
                            # diesel + fwd + manual + cab |
                            fwd + cab |
                            not_johndeere)



# Interactions do not appear to be too important,
# so plot with all unexplained variation together.
plot(tractor_oaxaca_njd, decomposition = "twofold",
     group.weight = -1)

# See: All the bars switched direction.
