#Title: Logistic Regression with Model Estimation Sample 
#-------------------------------------------------------

# This script uses the sample with customers who have received a direct mail.
# Thus the sample contains information about whether or not someone responded to the mail campaign.
# We will use this information to estimate a logistic regression.


#-----------------------------------
#Set working directory and load data
#-----------------------------------

#Sets working directory
setwd("C:/Users/deu06/Desktop/Customer Anaylsis")

#pulls data from a csv file
Sample1<-read.csv("C:/Users/deu06/Desktop/Customer Anaylsis/Assignment2_2022_data.csv", header=TRUE)

#Get an overview of the variables 
names(Sample1) 


attach(Sample1) #"activates" the data set for easier access to the variables

summary(Sample1)


#-------------------------------------------
# Install and load required packages
#-------------------------------------------

install.packages("DescTools")
library(DescTools)

install.packages("ResourceSelection")
library("ResourceSelection")
install.packages("car")
library(car)

# install package “data.table”
install.packages("data.table", type= "binary")

# load package “car” and “data.table”
library(data.table)


# -----------------------------------------------------------
# Step 1: Define research goal
# This is task 2
# -----------------------------------------------------------
# Identify customers who are most likely to respond to the direct mail campaign.

# -----------------------------------------------------------
# Step 2: Specify the model 
# This is task 3
# -----------------------------------------------------------

# Let's start with a full model including all information we have 
# excluding deposits as these are irrelevant (only 2 customers)


model1<-glm(Churn_Yes ~ tenure + MonthlyCharges + Dependents_Yes + MultipleLines_Yes + InternetService_DSL + 
              PaymentMethod_CreditCard, data=Sample1, family=binomial)

# The glm() function helps us to specify a generalized linear model.
# GLMs are a class of models that deal with non-normally distributed outcomes, e.g., yes or no.
# Since there are other forms of non normally-distributed outcomes than just binary ones,
# the glm function requires us to specify the family. Here it is binomial (i.e., just two values, response yes or no)
# Model specification is the same as in a standard linear regression.
# Note that we get an error message so we should take this model with caution.

#------------------------------------------------------------
# Step 3: Model Evaluation
# This is task 4
#------------------------------------------------------------

# Global model evaluation

anova(model1, test="Chisq")

# Omnibus tests/ANOVA tests whether 
# the variance in a set of data explained by the model is significantly greater than 
# the unexplained variance.
# Thus, we want this test to be significant (p<.05).
# The test proceeds in a step wise manner, adding one of the independent variables
# in each step. We are only interested in the value in the last step.
# Here it is significant so ok.

h1<-hoslem.test(Churn_Yes, fitted(model1))
h1
# Hosmer Lemeshow test whether the predicted values and the actual values are significantly different.
# "respons" identifies the observed binary, "fitted"model" the predicted.
# The test partitions the data into groups and compares for each one 
# whether there are differences between predictions and observations.
# "g=10" is th edefault choice for the number of groups the test uses.
# For a good model performance we want them to be NOT different.
# Thus, we want the Hosmer Lemeshow Test to be INSIGNIFICANT (>.05)!
# Here it is significant so not ideal.

PseudoR2(model1, which = "Nagelkerke") 
# There is no meaningful R-Square godness of fit measure as it is in linear regression.
# Thus we use Pseudo R-square measures. Here we choose Nagelkerke R-Square).
# It is always between 0 and 1. The higher the better.
# Here it could be better or worse. We could live with that.

# Local model evaluation, i.e., model parameters

summary(model1)
# We see that many model parameters are not significant
# This is not a problem per se but in our case we may have overspecified the model.
vif(model1)
# some VIFs are also a bit high (>5) 

# All in all we see that the model does not perform so well 
# in terms of Hosmer Lemeshow test (significant) 
# and also in terms of coefficients, 
# many of which are insignificant and/or multicollinear (according to VIFs).
# So let's specify a simpler model, where we only use the significant coefficients.
# Note, however, that we need at least one metric variable in the model, otherwise we would have to use another class of models.


exp(coef(model1))   # Remember, an odds ratio is exp(coefficient).
# Reading example: Inest has an odds value of 6.56,
# which means that someone who holds the Invest-product
# is 6.56 times as likely to respond as someone who does not hold it. 

# We also might want to look at standardized coefficients 
install.packages("reghelper")
library("reghelper")          # to see which variable has the strongest effect.
model1.std <- beta(model1) # We can do that with the beta function from the reghelper PAckage
model1.std

#--------------------------------------------
# Step 4: Define classification cut-off values
# This is task 5
#--------------------------------------------
              # 1$ Dollar per mail, $50 per positive response.
# That is, minimum to avoid loss is 1/50 = .02
Conf(model1, cutoff = 0.1) # We ask for a classification table.

##### Model from lecture ##############
# model2<-glm(respons ~ Invest + Damage + Auto  + trans, data=Sample1, family=binomial)
# anova(model2, test="Chisq")
# h<-hoslem.test(respons, fitted(model2), g=10)
# h
# PseudoR2(model2, which = "Nagelkerke") 
# summary(model2)
# vif(model2)
# Conf(model2, cutoff = 0.03)
######################################


#---------------------------------------------
# Step 5: Predictions
# This is task 6
#---------------------------------------------
# We pull the sample for the customers from which we should choose the prospects using our model
 # Counts the rows so we see that we have 4809 customers in the prediction sample

# Next we apply the model to predict the probabilities
prediction<-predict(model1, newdata=Sample1, type = "response")
# model2 is our fitted model
# newdata= specifies the sample that shall be used
# type = "response" clarifies that we want the response probabilities


# We find out how much we should spend and what the expected profit is.
# This is task 6

cutoff_02<-ifelse(prediction>=0.1, 1, 0)
# We use the appropriate cut-off to identify prospects a "1", all others a "0".
sum(cutoff_02)      # counting all 1s tells us that there are 991 prospects in the sample
# As we should only contact the prospects, we would spend $991.

prediction.sorted<-sort(prediction, decreasing = TRUE)   
prediction.sorted[1:50]
# We sort the customers based on decreasing response probabilities.

prospect.probabilities<-prediction.sorted[1:991]
prospect.probabilities[1:50]
# We only choose the first 991 (as those are the prospects)

expected.profits<-prospect.probabilities*50-1
sum(expected.profits)
# And for those we calculate the expected profits,
# which are $1901.512.
# Remember: In the initial sample with the untargeted campaign,
# our profit amounted to $659 only.
# But ATTENTION: Ideally the estimation sample and the prediction sample have the same size.
# Since the samples are not exactly the same size in our case, the comparison is not perfect! 
# Nevertheless the substantial difference between the 1901.51 and 659 indicate 
# that the model could substantially improve performance. 
# Next steps in real life would be to target a campaign to the prospects only and then compare the actual with the predicted results.

detach(Sample1)
