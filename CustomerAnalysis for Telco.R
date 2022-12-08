#--------------------------------------------
#Set working directory, load and prepare data
#--------------------------------------------

#Sets working directory
setwd("C:/Users/deu06/Desktop/Customer Anaylsis/Materials for Group Research Project-20221031")
#pulls data from a csv file
Telco1<-read.csv("C:/Users/deu06/Desktop/Customer Anaylsis/Materials for Group Research Project-20221031/telecom_churn_28Oct2022.csv", header=TRUE)
#Checking data
names(Telco1)
summary(Telco1)

str(Telco1)
# We see that most variables are not specified the way they should be.
# In the analysis that follows correct specifications are useful.
# Thus, we respecify the variable types

Telco1[1:2]<-lapply(Telco1[1:2], as.numeric)
Telco1[3]<-lapply(Telco1[3], as.factor)
Telco1[5]<-lapply(Telco1[5], as.numeric)
Telco1[7]<-lapply(Telco1[7], as.numeric)
# DataPlan, CustServCalls : factor ('Whether or not' data), others consider as a numeric. 
# The analysis below would not work if we use it as a factor.
str(Telco1)   

numb.churn<-table(Telco1$Churn)  
numb.churn                       
barplot(numb.churn) 

#----------------------------------------------------------------------------------
# Identify the relevant indicators for the time and the event of interest 
#----------------------------------------------------------------------------------

library("survival")     
#install.packages("survminer")
library ("survminer") 
#install.packages("reshape2")
library("reshape2")      
library("car")    


#-----------------------------------------------------------------
# Create "survival object", which exemplifies the key dependent information for survival analysis
#-----------------------------------------------------------------

surv.obj<-Surv(Telco1$AccountWeeks, Telco1$Churn)     
surv.obj[1:10]                    
summary(surv.obj)                 # We see that the object contains the time and event information.
summary(Telco1$AccountWeeks)
sum(Telco1$AccountWeeks == '0')

attach(Telco1)


#-----------------------------------------------------------------
# Create Mod0
#-----------------------------------------------------------------
mod0 <- survreg(Surv(AccountWeeks, Churn) ~ 1, data = Telco1) # This is an empty model without any explanatory factors.
summary(mod0)
AIC(mod0)


#-----------------------------------------------------------------
# Visualization for Mod0
#-----------------------------------------------------------------

surv <- seq(.99, .01, by = -.01)
t <- predict(mod0, type = "quantile", p = 1 - surv, newdata = data.frame(1))
surv.df <- data.frame(time = t, surv = surv)
surv.df2 <- data.frame(time = t, surv = surv, upper = NA, lower = NA, std.err = NA)
ggsurvplot_df(fit = surv.df2, surv.geom = geom_line, xlim=c(0,250), break.time.by = 50) 
#the maximum of AccountWeeks = 243



#-----------------------------------------------------------------
# Create Mod1
#-----------------------------------------------------------------
mod1 <- survreg(Surv(AccountWeeks, Churn) ~ DataPlan, data = Telco1) # This is an empty model with one explanatory factors.
summary(mod1)
AIC(mod1)


#Model 2

mod2 <- survreg(Surv(AccountWeeks, Churn) ~ DataPlan + DataUsage  + CustServCalls + 
                  DayMins + DayCalls + MonthlyCharge + OverageFee + RoamMins, data = Telco1) 
summary(mod2)     
vif(mod2)

#Model 3
mod3 <- survreg(Surv(AccountWeeks, Churn) ~ DataPlan + DataUsage  + CustServCalls + 
                   DayMins + DayCalls + OverageFee + RoamMins, data = Telco1) 

summary(mod3) 
AIC(mod3) #LoWer than mod2
vif(mod3)

#Model 4
mod4 <- survreg(Surv(AccountWeeks, Churn) ~ DataPlan + CustServCalls + 
                   DayMins + DayCalls + OverageFee + RoamMins, data = Telco1) 
summary(mod4) 
AIC(mod4) #LoWer than mod 3
vif(mod4)

#Model 5
mod5 <- survreg(Surv(AccountWeeks, Churn) ~ DataPlan + 
                   DayMins + DayCalls + OverageFee + RoamMins, data = Telco1) 
summary(mod5) 
AIC(mod5) #higher than Model 4
vif(mod5)

#Model 4 is the best one

#-----------------------------------------------------------------
# Visualization for Mod1
#-----------------------------------------------------------------

n.dat <- expand.grid(DataPlan = levels(DataPlan)) 
# Creates a dataframe for plotting of two customer prototypes

levels(DataPlan)
n.dat  

surv <- seq(.99, .01, by = -.01)      # To create the curve we want a predicted survival time value 
# for each survival probability between 100 and 0.
a1<-predict(mod1, type = "quantile", p = 1-surv,
            newdata = data.frame(n.dat))
# We predict survival times
dim(a1)               
a1[1:2,1:10]        
a2<-cbind(n.dat, a1) 
a3<-melt(a2, id.vars=c("DataPlan"), variable.name="surv_id", value.name="time")
# Rearranges the data, stacks variables in columns.
a3$surv <- surv[as.numeric(a3$surv_id)]
a3[, c("upper", "lower", "std.err", "strata")] <- NA

ggsurvplot_df(a3, surv.geom = geom_line,
              linetype = "DataPlan", color="DataPlan", legend.title = NULL, xlim=c(0,250), break.time.by = 50)

detach(Telco1)

