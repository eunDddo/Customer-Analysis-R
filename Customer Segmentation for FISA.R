#Sets working directory
setwd("C:/Users/deu06/Desktop/Customer Anaylsis")

#pulls data from a csv file
customerdata<-read.csv("C:/Users/deu06/Desktop/Customer Anaylsis/Assignment 1_Data_2022.csv", header=TRUE)

#Get an overview of the variables 
names(customerdata) 

attach(customerdata) #"activates" the bankdata set for easier access to the variables

#----------------------------------------------------------
# Step 1: We define research goal: s to segment your current customers according to their credit card usage patterns

#----------------------------------------------------------------------------
# Step 2: we choose variables that align with the research goal
#----------------------------------------------------------------------------
# Create dataframe for cluster analysis that contains the variables to be used

cluster.raw.df <- data.frame(ONEOFF_PURCHASES, INSTALLMENTS_PURCHASES, CASH_ADVANCE, CREDIT_EXAUST, PAYMENT_DIFFERENCE)

# we now work with the new dataframe so we want to deactivate the old one
detach(customerdata)

# and attach the new one
attach(cluster.raw.df)

# Check variable measurement
summary(cluster.raw.df)
str(cluster.raw.df)               # All are numeric

# Check for multicollinearity
cor(cluster.raw.df, method=c("pearson"), use="complete") # shows that correlations are below .8 so multicollinearity is no problem
ONEOFF_PURCHASES_z <- scale(ONEOFF_PURCHASES)   # Standardization is not necessary here but it doesn't harm either.
INSTALLMENTS_PURCHASES_z <-scale(INSTALLMENTS_PURCHASES)
CASH_ADVANCE_z <-scale(CASH_ADVANCE) 
CREDIT_EXAUST_z <-scale(CREDIT_EXAUST) 
PAYMENT_DIFFERENCE_z <-scale(PAYMENT_DIFFERENCE)

summary(ONEOFF_PURCHASES_z) # Example what the scale procedure does: mean of 0...
sd(ONEOFF_PURCHASES_z)    # ...and sd of 1

#We will only work with the standardized variables next so we detach the dataframe with the unstandardized variables
detach(cluster.raw.df) 

#Define the dataframe for the clusteranalysis with the standardized variables
cluster.z.df<-data.frame(ONEOFF_PURCHASES_z, INSTALLMENTS_PURCHASES_z, CASH_ADVANCE_z, CREDIT_EXAUST_z, PAYMENT_DIFFERENCE_z)

#-------------------------------------------------------------
# Step 4.1: Hierarchcial Cluster Analysis
#-------------------------------------------------------------
# Run hierachical cluster analysis
# "euclidean" is the standard method to maximize the distance between clusters
# "ward.D2" is our preferred method to determine how clusters are merged to mimize the distances within clusters
hca <- hclust(dist(cluster.z.df, method = "euclidean"), method = "ward.D2") 

# Scree plot to identify the best number of clusters
plot(hca$height [7000:6975], type="line")  # Applying the "elbow-criterion" the scree plot suggests to choose between 5-7 clusters


#------------------------------------------------------------
#Step 4.2: K-means Cluster Analysis
#------------------------------------------------------------
# RUn k-means cluster analysis
kmca <- kmeans(cluster.z.df,4)
kmca #The interesting part for us are the Cluster means. However, for interoretation we want unstandardized means...

# Instead we can use the aggregate function to derive means per cluster

aggregate(cbind(cluster.raw.df$ONEOFF_PURCHASES, cluster.raw.df$INSTALLMENTS_PURCHASES, 
                cluster.raw.df$CASH_ADVANCE, cluster.raw.df$CREDIT_EXAUST,cluster.raw.df$PAYMENT_DIFFERENCE)~kmca$cluster, 
          cluster.raw.df, mean)             # Cbind combines variables/datasets by column. 
# Aggregate offers summary statistics for each
# please note that each time you rerun k-MEANS,
# the clusters are  a bit different and so are the 
# cluster means.

## Now with the means we could further refine the output by writing our own function. However, our focus is on 
## understanding not coding. So for the ease of it let's switch to Excel now and let's quickly pull together a table
## that we now use for interpretation purposes.

