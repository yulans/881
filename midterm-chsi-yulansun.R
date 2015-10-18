install.packages("jsonlite")
library(jsonlite)
x <- fromJSON("d5190a7b-361e-4f50-9ab4-bf526cc67124")
x
#----------------------------------

library(dplyr)
library(useful)
library(broom)
library(ggplot2)

risk <- read.csv ("RISKFACTORSANDACCESSTOCARE.csv", header = TRUE, stringsAsFactors = F)

# Pick up county name, state, obesity, high bood pressure, smoker, diabetes from risk file.
risk <- select(risk, CHSI_County_Name, CHSI_State_Abbr, No_Exercise, Few_Fruit_Veg, Obesity, 
               High_Blood_Pres, Smoker, Diabetes)

# Remove county with no report.
risk[risk==-1111.1] <- NA
risk <- na.omit(risk)
head(risk)
dim(risk)

# We only need numbers to do kmeans cluster, so remove 2 columns.
risk2 <- risk[,-(1:2)]

# Find the right number of clusters.
risk.best <- FitKMeans(risk2, max.clusters = 20)
PlotHartigan(risk.best)

# According to the plot, 5 clusters seems a good choice.
risk.kmeans <-kmeans(x = risk2, centers = 5)
plot(risk.kmeans, data=risk2)

# Find Worcester belongs to which cluster.
which(risk$CHSI_County_Name == "Worcester" & risk$CHSI_State_Abbr == "MA") #561
risk[561,]
augment(risk.kmeans, risk2)[561,]$.cluster

# Since the varables are all unhealthy measures, the lower components means better health.
# According to the cluster plot, Worcester is about a little higher than the   
# other communities regards to residents' health measures.
#-------------------------------------------------------------------

# But the community leaders of Worcester are not very sure about this results,
# they said they want to know residents's health status to confirm this.

# So we ick up average life expectancy, self-rated health status from summary file.
sum <- read.csv ("SUMMARYMEASURESOFHEALTH.csv", header = TRUE, stringsAsFactors = F)
sum <- select(sum, CHSI_County_Name, CHSI_State_Abbr, ALE, Health_Status)

# Remove counties with no report.
sum[sum==-1111.1] <- NA
sum <- na.omit(sum)
head(sum)
dim(sum)

sum2 <- sum[,-(1:2)]

# Find the right number of clusters.
sum.best <- FitKMeans(sum2, max.clusters = 10)
PlotHartigan(sum.best)

# According to the plot, 6 clusters seems a good choice.
sum.kmeans <-kmeans(x = sum2, centers = 6)
plot(sum.kmeans, data=sum2)

# Find Worcester belongs to which cluster.
which(sum$CHSI_County_Name == "Worcester" & sum$CHSI_State_Abbr == "MA") #998
sum[998,]
augment(sum.kmeans, sum2)[998,]$.cluster

# As for self-rated health status, the higher number means worse health.
# So the lower components means better health.
# Again ccording to the cluster plot, Worcester is a little higher than 
# other communities regards to residents' health status.
#--------------------------------------------------------------------

# The community leaders of Worcester felt the results are not bad.
# But they want to improve residents' self-rated health status, to make
# residents happier about their health. One of the leader suggested they 
# can hire more physicians. Other people want to know if it is an efficient way.

# So we make a data frame with primary care physicians rate and self-rated health status.
risk3 <- read.csv ("RISKFACTORSANDACCESSTOCARE.csv", header = TRUE, stringsAsFactors = F)
sum3 <- read.csv ("SUMMARYMEASURESOFHEALTH.csv", header = TRUE, stringsAsFactors = F)
phys <- select(risk3, CHSI_County_Name, CHSI_State_Abbr, Prim_Care_Phys_Rate)
status <- select(sum3, CHSI_County_Name, CHSI_State_Abbr, Health_Status)
t <- merge(phys,status, by=c("CHSI_County_Name", "CHSI_State_Abbr"))
t[t==-1111.1] <- NA
t <- na.omit(t)
head(t)
dim(t)

# Wocester's data:
which(t$CHSI_County_Name == "Worcester" & t$CHSI_State_Abbr == "MA") #2446
t[2446,]

# Plot data of all counties with primary care physicians rate < 300.
ts <- t[t$Prim_Care_Phys_Rate < 300,]
ggplot(ts, aes(x=Prim_Care_Phys_Rate, y=Health_Status))+geom_point(alpha=1/5) + geom_smooth()

# Since primary care physicians rate of Wocester is 130.9, from the plot it seems 
# hiring more physicians is not an efficient way to improve resident's self-rated 
# health status when self-rated status rate > 100.


