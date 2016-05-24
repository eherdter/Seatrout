#calculate linear trend between rho (from Low_frequency Zone correlation) and great circle distance 
setwd("~/Desktop/Github Repo/Seatrout/Data/Excel Files")

data <- read.csv("Rho_vs_Distance.csv")
lm_model <- lm(data$Rho ~ data$Great.Circle.Distance)

#model relationship without two negative values
data_subset <- subset(data, Rho > 0)
lm_model_subset <- lm(data_subset$Rho ~ data_subset$Great.Circle.Distance)
