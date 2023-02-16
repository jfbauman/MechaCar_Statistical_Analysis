# Linear Regression to Predict MPG
library(dyply) #Use the library() function to load the dplyr package
MechaCar <- read.csv("MechaCar_mpg.csv",stringsAsFactors = F,check.names = F) #Import and read in the MechaCar_mpg.csv file as a dataframe
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = MechaCar) # Perform linear regression using the lm() function
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = MechaCar) #determine the p-value and the r-squared value for the linear regression model
# Summary Statistics on Suspension Coils 
SuspensionCoilTable <- read.csv("Suspension_Coil.csv",stringsAsFactors = F,check.names = F)
total_summary <- SuspensionCoilTable %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))
lot_summary <- SuspensionCoilTable %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI), .groups = 'keep')
# T-Tests on Suspension Coils 
t.test(subset(SuspensionCoilTable,Manufacturing_Lot == "Lot1") $PSI,mu= 1500)
t.test(subset(SuspensionCoilTable,Manufacturing_Lot == "Lot2") $PSI,mu= 1500)
t.test(subset(SuspensionCoilTable,Manufacturing_Lot == "Lot3") $PSI,mu= 1500)

