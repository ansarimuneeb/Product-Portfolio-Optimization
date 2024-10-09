#####################
# Conjoint Analysis #
#####################


## Load Packages and Set Seed
library(conjoint)
set.seed(1)

## Set up attributes and levels as a list
attrib.level <- list(Environmental_friendliness = c("0% CO2 reduction", "30% CO2 reduction", "50% CO2 reduction"),
                     Delivery_time = c("14 Days", "21 Days", "30 Days"), 
                     Service_Level = c("5-year warranty", "5-year warranty & free maintenance", "5-year warranty, free maintenance and installation, & upgradeability"),
                     Price = c("1000 GBP", "1200 GBP", "1500 GBP"), 
                     Quality_of_material = c("Market average", "A bit higher than market average"),
                     Marketing_Proficiency = c("Not very proficient and poor communication", "Very proficient and have good communication skills"))


#Load Product Profiles, Deleted the first column, added _ between spaces of variable names
design <- read.csv(file.choose())

str(design)
design[] <- lapply(design, factor)
str(design)

design <- within(design, {
  Environmental_friendliness <- factor(Environmental_friendliness, levels = c("1", "2", "3"), labels = c("0% CO2 reduction", "30% CO2 reduction", "50% CO2 reduction"))
  Delivery_time <- factor(Delivery_time, levels = c("1", "2", "3"), labels = c("14 Days", "21 Days", "30 Days"))
  Service_Level <- factor(Service_Level, levels = c("1", "2", "3"), labels = c("5-year warranty", "5-year warranty & free maintenance", "5-year warranty, free maintenance and installation, & upgradeability"))
  Price <- factor(Price, levels = c("1", "2", "3"), labels = c("1000 GBP", "1200 GBP", "1500 GBP"))
  Quality_of_material <- factor(Quality_of_material, levels = c("1", "2"), labels = c("Market average", "A bit higher than market average"))
  Marketing_Proficiency <- factor(Marketing_Proficiency, levels = c("1", "2"), labels = c("Not very proficient and poor communication", "Very proficient and have good communication skills"))
})

# Check the results
str(design)


# Manually adding attributes
attr(design, "out.attrs") <- list(
  dim = c(3, 3, 3, 3, 2, 2),  # Number of levels in each factor, ensure these numbers match your data
  dimnames = list(
    Environmental_friendliness = c("Environmental_friendliness=0% CO2 reduction", "Environmental_friendliness=30% CO2 reduction", "Environmental_friendliness=50% CO2 reduction"),
    Delivery_time = c("Delivery_time=14 Days", "Delivery_time=21 Days", "Delivery_time=30 Days"),
    Service_Level = c("Service_Level=5-year warranty", "Service_Level=5-year warranty & free maintenance", "Service_Level=5-year warranty, free maintenance and installation, & upgradeability"),
    Price = c("Price=1000 GBP", "Price=1200 GBP", "Price=1500 GBP"),
    Quality_of_material = c("Quality_of_material=Market average", "Quality_of_material=A bit higher than market average"),
    Marketing_Proficiency = c("Marketing_Proficiency=Not very proficient and poor communication", "Marketing_Proficiency=Very proficient and have good communication skills")
  )
)

# Check the structure to see if the attributes are added
str(design)

## Check for correlation in design
print(cor(caEncodedDesign(design)))

## Run the conjoint analysis study

## Read in the survey preference results, delete the first column, save as csv.
pref <- read.csv(file.choose()) ## Choose the file named Conjoint_Preference_Cleaned.csv
str(pref)

## Set up attributes and levels as a vector and Estimate the part-worths for each respondent

attrib.vector <- data.frame(unlist(attrib.level,use.names=FALSE))


temp <- caPartUtilities(pref, design, attrib.vector)
str(temp)
summary(temp)

colnames(attrib.vector) <- c("levels")
part.worths <- NULL

for (i in 1:ncol(pref)) {
  temp <- caPartUtilities(pref[,i], design, attrib.vector)
  
  ## Base Case: Environmental.friendliness 0%, Delivery.time 14 days, Service.Level 5 year warranty, price 1000GBP, Quality.of.material average, Marketing.Proficiency poor
  base_Environmental_friendliness <- temp[,"0% CO2 reduction"]
  base_Delivery_time <- temp[,"14 Days"]
  base_Service_Level <- temp[,"5-year warranty"]
  base_Price <- temp[,"1000 GBP"]
  base_Quality_of_material <- temp[,"Market average"]
  base_Marketing_Proficiency <- temp[,"Not very proficient and poor communication"]
  
  ## Adjust Intercept
  temp[,"intercept"] <- temp[,"intercept"] - base_Environmental_friendliness - base_Delivery_time - base_Service_Level -
    base_Price - base_Quality_of_material - base_Marketing_Proficiency
  
  ## Adjust Coefficients for each attribute
  ## Environmental_friendliness
  L1 <- length(attrib.level$Environmental_friendliness)
  for (j in 1:L1) { temp[,j] <- temp[,j] - base_Environmental_friendliness }
  ## Delivery_time
  L2 <- length(attrib.level$Delivery_time) + L1
  for (k in (L1+1):L2) { temp[,k] <- temp[,k] - base_Delivery_time }
  ## Service_Level
  L3 <- length(attrib.level$Service_Level) + L2
  for (l in (L2+1):L3) { temp[,l] <- temp[,l] - base_Service_Level }
  ## Price
  L4 <- length(attrib.level$Price) + L3
  for (m in (L3+1):L4) { temp[,m] <- temp[,m] - base_Price }
  ## Quality_of_material
  L5 <- length(attrib.level$Quality_of_material) + L4
  for (n in (L4+1):L5) { temp[,n] <- temp[,n] - base_Quality_of_material }
  ## Marketing_Proficiency
  L6 <- length(attrib.level$Marketing_Proficiency) + L5
  for (n in (L5+1):L6) { temp[,n] <- temp[,n] - base_Marketing_Proficiency }
  
  part.worths <- rbind(part.worths, temp)
}


rownames(part.worths) <- colnames(pref)
summary(part.worths)
print(part.worths)


## Export part-worths from analysis
write.csv(part.worths, "Conjoint_Partworths_Assignment.csv", row.names = FALSE) ## Name the file Conjoint_Partworths_Assignment.csv

