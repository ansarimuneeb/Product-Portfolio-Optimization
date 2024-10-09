######################################
## Perceptual and Preference Mapping #
######################################

## Load Packages and Set Seed
library(data.table)
set.seed(1)

## Read in perceptions data
per <- read.csv(file.choose()) #Load PCA data
str(per)
summary(per)

## Run Princple Components Analysis on Perceptions

pca <- prcomp(per[,2:length(per)], retx=TRUE, scale=TRUE)
print(pca)

## Perceptual Map Data - Attribute Factors and CSV File

attribute <- as.data.table(colnames(per[,2:length(per)])); setnames(attribute, 1, "Attribute")
print(attribute)

# Calculate the singular values
singular_values <- pca$sdev * sqrt(nrow(per) - 1)
print(singular_values)
#Observe where the singular values drop significantly, that is the number of PCs that you should have.

total_variance = sum(singular_values^2)
pve = (singular_values^2) / total_variance
print(pve)
#Proportion of variance explained
#1st PC exlpains 60% and 2nd PC explains 24% of the data, which is +80%, Hence 2 PCs are best.

factor1 <- pca$rotation[,1]*pca$sdev[1]; factor2 <- pca$rotation[,2]*pca$sdev[2]; path <- rep(1, nrow(attribute))
print(factor1) #Loading Factors of PCA1, can tell which feature is most important
print(factor2)

pca_factors <- subset(cbind(attribute, factor1, factor2, path), select = c(Attribute, factor1, factor2, path))
print(pca_factors)

pca_origin <- cbind(attribute, factor1 = rep(0,nrow(attribute)), factor2 = rep(0,nrow(attribute)), path = rep(0,nrow(attribute)))
print(pca_origin)

pca_attributes <- rbind(pca_factors, pca_origin)
print(pca_attributes)

write.csv(pca_attributes, "perceptions_attributes_assignment2.csv", row.names = FALSE) ## Name file perceptions_attributes_assignment2.csv

## Perceptual Map Data - Brand Factors and CSV File

score1 <- (pca$x[,1]/apply(abs(pca$x),2,max)[1])
print(score1)

score2 <- (pca$x[,2]/apply(abs(pca$x),2,max)[2])
print(score2)

pca_scores <- subset(cbind(per, score1, score2), select = c(Model, score1, score2))
print(pca_scores)

#Observe what models have the highest scores in both PC models or score1 and score2. High scores in that specific PC indicates that that specific model 
#is highly correlated with the attributes heavily loading on this component. + or - sign will indicate if this relation is positive or negative.

write.csv(pca_scores, "perceptions_scores_assignment2.csv", row.names = FALSE) ## Name file perceptions_scores_assignment2.csv
