####UBIQUM MODEL 2, TASK 3####
#In this exercise we are making a market basket analysis using a dataset that 
#contains 9835 transactions. This dataset contains transactions done by cus-
#tomers of an e-commerce store done for a whole month.


####SET DIRECTORY, LOAD PACKAGES AND IMPORT DATA####
setwd("C:/Users/mgold/Desktop/Ubiqum/Module 2/Task 4 Market Basket")

##install.packages("arules")
##install.packages("arulesViz")
##install.packages("caTools")
##install.packages("prabclus")

library(ggplot2)
library(caret)
library(dplyr)
library(ggExtra)
library(arules)
library(arulesViz)

#importing electronidex product and product type dataset
trans_orig <- read.transactions("ElectronidexTransactions2017.csv", format 
  = 'basket', sep = ',', rm.duplicates = TRUE)

#for product types we take out products that are duplicate in the same purchase
trans_type <- read.transactions("ProductTypesElectronidex.csv", format 
  = 'basket', sep = ',', rm.duplicates = FALSE)

####ANALYZING THE PRODUCTS####
summary(trans_orig)
inspect (trans_orig[0:20])
length (trans_orig) # Number of transactions.
itemLabels(trans_orig)# To see the item labels

#plotting our top 20 sold products: relative and absolute values
itemFrequencyPlot(trans_orig,
                  topN=20,
                  main='Item Frequency Plot',
                  type="absolute",
                  ylab="Item Frequency")
itemFrequencyPlot(trans_orig,
                  topN=20,
                  main='Relative Item Frequency Plot',
                  type="relative",
                  ylab="Item Frequency (Relative)")

#visualizing duplicate products
duplicated_trans <- trans_orig[duplicated(trans_orig)]
summary(duplicated_trans) 
#we are interested in how many duplicates per number of purchased items and in
#the mean of the value

####ANALYZING PRODUCT TYPES####
summary(trans_type)
inspect (trans_type[0:20])
length (trans_type) # Number of transactions.
itemLabels(trans_type)# To see the item labels

#plotting our top 20 sold products: relative and absolute values
itemFrequencyPlot(trans_type,
                  topN=20,
                  main='Item Frequency Plot',
                  type="absolute",
                  ylab="Item Frequency")
itemFrequencyPlot(trans_type,
                  topN=20,
                  main='Relative Item Frequency Plot',
                  type="relative",
                  ylab="Item Frequency (Relative)")

#visualizing duplicate products
duplicated_trans <- trans_type[duplicated(trans_type)]
summary(duplicated_trans) 
#we are interested in how many duplicates per number of purchased items and in 
#the mean of tha value


####APRIORI ALGORITHM FOR PROCUTS####

#### Important: Creating our Rules: YOU CAN USE ANY YOU WANT:

## GENERAL ##
RulesName<- apriori (trans_orig, parameter = list(supp = 0.0015, conf = 0.9))

## SPECIFIC PRODUCT ON THE RHS ##
RulesName <- apriori(trans_orig, parameter = list(supp=0.01, conf=0.2), 
  appearance = list(default="lhs",rhs="iMac"))

## SPECIFIC PRODUCT ON THE LHS ##
Rules_after <- apriori(trans_orig, parameter = list(supp=0.02, conf=0.25), 
  appearance = list(lhs="Acer Aspire",default="rhs"))


# Taking out redundant rules
subset.rules <- which(colSums(is.subset(RulesName, RulesName)) > 1)

#Making sure we don't create an empty list in case there's no redundant rules
if (length(subset.rules) != 0){
  RulesName <- RulesName[-subset.rules] # remove subset rules.
  }

# Visualizing our rules
summary(RulesName)

# Sorting rules by lift, confidence and support
inspect(head(sort(RulesName, by="lift"),10))
inspect(head(sort(RulesName, by="confidence"),10))
inspect(head(sort(RulesName, by="support"),10))

# Plotting the rules in order to better decide which ones to use
if (length(RulesName) > 50){
  plotly_arules(head(sort(RulesName, by="lift"), 50), jitter = 1)
}else{
  plotly_arules(head(sort(RulesName, by="lift"), length(RulesName)), jitter = 1)
}


####DIFFERENT VISUALIZATIONS OF RESULTS FOR PRODUCTS####
#plot relations sorted by lift: graph
plot(head(sort(RulesName,by="lift"),10), method="graph", engine = "htmlwidget")

#plot relations sorted by lift: paracoord
plot(head(RulesName, n=20, by="lift"), method="paracoord")

#plot LHS VS RHS
if (length(RulesName) > 10){
  plot(RulesName[0:10], method = "grouped")
}else{
  plot(RulesName[0:length(RulesName)], method = "grouped")
}

#plot of rules: RHS VS LHS with lift
if (length(RulesName) > 10){
  plot(RulesName[0:10], method="matrix", control = list(reorder = "similarity"))
  }else{
    plot(RulesName[0:length(RulesName)], method="matrix", control = 
      list(reorder = "similarity"))
    }

#plot with conditions
subRules<-RulesName[quality(RulesName)$confidence>0.002]
plot(subRules)


####APRIORI ALGORITHM FOR PRODUCT TYPES####

#### Important: Creating our Rules: YOU CAN USE ANY YOU WANT:

## GENERAL ##
RulesTypeName<- apriori (trans_type, parameter = list(supp = 0.0015, conf = 0.9))

## SPECIFIC PRODUCT ON THE RHS ##
RulesTypeName <- apriori(trans_type, parameter = list(supp=0.01, conf=0.2),
  appearance = list(default="lhs",rhs="Laptop"))

## SPECIFIC PRODUCT ON THE LHS ##
RulesTypeName <- apriori(trans_type, parameter = list(supp=0.02, conf=0.25),
  appearance = list(lhs="Desktop",default="rhs"))


# Taking out redundant rules
subset.rulesType <- which(colSums(is.subset(RulesTypeName, RulesTypeName)) > 1)

 #Making sure we don't create an empty list in case there's no redundant rules
if (length(subset.rulesType) != 0){
  RulesTypeName <- RulesTypeName[-subset.rulesType] # remove subset rules.
}

# Visualizing our rules
summary(RulesTypeName)

# Sorting rules by lift, confidence and support
inspect(head(sort(RulesTypeName, by="lift"),10))
inspect(head(sort(RulesTypeName, by="confidence"),10))
inspect(head(sort(RulesTypeName, by="support"),10))

# Plotting the rules in order to better decide which ones to use
if (length(RulesTypeName) > 50){
  plotly_arules(head(sort(RulesTypeName, by="lift"), 50), jitter = 1)
}else{
  plotly_arules(head(sort(RulesTypeName, by="lift"), length(RulesTypeName)), jitter = 1)
}

####DIFFERENT VISUALIZATIONS OF RESULTS FOR PRODUCT TYPES####
#plot relations sorted by lift: graph
plot(head(sort(RulesTypeName,by="lift"),10), method="graph", engine = "htmlwidget")

#plot relations sorted by lift: paracoord
plot(head(RulesTypeName, n=20, by="lift"), method="paracoord")

#plot LHS VS RHS
if (length(RulesTypeName) > 10){
  plot(RulesTypeName[0:10], method = "grouped")
}else{
  plot(RulesTypeName[0:length(RulesTypeName)], method = "grouped")
}

#plot of rules: RHS VS LHS with lift
if (length(RulesTypeName) > 10){
  plot(RulesTypeName[0:10], method="matrix", control = list(reorder = "similarity"))
}else{
  plot(RulesTypeName[0:length(RulesTypeName)], method="matrix", control = 
    list(reorder = "similarity"))
}

#plot with conditions
subRulesType<-RulesName[quality(RulesTypeName)$confidence>0.002]
plot(subRulesType)