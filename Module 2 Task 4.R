####UBIQUM MODEL 2, TASK 3:
#In this exercise we are using a dataset...


####SET DIRECTORY, LOAD PACKAGES AND IMPORT DATA# FINISHED ####
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

#importing electronidex items dataset

trans_orig <- read.transactions("ElectronidexTransactions2017.csv", format = 'basket', sep = ',', rm.duplicates = TRUE)

?read.transactions

#visualizing our data
summary(trans_orig)



inspect (trans_orig[0:5]) # You can view the transactions. Is there a way to see a certain # of transactions?
length (trans_orig) # Number of transactions.
size (trans_orig) # Number of items per transaction
LIST(trans_orig) # Lists the transactions by conversion (LIST must be capitalized)
itemLabels(trans_orig)# To see the item labels


par(mar = rep(2, 4))
itemFrequencyPlot(trans_orig, topN=20, type = 'absolute')
image(trans_orig)


image(sample(trans_orig, 300))

##first model
RulesName<- apriori (trans_orig, parameter = list(supp = 0.001, conf = 0.8, minlen = 2))
RulesName
inspect(RulesName)
summary(RulesName)

sort(RulesName, by="support", decreasing=TRUE)

ItemRules <- subset(RulesName, items %in% "HP Monitor")
inspect(RulesName[1:10])

is.redundant(RulesName)
inspect(RulesName[97:107])
inspect(RulesName[c(105, 135)])

plot(RulesName, jitter = 0)        
plot(RulesName[0:10], method="graph")
plot(RulesName[0:10], method = "grouped")
plot(RulesName[0:10], method="paracoord", control=list(reorder=TRUE))

