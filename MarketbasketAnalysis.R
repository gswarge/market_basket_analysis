#Market Basket Analysis
    
    
    
library(plyr)
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(scales)
library(knitr)
library(plotly)
library(arules)
library(arulesViz)
library(visNetwork)
library(igraph)
library(kableExtra)
library(tidyverse)
library("RColorBrewer")

#Reading all the Datasets

products <- fread("/Dataset/products.csv")
departments <- fread("/Dataset/departments.csv")

opp <- fread("/Dataset/order_products__prior.csv")

#converting to factor variables
orders$order_hour_of_day <- as.numeric(orders$order_hour_of_day)
orders$order_dow <- as.factor(orders$order_dow)
orders$eval_set <- as.factor(orders$eval_set)

products$product_name <- as.factor(products$product_name)

departments$department <- as.factor(departments$department)

#aisles$aisle <-  as.factor(aisles$aisle)


#Merging the tables:

#products_aisles <- merge(products,aisles,by="aisle_id")
#products_aisles_dept <- merge(products_aisles,departments,by="department_id")

#Department Level

data <- opp %>%
  inner_join(products, by = "product_id")%>% 
  inner_join(departments, by = "department_id")%>% 
  filter(reordered==1)%>%
  group_by(order_id)%>%
  select(order_id,department)

nperishable_dept<- opp %>%
  inner_join(products, by = "product_id")%>% 
  inner_join(departments, by = "department_id")%>% 
  filter(reordered==1)%>%
  filter(!department %in% c("dairy eggs","snacks","beverages","produce","frozen")) %>% 
  group_by(order_id)%>%
  select(order_id,department)

head(nperishable_dept,25)
dim(nperishable_dept)
data1 <- split(nperishable_dept$department,nperishable_dept$order_id)
head(data1,25)

#converting to class transactions
transaction_prior<-as(data1,"transactions")
dim(transaction_prior)
inspect(transaction_prior[1:2])

frequentDept <- eclat (transaction_prior, parameter = list(supp = 0.07, maxlen = 15)) # calculates support for frequent items
inspect(frequentDept)

#frequent aisles in the transactions
itemFrequencyPlot(transaction_prior,topN=20,type="absolute")

#Running the Apriori algorithm
Dept_basket_rules <- apriori(transaction_prior, parameter = list(support = 0.03, confidence = 0.80, minlen=3,maxlen=10,target = "rules"))
Dept_basket_rules

#Removing Redundent Rules (rules that are subset of larger rules)
# get subset rules in vector
subsetRules <- which(colSums(is.subset(Dept_basket_rules, Dept_basket_rules)) > 1) 
length(subsetRules) 
# remove subset rules 
Dept_basket_rules <- Dept_basket_rules[-subsetRules]
Dept_basket_rules
# 'high-confidence' rules.
Dept_basket_rules_conf <- sort (Dept_basket_rules, by="confidence", decreasing=TRUE) 

# show the support, lift and confidence for all rules
inspect(head(Dept_basket_rules_conf)) 

# High-Lift Rules
Dept_basket_rules_lift <- sort (Dept_basket_rules, by="lift", decreasing=TRUE) 

# show the support, lift and confidence for all rules
inspect(head(Dept_basket_rules_lift)) 

plot(Dept_basket_rules,control=list(col=brewer.pal(11,"Spectral")),main="")
plot(Dept_basket_rules, method = "graph")
plotly_arules(Dept_basket_rules)
inspectDT(Dept_basket_rules)


#Aisle Level
data <- opp %>%
  inner_join(products, by = "product_id")%>% 
  inner_join(aisles, by = "aisle_id")%>% 
  group_by(order_id) 

kable(head(data,25))
data1 <- split(data$aisle,data$order_id)
kable(head(data1,25))

#converting to class transactions
transaction_prior<-as(data1,"transactions")
dim(transaction_prior)
inspect(transaction_prior[1:2])

frequentAisles <- eclat (transaction_prior, parameter = list(supp = 0.07, maxlen = 15)) # calculates support for frequent items
inspect(frequentAisles)

#frequent aisles in the transactions
itemFrequencyPlot(transaction_prior,topN=20,type="absolute")

#Running the Apriori algorithm
aisle_basket_rules <- apriori(transaction_prior, parameter = list(support = 0.03, confidence = 0.80, minlen=3,maxlen=10,target = "rules"))
aisle_basket_rules

#Removing Redundent Rules (rules that are subset of larger rules)
# get subset rules in vector
subsetRules <- which(colSums(is.subset(aisle_basket_rules, aisle_basket_rules)) > 1) 
length(subsetRules) 
# remove subset rules 
aisle_basket_rules <- aisle_basket_rules[-subsetRules]
aisle_basket_rules
# 'high-confidence' rules.
aisle_basket_rules_conf <- sort (aisle_basket_rules, by="confidence", decreasing=TRUE) 

# show the support, lift and confidence for all rules
inspect(head(aisle_basket_rules_conf)) 

# High-Lift Rules
aisle_basket_rules_lift <- sort (aisle_basket_rules, by="lift", decreasing=TRUE) 

# show the support, lift and confidence for all rules
inspect(head(aisle_basket_rules_lift)) 

plot(aisle_basket_rules,control=list(col=brewer.pal(11,"Spectral")),main="")
plot(aisle_basket_rules, method = "graph")
plotly_arules(aisle_basket_rules)
inspectDT(aisle_basket_rules)
