library(arules)
library(arulesViz)
library(ggplot2)

setwd('Your Directory')
bakery <- read.csv('5000-out2.csv',header = FALSE,stringsAsFactors = FALSE)
food <- read.csv('foodname.csv',header = FALSE,stringsAsFactors = FALSE)
bakery$V1 <- NULL
bakery <- as.matrix(bakery)
#Change column names to food names
dimnames(bakery) <-  list(NULL,food$V1) 

#Read as transactions
trans <-  as(bakery, "transactions")
trans
inspect(trans[1:5])
#Plot top 20 most purchased items
itemFrequencyPlot(trans,topN=20,type="absolute")
bar <- barplot(sort(colSums(bakery != 0), decreasing=FALSE)[1:20])

#Apriori Algorithm
rules <- apriori(trans, parameter = list(minlen=2,supp = 0.001, conf = 0.8)) 
rules
inspect(rules)
options(digits=3)
inspect(rules[1:10])
summary(rules)

#Sort rules by confidence
rules.sorted<-sort(rules, by="confidence", decreasing=TRUE)
rules.sorted
inspect(rules.sorted[1:10])

#Remove redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules.pruned
summary(rules.pruned)
inspect(rules.pruned[1:10])
rules.pruned.sorted<-sort(rules.pruned, by="confidence", decreasing=FALSE)
inspect(rules.pruned.sorted[1:5])

#Set lhs and rhs to observe the rules in Chocolate Eclair
#rhs= Chocolate Eclair
rules<-apriori(trans, parameter=list(supp=0.001,conf = 0.02), 
               appearance = list(default="lhs",rhs="Chocolate Eclair"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules)

#Remove redundant rules in Chocolate Eclair 
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules.pruned
summary(rules.pruned)
inspect(rules.pruned[1:10])
rules.pruned.sorted<-sort(rules.pruned, by="confidence", decreasing=TRUE)
inspect(rules.pruned.sorted)

#lhs= Chocolate Eclair
rules<-apriori(trans, parameter=list(supp=0.001,conf = 0.02,minlen=2), 
               appearance = list(default="rhs",lhs="Chocolate Eclair"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

#Visualization Rules
plot(rules,method="graph",interactive=TRUE,shading=NA)

#Show Top 10 lowest sales items
as.integer(colSums(bakery != 0))
qty<-as.integer(colSums(bakery != 0))
items <- data.frame(food$V1,qty)
colnames(items) <- c("ItemName","Qty")
items <- items[with(items, order(qty)),]
items <- head(items,10) #TOP 10
#items <- head(items,) #TOP 5

act.graph <-ggplot(data=items, aes(x=reorder(ItemName,-Qty), y=Qty, fill=ItemName)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Qty), hjust=1.2, size=4) +
  ggtitle("Top 10 Lowest Sales") +
  labs(x="ItemName",y="Qty")
act.graph + coord_flip()
