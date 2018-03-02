library(arules)#association rules
library(arulesViz)#association rules visualization
library(datasets)
data("Groceries")
inspect(Groceries)
LIST(Groceries[1:5])

#let's apply apriori algorithm
frequentitems=eclat(Groceries,parameter = list(supp=0.07,maxlen=15))
inspect(frequentitems)
itemFrequencyPlot(Groceries,topN=20,type="absolute")
rules=apriori(Groceries,parameter = list(supp=0.001,conf=0.5))
inspect(rules[1:5])
rulesl=sort(rules,by="lift",decreasing = T)
inspect(rulesl[1:5])


rules=apriori(data=Groceries,parameter=list(supp=0.001,conf=0.08),appearance = list(default="lhs",rhs="whole milk"),control = list(verbose=F))

inspect(rules[1:5])




rules1=apriori(data=Groceries,parameter=list(supp=0.001,conf=0.08),appearance = list(default="rhs",lhs="whole milk"),control = list(verbose=F))

inspect(rules1[1:20])
plot(rules[1:10],method="graph",engine="interactive",shading = NA)


