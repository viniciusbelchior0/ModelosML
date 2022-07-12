library(arules)
library(arulesViz)

groceries <- read.transactions("C:\\Users\\NOTEBOOK CASA\\Desktop\\Mainstream\\Estatística\\Interdependência\\market_basket\\basket.csv",
                               format = "basket",
                               sep = ",",
                               rm.duplicates = F)

inspect(groceries)
summary(groceries)

itemFrequencyPlot(groceries, topN = 10)

# Regras de Associacao ####
regras <- apriori(groceries, 
                  parameter = list(support = 0.01,
                                   confidence = 0.10,
                                   minlen = 2,
                                   maxlen = 10))
#confidence e lift sao mais interessantes do que suporte

plot(regras, method = "scatter", engine = "htmlwidget", max = 300)#scatterplot
plot(regras, method = "graph", engine = "htmlwidget", max = 300)#grafo
plot(regras, method = "grouped", measure = "lift", shading = "confidence")#agrupado

# Regras como DF ####
df <- as(regras, "data.frame")
view(df)
