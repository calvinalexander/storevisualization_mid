order <- read.csv('orders.csv', stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
product <- read.csv('products.csv', stringsAsFactors = F)

product_order <- merge(order, product, by = 'product_id')
View(order)

install.packages('Metrics')
library(Metrics)

#1.a
boxplot(product_price ~ department, data = product, main='Product Price of All Product Department'
        ,xlab='Department', ylab='Product Price', col = rainbow(21), outline = T, horizontal = F)

# 1.b
product_distr <- table(product$department)
# rank(desc(product_distr))
product_distr <- sort(product_distr, decreasing=T)
n <- length(product_distr)
others <- c(sum(product_distr[6:n]))
product_distr <- product_distr[1:5]
#product_distr <- rbind(product_distr[1:5], others) ERROR
persen <- round(product_distr / sum(product_distr) *100,2)
labelx <- paste(names(product_distr), ' (', persen, '%',')', sep ='')
pie(product_distr, main='Top 5 Department (Based on Product Count)', labels = labelx, col= rainbow
  (5))

#1.c
aisle_count <- product[product$department == 'frozen',]
aisle_count <- table(aisle_count$aisle)
aisle_count <- sort(aisle_count)
aisle_count <- aisle_count[1:3]
barplot(aisle_count, main='Lowest 3 Aisle in Frozen Department (Based on Product Count',col= rainbow(3))


#2.a
dataset <- product_order
dataset <- dataset[dataset$department =='alcohol', ]
dataset <- dataset[dataset$aisle != 'specialty wines champagnes', ]
dataset <- dataset[!duplicated(dataset), ]

#2.b
library(arules)
itemset <- split(dataset$product_name, dataset$order_id)
itemset <- as(itemset, 'transactions')

#2.c
frequent_itemset <- apriori(itemset, parameter = list(support = 0.04, target = 'frequent itemsets'))
inspect(frequent_itemset)

#2.d
rules <- ruleInduction(frequent_itemset, confidence = 0.5)
inspect(rules)



