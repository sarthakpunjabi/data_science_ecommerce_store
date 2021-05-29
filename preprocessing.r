options(max.print=9999999)
dataset = read.csv("Ecommerce.csv")
View(dataset)
library("tidyverse
        ")
df <- subset(dataset, select = -c(X))
view(df)
install.packages("mice")
library(mice)

summary(df)



df$CustomerID[which(is.na(df$CustomerID))] = na.omit()
df = df[!is.na(df$CustomerID), ]
summary(df)

sort_method = df[order(-df$UnitPrice),]
View(sort_method)

ndf = subset(df,df$Description != "Manual" & df$Description != "POSTAGE" & df$Description != "DOTCOM POSTAGE" & df$Description != "AMAZON FEE" & df$Description != "Discount"& df$Description != "CRUK Commission")

summary(ndf)
table(ndf$CustomerID)
 c = ndf$Description=="CARRIAGE"
ndf[ndf$Description=="CARRIAGE",]

ndf$TotalPrice = ndf$Quantity * ndf$UnitPrice
ndf
View(ndf)
ndf[ndf$CustomerID]
length(unique(ndf$CustomerID))

?Boxplot
help("barplot")

barplot(ndf$Country,xlab="Country name",col = "blue")

length(unique(ndf$Country))

ggplot(ndf, aes(x=UnitPrice, y=Quantity,fill=UnitPrice))+
  geom_bar(stat='identity')

with(ndf, cor(Quantity, UnitPrice))

getwd()
write.csv(ndf,file = "processedData.csv")


