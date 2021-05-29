library(dplyr)
library(tidyverse)
library(skimr)
library(ggplot2)
library(VIM)
library(plotrix)


main_df = read.csv("main.csv")
View(main_df)
# 'aggr' plots the amount of missing/imputed values in each column
aggr(main_df)
preDataset <- as_tibble(read.csv("processedData.csv",stringsAsFactors = FALSE))
View(preDataset)
skim(preDataset)
#head(filter(select(preDataset,Description,Quantity,UnitPrice,CustomerID,TotalPrice),Quantity < 0),20)
#head(arrange(select(preDataset,Description,Quantity,UnitPrice,CustomerID,TotalPrice),desc(TotalPrice)),40)

Data_with_minus = select(preDataset,X,Description,Quantity,UnitPrice,CustomerID,TotalPrice)
Data_with_minus <- filter(preDataset,Quantity < 0)
Data_plus = filter(select(preDataset,X,Description,Quantity,UnitPrice,CustomerID,TotalPrice),Quantity > 0)
profit_earned = sum(preDataset$TotalPrice)


#head(table(preDataset$Country,preDataset$TotalPrice),1)


cor.test(preDataset$UnitPrice,preDataset$Quantity)
plot(preDataset$Quantity,preDataset$UnitPrice)
max(Data_plus$Quantity)
filter(preDataset,Quantity > 60000)  

preDataset <- filter(preDataset,Quantity<60000)
preDataset <- filter(preDataset,Quantity > -60000)


outliers = arrange(filter(preDataset,UnitPrice>250),desc(TotalPrice))
out_table = table(outliers$UnitPrice,outliers$Quantity)

length(Data_plus$Quantity)
length(Data_with_minus$Quantity)

#negval = table(Data_with_minus$Quantity,Data_plus$Quantity)
#barplot()

new_df <- data.frame(filter(preDataset,TotalPrice == 0))
length(new_df)
table(new_df$TotalPrice,new_df$CustomerID)

rm(emls)
for(x in new_df$X)
{
  
  preDataset <- filter(preDataset,X!=x)
}

mean(main_df$sum)

plot(preDataset$UnitPrice,preDataset$TotalPrice)


filter(preDataset,InvoiceNo == 537805)

preDataset <- arrange(preDataset,CustomerID)
main_df  = preDataset %>% group_by(CustomerID) %>% summarise(mean = mean(TotalPrice) ,sum = sum(TotalPrice),n = n())
country_df = preDataset %>% group_by(Country) %>%  summarise(sum = sum(TotalPrice))

plot(main_df$CustomerID,main_df$sum)
write.csv(main_df,"main.csv")

main_df <- filter(main_df,sum!=0)
main_df <- filter(main_df,sum>0)

main_df <- as.character(main_df$CustomerID)

boxplot(CustomerID ~ sum, data = main_df,
        varwidth = TRUE, log = "y", las = 1)
bb = arrange(filter(main_df,sum<2500),desc(sum))

a = filter(main_df,sum==278778.0)
plot(bb$CustomerID,bb$sum)
filter(main_df,max(n))

max(main_df$sum)
filter(main_df,sum>278777)
arrange(main_df,sum)


str(main_df)
country_df <- arrange(country_df,desc(sum))

pie(head(country_df$sum,5),labels = head(country_df$Country,5),main = "Country wise purchase",col = rainbow(6))
pie3D(head(country_df$sum,5),labels = head(country_df$Country,5),main = "Country wise purchase",explode = 0.0)
  

boxplot(bb$sum,horizontal = TRUE)



