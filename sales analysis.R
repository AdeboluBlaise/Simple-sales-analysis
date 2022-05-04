library(ggplot2)
library(dplyr)
setwd("C:/Users/HP/Desktop/R")
#SALES1<-read.csv("Sales dataset.CSV")
SALES1<-read.csv("https://drive.google.com/file/d/1cgW_G7Lb7puwX0NUwfEuuXQ504lhDXxu/view?usp=sharing")
#This is a sales data recorded in Blaise bicycles company

#To list the variables in mydata
names(SALES1)

#To removed unused columns
SALES <- subset(SALES1, select = -c(Transaction_date, Gender, Age,Marital.status,Employment.status,Payment.method,Referal))

#To list the updated dataset
names(SALES)

#To view the first10 dataset in SALES
head(SALES, n=10)
attach(SALES)
str(SALES)

#To get the total size of the dataset
print(nrow(SALES))

# To check null values
summary(is.na(SALES))  

#Summary statistics
summary(SALES)

#To get the total amount for sales
sum(Amount_spent,na.rm = TRUE)

#To get the total profit made
sum(Profit,na.rm = TRUE)

#To getthe total sales per segment
salessegment <- aggregate(Amount_spent, by=list(Segment), FUN=sum, na.rm=TRUE)
salessegment[order(-salessegment$x), ]

#To getthe total sales per state
salesstate <- aggregate(Amount_spent, by=list(State), FUN=sum, na.rm=TRUE)
salesstate[order(-salesstate$x), ]


#To get the total sales per brand
salesbrand <- aggregate(Amount_spent, by=list(brand), FUN=sum, na.rm=TRUE)
salesbrand[order(-salesbrand$x), ]



#To get the total Profit per state
Profitstate <- aggregate(Profit, by=list(State), FUN=sum, na.rm=TRUE)

#To arrange the ouput in a decending order
Profitstate[order(-Profitstate$x), ]
#To view th e top10 profitting states
head(Profitstate,10)


#To get the total profit per segment
Profitsegment <- aggregate(Profit, by=list(Segment), FUN=sum, na.rm=TRUE)
#To arrange the ouput in a decending order
Profitsegment[order(-Profitsegment$x), ]

#Visualizing the data using a barchart
ggplot(data = Profitsegment, mapping = aes(x=Group.1, y=x,fill = Group.1))+
  geom_bar(stat='identity',width = 0.7,show.legend = FALSE) +  
  labs(title = "Total profit per segment", y = "segment", x = "Profit(£)")+
  coord_flip()+
  geom_text(aes(label = x),hjust = 1.5,color = "white",size = 3) +
  theme_classic()


#To get the total Profit per brand
Profitbrand <- aggregate(Profit, by=list(brand), FUN=sum, na.rm=TRUE)
#To arrange the ouput in a decending order
Profitbrand<-Profitbrand[order(-Profitbrand$x),]

#Visualizing the data using a barchart
ggplot(data = Profitbrand, mapping = aes(x=Group.1, y=x,fill = Group.1))+
 geom_bar(stat='identity',width = 0.7,show.legend = FALSE) +  
labs(title = "Total profit per brand", y = "Brands", x = "Profit(£)")+
coord_flip()+
geom_text(aes(label = x),hjust = 1.5,color = "white",size = 3) +
theme_classic()