setwd("~/Downloads")

# Required Libraries
library(dplyr)
library(ggplot2)

# Load the data
orders <- read.csv('casestudy.csv')
str(orders)
View(orders)

orders <- orders[,-c(1)]

year15 <- orders %>% filter(year==2015)

year16<- orders %>% filter(year==2016)

year17<- orders %>% filter(year==2017)


# Total net revenue in year 2017
a <- aggregate(net_revenue~year,orders,sum)
a

# New Customer

# New Customer 2015
aggregate(net_revenue~year,year15,sum)[1,2]
#29036749

# New Customer 2016
b <-anti_join(year16,year15,by='customer_email')
b <- na.omit(b) 
aggregate(net_revenue~year,b,sum)[1,2]
# 18245491

# New Customer 2017
c <-anti_join(year17,year16,by='customer_email')
c <- na.omit(c) 
aggregate(net_revenue~year,c,sum)[1,2]
# 28776235

# Existing Customer Growth
# Revenue 2015
revenue15 <- as.numeric('29036749')

# Revenue 16 Existing
d <- left_join(year16,year15,by='customer_email')
d <- na.omit(d)
revenue16<- as.numeric(aggregate(net_revenue.x~year.x,d,sum)[1,2])
revenue16_d <- revenue16-revenue15
revenue16

# Revenue 17 Existing
e <- left_join(year17,year16,by='customer_email')
e <- na.omit(e)
revenue17<- as.numeric(aggregate(net_revenue.x~year.x,e,sum)[1,2])
revenue17_d <- revenue17-revenue16
revenue17

# Revenue lost from attrition
f <-data.frame(year= c('2014-15','2015-16','2016-17'),
           revenue_diff = c(revenue15,revenue16_d,revenue17_d))
f

# Existing Customer Revenue Current Year
g <- data.frame(year=c('2015','2016','2017'),
           revenue_existing=c(revenue15,revenue16,revenue17) )
g

# Existing Customer Revenue Prior Year
h <- data.frame(year=c('2015','2016','2017'),
           revenue_existing=c(revenue15,revenue16,revenue17) )
h

# Total Customers Current Year
i <- data.frame(year= c('2015','2016','2017')
           ,Total_Customer=c(nrow(year15),nrow(year16),nrow(year17)))
i

# Total Customers Previous Year
j <- data.frame(year= c('2015','2016','2017')
           ,Total_Customer=c(nrow(year15),nrow(year16),nrow(year17)))
j

# New Customers
k <- data.frame(year= c('2015','2016','2017')
           ,New_Custoemrs=c(nrow(year15),nrow(b),nrow(c)))
k

# Lost Customers
lost16 <- nrow(year15) - nrow(b)
lost16

lost17 <- ifelse((nrow(b) - nrow(c))>0,nrow(b) - nrow(c),0)
lost17

l <- data.frame(year= c('2015','2016','2017'),
           Lost_Customer = c(0,lost16,lost17))
l

orders %>% group_by(year) %>% summarise(net=sum(net_revenue)) %>% 
  ggplot(aes(x=year,y=net,fill=as.factor(year))) + geom_col() +
  labs(title = 'Overall Yearly Revenue',
       x = 'Year',y='Revenue',fill='Legend') + 
  theme_classic()

orders %>% group_by(year) %>% 
  summarise(n=n_distinct(customer_email)) %>% 
  ggplot(aes(year,n,label=n)) + geom_line() +
  geom_label()+
  labs(x= 'Year',y='No. of distinct emails',
       title="Disticnt Email ID's per year")

