setwd("~/Downloads")

library(dplyr)


orders <- read.csv('casestudy.csv')
str(orders)
View(orders)

orders <- orders[,-c(1)]

year15 <- orders %>% filter(year==2015)
#colnames(year15) <- c('email15','revenue15','year')
year16<- orders %>% filter(year==2016)
#colnames(year16) <- c('email16','revenue16','Year')
year17<- orders %>% filter(year==2017)
#colnames(year17) <- c('rmail17','revenue17','year')

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
data.frame(year= c('2014-15','2015-16','2016-17'),
           revenue_diff = c(revenue15,revenue16_d,revenue17_d))

# Existing Customer Revenue Current Year
data.frame(year=c('2015','2016','2017'),
           revenue_existing=c(revenue15,revenue16,revenue17) )

# Existing Customer Revenue Prior Year
data.frame(year=c('2015','2016','2017'),
           revenue_existing=c(revenue15,revenue16,revenue17) )

# Total Customers Current Year
data.frame(year= c('2015','2016','2017')
           ,Total_Customer=c(nrow(year15),nrow(year16),nrow(year17)))
# Total Customers Previous Year
data.frame(year= c('2015','2016','2017')
           ,Total_Customer=c(nrow(year15),nrow(year16),nrow(year17)))
# New Customers
data.frame(year= c('2015','2016','2017')
           ,New_Custoemrs=c(nrow(year15),nrow(b),nrow(c)))
# Lost Customers
lost16 <- nrow(year15) - nrow(b)
lost16

lost17 <- ifelse((nrow(b) - nrow(c))>0,nrow(b) - nrow(c),0)
lost17

data.frame(year= c('2015','2016','2017'),
           Lost_Customer = c(0,lost16,lost17))
