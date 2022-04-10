setwd("~/Downloads")
loans <- read.csv('loans_full_schema.csv',header = T)
View(loans)
str(loans)
sum(is.na(loans))

# Empty Character to NA
loans[loans==''] <- NA

# Change Date format 
loans$issue_month<-  paste(loans$issue_month, "-01",sep="")
loans$issue_month<-  as.Date(loans$issue_month,"%b-%Y-%d")

# Libraries
library(ggplot2)
library(dplyr)
library(stringr)
library(ggrepel)
library(GGally)
library(RColorBrewer)
library(maps)
library(randomForest)


options(scipen = 100)

# Data of interest rate and state
loan_interest <- loans[,c(3,43)]

# Match state abbreviations with state names
loan_interest$state <- state.name[match(loan_interest$state, state.abb)]

# Sum of NA values
sum(is.na(loan_interest))

# Washington DC was missing 
loan_interest$state[is.na(loan_interest$state)] <- 'Washington'

# Filter the data and create an average
loan_interest <-loan_interest %>%
  group_by(state) %>% 
  summarise(avg=mean(interest_rate))  %>% 
  arrange(state,by_group=T) 

loan_interest <- loan_interest[-c(11),]



# Make color buckets
loan_interest$colorBuckets <- as.numeric(cut(loan_interest$avg, 
                                       c(0,11,12,13,14,Inf)))

# Create a legend text
leg.txt<- c("<11%", "11-12%", "12-13%", "13-14%", ">14%")

# Crate a color palette
palette <- brewer.pal(5, "Pastel1")

# Map color to color buckets
loan_interest$colorCode <- palette[loan_interest$colorBuckets]

# Map the data
map(database = 'state',
    region = loan_interest$state,
    col=loan_interest$colorCode,fill=T,lty=1,lwd=1)

# Add a legend
legend('bottomright',legend=leg.txt,
       horiz = F,fill = palette)

# Add a title
title(main= "Average Interest Rate per State",
      font.main= 2, cex.main =2 , col.main= 'black')
dev.off()

# Loan data of Managers

loan_managers <- subset(loans, grepl('manage', emp_title))
View(loan_managers)
loan_managers <- loan_managers[c(39)]
str(loan_managers)
loan_managers <- loan_managers %>% 
  group_by(loan_purpose) %>% 
  summarise(count = n())

loan_managers $loan_purpose <- c('Car','Credit Card',
                                 'Debt Consolidation',
                                 'Home Improvement',
                                 'House',
                                 'Major Purchase',
                                 'Medical',
                                 'Moving',
                                 'Other',
                                 'Renewable Energy',
                                 'Small Business',
                                 'Vacation')

plot2 <- loan_managers %>% 
  arrange(count) %>% 
  ggplot(aes(x= loan_purpose,y=count)) +
  geom_col(aes(fill=as.factor(loan_purpose))) +
  geom_label(aes(label= count),nudge_y = 15) +
  labs(title = "Loan Purpose of Managers",
       x= "Loan Purpose",
       y= "No. of Managers",
       fill= 'Purpose of Loan') +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'none') + coord_flip()
plot2

# Parallael Coordination Plot
loan_parallel <- loans[c(1,4,45,5,42,43,44,41)]

# Select HR titles
loan_parallel <- subset(loan_parallel,grepl('hr',emp_title))

# PLot the co-ordination plot
plot3 <- ggparcoord(loan_parallel,columns = 3:8,
                    groupColumn = 2,
           showPoints = T,boxplot = T) +
  labs(title = 'Difference in factors for HR (Hiring Managers)',
       x='Factor', y='Simplified Value')+
  scale_color_discrete(name='Type of Home Ownership')+
  theme(plot.title = element_text(hjust = 0.5)) 
plot3

# Alluvial to see which Grade and month-issued loans were late.
library(alluvial)
loan_alluvial <- loans[c(45,47,40,48)]
loan_alluvial$issue_month <- (factor(loan_alluvial$issue_month,
                                    levels = c('2018-01-01',
                                               '2018-02-01',
                                               '2018-03-01'),
                                    labels = c('Jan','Feb','Mar')
                                    ))

View(loan_alluvial)
loan_alluvial <- table(loan_alluvial)
loan_alluvial <- as.data.frame(loan_alluvial)

# Select people who paid thier total loan
loan_alluvial <- loan_alluvial %>% filter(loan_status=='Fully Paid' &Freq>=1)
loan_alluvial <- aggregate(Freq~grade+issue_month+application_type+loan_status,data=loan_alluvial,sum)

# Add a color palette
newpalette <- brewer.pal(7,'Dark2')

# Change column names
colnames(loan_alluvial) <- c("Grade","Month Issued","Applicant Type","Loan Status","Freq")

# Plot alluvial
alluvial(loan_alluvial[1:3],
                   freq = loan_alluvial$Freq,
                   col = newpalette)

# Potential Defaulters
loans_n <- loans[,c(40,39,41,45,48,5,26,51,34)]

# Convert INT as Numeric
loans_n$loan_amount <- as.numeric(loans_n$loan_amount)
loans_n$num_cc_carrying_balance <- as.numeric(loans_n$num_cc_carrying_balance)
str(loans_n)

# Filter those who are late for payment
# Potential Defaulters
loans_n <- loans[,c(5,41,34,20,48,38,45,51)]

# Convert INT as Numeric
loans_n$loan_amount <- as.numeric(loans_n$loan_amount)
loans_n$public_record_bankrupt <- as.numeric(loans_n$public_record_bankrupt)
loans_n$num_historical_failed_to_pay <- as.numeric(loans_n$num_historical_failed_to_pay)
str(loans_n)

# Filter those who are late for payment
loans_n <-loans_n %>% filter(loan_status=='Late (31-120 days)' |
                               loan_status=="Late (16-30 days)"|
                               loan_status=="In Grace Period" ,
                             num_historical_failed_to_pay>=1 &
                               public_record_bankrupt>=1)
loans_n <-na.omit(loans_n)

# Plot the facet plot
plot4 <- loans_n %>% ggplot(aes(x=loan_amount, y= annual_income,
                                col=grade,size=balance))+
  geom_jitter()+
  facet_wrap(num_cc_carrying_balance~public_record_bankrupt) +
  labs(title = "Probable Loan Defaulters for Each Grade",
       x= 'Loan Amount',
       y='Annual Income') +
  theme(plot.title = element_text(hjust = 0.5))
plot4  

# People in California who took a loan

# Simple histogram showing amount of loans most taken in California
loans_ca <- loans %>% filter(state=='CA')
plot5 <- loans_ca %>% ggplot(aes(x=loan_amount),col=grade) + 
  geom_histogram(binwidth = 5000,alpha=0.6,col='blue')+
  theme_bw() + labs(title = "Loans Recieved in California",
                    x= 'Loan Amount',y='Count')+
  theme(plot.title = element_text(hjust = 0.5)) 
plot5

#-----------------------------------------------------------------
loans_linear <- loans[,c(18,5,6,39,40,41,42,43,44,45)]



set.seed(100)

# Create sample 8000 indices
index <- sample(1:nrow(loans_linear), 0.8*nrow(loans_linear)) 

# Training Data
train <- loans_linear[index, ]

# Testing Data
test  <- loans_linear[-index, ]   

# Crate Linear Model
model <- glm(interest_rate ~ grade
            , data=train)
summary(model)

# Predict Test Data
predicted <- predict(model,test)
predicted


combined <- data.frame(cbind(actuals=test$interest_rate, predicteds=predicted))

# Find accuracy of the data
accuracy <- cor(combined)
accuracy
head(combined)

# Plot the data and accuracy
plot(combined$predicteds,                                
     combined$actuals,
     xlab='Predicted',
     ylab='Actual',
     main='Actual vs. Predicted')
abline(a = 0,   b=1,                                     
       col = "red",
       lwd = 2)
text(x=12,y=30,paste("Accuracy:",round(accuracy[2],digits = 6)*100,'%'))
dev.off()


set.seed(1)

# Create a Random Forest Model
model2 <- randomForest(
  formula = interest_rate ~ verified_income+loan_amount+ installment+
    total_credit_utilized + term + application_type,
  data = train
)


model

# Predict the model
predicted2 <- predict(model2,test)
combined2 <- data.frame(cbind(actual=test$interest_rate, predicted = predicted2))

# Find the accuracy
accuracy2 <- cor(combined2)

# Plot the data and accuracy
plot(x= combined2$predicted,
     y= combined2$actual,
     xlab='Precited',
     ylab='Actuals',
     main='Actual vs Predicted (Random Forests)')
abline(a = 0,                                        # Add straight line
       b = 1,
       col = "red",
       lwd = 2)
text(x=12,y=30,paste("Accuracy:",round(accuracy2[2],digits = 6)*100,'%'))

     