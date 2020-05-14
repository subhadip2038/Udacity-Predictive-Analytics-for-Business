# First let us upload the data from raw file.

credit_data <- read_excel(file.choose())
View(credit_data)

# Check the content of the dataset.
head(credit_data)

# data types of the variables
str(credit_data)
type <- sapply(credit_data, class)
View(type)

#Statistical Summary of the data set
summary(credit_data)

# Data clean up 
## Check for missing value. Count the missing value in each column and store them in a data frame

na_count <- sapply(credit_data, function(x) sum (is.na(x)))
na_count <- data.frame(na_count)
View(na_count)



##Drop the "Duration-in-Current-address".
credit_data_clean <- select(credit_data, -c(11:11))
View(credit_data_clean)


numSummary(credit_data_clean[,c("Age-years", "Credit-Amount", 
                                "Duration-of-Credit-Month", "Foreign-Worker", "Instalment-per-cent", 
                                "Most-valuable-available-asset", "No-of-dependents", "Occupation", 
                                "Telephone", "Type-of-apartment"), drop=FALSE], statistics=c("mean", "sd", 
                                                                                             "IQR", "quantiles"), quantiles=c(0,.25,.5,.75,1))



## Explore the variability

### Factors
ggplot(credit_data_clean, aes(y=`Age-years`))+ geom_boxplot()+ ggtitle("Age -Years")
ggplot(credit_data_clean, aes(y=`Credit-Amount`))+ geom_boxplot()+ ggtitle("Credit Amount")
ggplot(credit_data_clean, aes(y=`Duration-of-Credit-Month`))+ geom_boxplot()+ ggtitle("Duration of Credit Month")
ggplot(credit_data_clean, aes(y= `Foreign-Worker`))+ geom_boxplot()+ ggtitle("Foreign Worker")
ggplot(credit_data_clean, aes(y= `Instalment-per-cent`))+ geom_boxplot()+ ggtitle(" Instalment per cent")
ggplot(credit_data_clean, aes(y=`Most-valuable-available-asset`))+ geom_boxplot()+ ggtitle("Most Valuable Available Asset")
ggplot(credit_data_clean, aes(y=`No-of-dependents`))+ geom_boxplot()+ ggtitle("Number of Dependents")
ggplot(credit_data_clean, aes(y=`Occupation`))+ geom_boxplot()+ ggtitle("Occupation")
ggplot(credit_data_clean, aes(y=`Telephone`))+ geom_boxplot()+ ggtitle("Telephone")
ggplot(credit_data_clean, aes(y=`Type-of-apartment`))+ geom_boxplot()+ ggtitle("Type of Apartment")

ggplot(credit_data_clean, aes(x=`Type-of-apartment`))+ geom_bar()+ ggtitle("Type of Apartment")+xlab('Type of Apartment')
ggplot(credit_data_clean, aes(x=`Foreign-Worker`))+ geom_bar()+ ggtitle("Foreign Worker")+xlab('Foreign Worker')
ggplot(credit_data_clean, aes(x=`Occupation`))+ geom_bar()+ ggtitle("Occupationr")+xlab('Occupation')

### Numerics
ggplot(credit_data_clean, aes(`Account-Balance`))+geom_bar() + ggtitle('Account Balance')+xlab('Account Balance')
ggplot(credit_data_clean, aes(`Payment-Status-of-Previous-Credit`))+geom_bar()+ ggtitle('Payment Status \nPrevious Credit')+xlab('Privious Credit')
ggplot(credit_data_clean, aes(Purpose))+geom_bar() + ggtitle('Purpose')+xlab('Purpose')
ggplot(credit_data_clean, aes(`Value-Savings-Stocks`))+geom_bar()+ ggtitle('Value Saving Stock')+xlab('Value Saving Stock')
ggplot(credit_data_clean, aes(`Length-of-current-employment`))+geom_bar()+ggtitle('Length of Current \nEmployment')+xlab('Length of employment')
ggplot(credit_data_clean, aes(Guarantors))+geom_bar()+ ggtitle('Guarantors')+xlab('Guarantors')
ggplot(credit_data_clean, aes('Concurrent-Credits'))+geom_bar()+ ggtitle('Concurrent Credits')+xlab('Concurrent Credits')
ggplot(credit_data_clean, aes(`No-of-Credits-at-this-Bank`))+geom_bar()+ ggtitle('# of Credits \n This Bank')+xlab('Number of Credits')

## Replace the missing values by median values of Age.years

credit_data_clean$'Age-years' <- replace_na(credit_data_clean$'Age-years',33)
View(credit_data_clean)
summary(credit_data_clean)

## Drop the vaiables

credit_data_training <- select(credit_data_clean, -c('Foreign-Worker','No-of-dependents','Occupation','Concurrent-Credits', 'Guarantors', 'Telephone'))
View(credit_data_training)

# The cleaned Training data set
str(credit_data_training)
write.xlsx(credit_data_training, file= "D:/Predictive Analytics/Udacity/Predictive Analytics for Business/Project 4/Creditworthiness/cleaned-credit-data-training.xlsx")

type_clean_data <- sapply(credit_data_training,class)
View(type_clean_data)
