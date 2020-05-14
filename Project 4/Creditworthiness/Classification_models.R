install.packages("ROCR") # to develop a ROC curve
install.packages("rpart") # to develope a decision tree model
install.packages("rpart.plot")
install.packages("randomForest") # to debvelop a random forest model
install.packages("gbm") # to develop boosted models
install.packages("caret")
install.packages("gmodels")
install.packages("varImp")
install.packages("vip")
install.packages("mboost")


set.seed(1)

## Set a traning set and test set

## training set 70% test set 30%

test_set <- 500*0.3
train_set <- 500*0.7

train_set
test_set

### Randomly select the 350 observation as training set

train_sample <- sample(500, train_set) # Randomly samples train_set number of samples out of 500
train_sample 

# fresh upload the cleaned data set.
credit <- read_excel(file.choose()) # upload the cleaned file

# Random selection of training and test set

credit_train <- credit[train_sample,] # Training set was selected randomly 
credit_test <- credit[-train_sample,] # Test set set was also selected randomly

View(credit_train)
View(credit_test)
# check the distribution of predicted variable in both sets
prop.table(table(credit_train$`Credit-Application-Result`))
prop.table(table(credit_test$`Credit-Application-Result`))

## Create Factor levels for the variables

### Training Set
credit_train$`Credit-Application-Result` <- as.factor(credit_train$`Credit-Application-Result`)
credit_train$`Value-Savings-Stocks` <- as.factor(credit_train$`Value-Savings-Stocks`)
credit_train$`Account-Balance`<- as.factor(credit_train$`Account-Balance`)
credit_train$`Payment-Status-of-Previous-Credit` <- as.factor(credit_train$`Payment-Status-of-Previous-Credit`)
credit_train$Purpose <- as.factor(credit_train$Purpose)
credit_train$`Length-of-current-employment`<- as.factor(credit_train$`Length-of-current-employment`)
credit_train$`No-of-Credits-at-this-Bank` <- as.factor(credit_train$`No-of-Credits-at-this-Bank`)
credit_train$`Type-of-apartment`<- as.factor(credit_train$`Type-of-apartment`)
credit_train
str(credit_train)

### Test Set
credit_test$`Credit-Application-Result` <- as.factor(credit_test$`Credit-Application-Result`)
credit_test$`Value-Savings-Stocks` <- as.factor(credit_test$`Value-Savings-Stocks`)
credit_test$`Account-Balance`<- as.factor(credit_test$`Account-Balance`)
credit_test$`Payment-Status-of-Previous-Credit` <- as.factor(credit_test$`Payment-Status-of-Previous-Credit`)
credit_test$Purpose <- as.factor(credit_test$Purpose)
credit_test$`Length-of-current-employment`<- as.factor(credit_test$`Length-of-current-employment`)
credit_test$`No-of-Credits-at-this-Bank` <- as.factor(credit_test$`No-of-Credits-at-this-Bank`)
credit_test$`Type-of-apartment`<- as.factor(credit_test$`Type-of-apartment`)
credit_test




## Change fator levels of a column with special charaters
levels(credit_train$`Value-Savings-Stocks`)[levels(credit_train$`Value-Savings-Stocks`)== '£100-£1000' ] <- " between 100 and 1000"
levels(credit_train$`Value-Savings-Stocks`)[levels(credit_train$`Value-Savings-Stocks`)== '< £100' ] <- "less than 100 pound"

levels(credit_test$`Value-Savings-Stocks`)[levels(credit_test$`Value-Savings-Stocks`)== '£100-£1000' ] <- " between 100 and 1000"
levels(credit_test$`Value-Savings-Stocks`)[levels(credit_test$`Value-Savings-Stocks`)== '< £100' ] <- "less than 100 pound"


str(credit_train$`Value-Savings-Stocks`)
str(credit_train$`Length-of-current-employment`)

str(credit_test$`Value-Savings-Stocks`)
str(credit_test$`Length-of-current-employment`)

#Decision Tree Model by Rpart algorithm

## Training the model  by rpart 

##credit_model <- rpart(`Credit-Application-Result`~ `Account-Balance`+`Duration-of-Credit-Month`+
##                        `Payment-Status-of-Previous-Credit`+`Purpose`+`Credit-Amount`+
##                        `Value-Savings-Stocks`+`Length-of-current-employment`+
##                        `Age-years`+`No-of-Credits-at-this-Bank`+
##                        `Telephone`,data= credit_train, control = (cp=0))


credit_model <- rpart(`Credit-Application-Result`~.,data= credit_train, control = (cp=0))
credit_model
summary(credit_model)


## Plot the decision tree and Show the split variable name in the interior nodes. use type=5
rpart.plot(credit_model, type=5)

# Let us now check the variable imporatance from model summary


## Evaluation of model performance

str(credit_test)

credit_pred <- rpart.predict(credit_model, credit_test, type= "class")

summary(credit_pred)


table <- CrossTable(credit_test$`Credit-Application-Result`, credit_pred, prop.r= FALSE, proc.c= FALSE, prop.chisq = FALSE, prop.t= FALSE)
table




## Logistic Model 

## Training of the model
glm_t <- glm(formula = `Credit-Application-Result`~.,family= binomial, data= credit_train)
glm_t
summary(glm_t)

# Stepwise Backward regression

glm_trn<- stepwise(glm_t, direction = 'backward', criterion = 'AIC')

glm_trn <- stepwise(glm_t, direction = 'forward', criterion = 'AIC')

## Evaluation and prediction

glm_p <- predict.glm(glm_trn, newdata = credit_test, type= "response")

conf_m <- table(credit_test$`Credit-Application-Result`, as.numeric(glm_p >0.5))
conf_m
glm_p
summary(glm_p)
View(glm_p)


# ROC Curve development for logistic model

pr<- prediction(glm_p, credit_test$`Credit-Application-Result`)
perf <- performance(pr, measure = "acc")
plot(perf)

## random Forest Model
### randomForest( list on predictors , response variable, data, ntree, mty )
set.seed(1)
forest_train <- randomForest(credit_train[2:13], credit_train$`Credit-Application-Result`, 
                             data = credit_train, ntree = 1500,mtry = 5)

forest_train
# Plot the variable imporatance plot
varImpPlot(forest_train, sort=TRUE, main = "Variable Importance Plot")                             

# plot the method for random forest
plot(forest_train, main = "Percentage Error for Different number of Trees" , type="l", )

forest_pred <- predict(forest_train, newdata = credit_test, type = "response")
forest_pred
plot(forest_pred, main= " Predicted Variables")

##Confusion Matrix for the Forest Model.
CrossTable(credit_test$`Credit-Application-Result`, forest_pred, prop.r= FALSE, proc.c= FALSE, prop.chisq = FALSE, prop.t= FALSE)


# Boosted Model by gbm package.


boosted_train <- gbm(credit_train$`Credit-Application-Result`~., data = credit_train, distribution = "gaussian",
                      shrinkage =0.01,  n.trees = 4000)

#summary(boosted_train, cBars= 10, method= relative.influence, las = 2)

print(boosted_train)  

inf_bm <- summary(boosted_train)
inf_bm <- as.data.frame(inf_bm)
inf_bm$var <- as.factor(inf_bm$var)

# varaible Importance plot
inf_bm %>%
  mutate(Var= fct_reorder(inf_bm$var, inf_bm$rel.inf)) %>% 
  ggplot(aes(x= rel.inf, y= Var)) + geom_point() +ggtitle("Variable Importance Plot")+xlab("Relative Importance %") + ylab("Variables")

### Shorter technique to use the bove ggplot
vip::vip(boosted_train)

#predited Model
boosted_pred <- predict.gbm(object= boosted_train, newdata= credit_test,n.trees= 4000)
boosted_pred
p.bst<- apply(boosted_pred, 1, which.max)

labels = colnames(boosted_pred)[lapply(boosted_pred, 1, which.max)]
result <- data.frame(credit_test$`Credit-Application-Result`, labels)

print(result)


#####  random forest model for the prediction of new data set.

cus_score <- read_excel((file.choose()))

cus_score <- select(cus_score, -c('Foreign-Worker','No-of-dependents','Occupation','Telephone','Concurrent-Credits', 'Guarantors','Duration-in-Current-address'))

View(cus_score)

str(cus_score)

levels(cus_score$`Value-Savings-Stocks`)[levels(cus_score$`Value-Savings-Stocks`)== '£100-£1000' ] <- " between 100 and 1000"
levels(cus_score$`Value-Savings-Stocks`)[levels(cus_score$`Value-Savings-Stocks`)== '< £100' ] <- "less than 100 pound"



cus_score$`Credit-Application-Result` <- as.factor(cus_score$`Credit-Application-Result`)
cus_score$`Value-Savings-Stocks` <- as.factor(cus_score$`Value-Savings-Stocks`)
cus_score$`Account-Balance`<- as.factor(cus_score$`Account-Balance`)
cus_score$`Payment-Status-of-Previous-Credit` <- as.factor(cus_score$`Payment-Status-of-Previous-Credit`)
cus_score$Purpose <- as.factor(cus_score$Purpose)
cus_score$`Length-of-current-employment`<- as.factor(cus_score$`Length-of-current-employment`)
cus_score$`No-of-Credits-at-this-Bank` <- as.factor(cus_score$`No-of-Credits-at-this-Bank`)
cus_score$`Type-of-apartment`<- as.factor(cus_score$`Type-of-apartment`)
cus_score
str(cus_score)
str(credit_train)
plot(forest_train)

View(cus_score)



cust_score_pr <- predict(forest_train, newdata = cus_score, type = "response")
cust_score_pr
plot(cust_score_pr, main= " Predicted Variables")



# ROC Curve development
