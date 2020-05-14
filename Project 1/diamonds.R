#Step 0 Create the linear multivariate regression model
# Upload the file diamonds.csv in a dataset diamond

diamond<- read_csv(file.choose())
# View the File in Rstudio
View(diamond)

# Summary of datasets in diamonds
summary(diamond)

set.seed(250)
#Create a linear regression model for price with predictor variables carat, clarity_ord and Cut_ord
price_model <- lm (formula= price ~ carat+clarity_ord+cut_ord, data = diamond)

# Summary of the Model
summary(price_model)

#Upload the file new-diamonds.csv in a dataset diamond_price
diamond_price <- read_csv(file.choose())
View(diamond_price) # View the diamond_price data set in Rstudio
# create a new coloumn 'predicted_price' with the model output formula and rename the datset as diamond_price_pred.

diamond_price_pred <- mutate(diamond_price, predicted_price = (-5255.223+8363.417*carat+457.802*clarity_ord+160.379*cut_ord))
# View the new data set 
View(diamond_price_pred)
# write the data in a new csv file diamond_pricepredicted.csv

write.csv(diamond_price_pred, file = "D:/Predictive Analytics/Udacity/Predictive Analytics for Business/diamond_pricepredicted.csv")

#Step 2 - Visualize the Data: Create two scatter plots. 

#Plot 1 - Plot the data for the diamonds in the database, with carat on the x-axis and price on the y-axis.

ggplot(diamond, aes(x=carat, y= price))+ geom_point()

#Plot 2 - Plot the data for the diamonds for which you are predicting prices with carat on the x-axis and predicted price on the y-axis.
ggplot(diamond_price_pred, aes(x=carat, y= predicted_price))+geom_point()

# see the color of points with respect to clarity
ggplot(diamond, aes(x=carat, y= price, color=clarity))+ geom_point()

ggplot(diamond_price_pred, aes(x=carat, y= predicted_price))+geom_point()
#bid based on predicted price.

sum <- sum(diamond_price_pred$predicted_price)
sum
bid <-0.7*sum
bid
