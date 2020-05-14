
load("D:/Predictive Analytics/Udacity/Predictive Analytics for Business/Project 3/cleaned/training_data_set.csv")
load("D:/Predictive Analytics/Udacity/Predictive Analytics for Business/Project 3/cleaned/training_data_set.csv")
scatterplot(total_sales~Households.with.Under.18, regLine=FALSE, 
  smooth=FALSE, boxplots=FALSE, data=training_data_set)
scatterplot(total_sales~Households.with.Under.18, regLine=TRUE, 
  smooth=list(span=0.5, spread=FALSE), boxplots=FALSE, data=training_data_set)
scatterplot(total_sales~Households.with.Under.18, regLine=TRUE, 
  smooth=FALSE, boxplots=FALSE, data=training_data_set)
scatterplot(total_sales~Households.with.Under.18, regLine=TRUE, 
  smooth=FALSE, boxplots=FALSE, data=training_data_set)
scatterplot(total_sales~land_area, regLine=TRUE, smooth=FALSE, 
  boxplots=FALSE, data=training_data_set)
scatterplot(total_sales~Population.Density, regLine=TRUE, smooth=FALSE, 
  boxplots=FALSE, data=training_data_set)
scatterplot(total_sales~Total.Families, regLine=TRUE, smooth=FALSE, 
  boxplots=FALSE, data=training_data_set)
scatterplot(total_sales~Households.with.Under.18, regLine=TRUE, 
  smooth=FALSE, boxplots=FALSE, data=training_data_set)

