
Boxplot( ~ total_sales, data=training_data_set, id=list(method="y"))
Boxplot( ~ total_sales, data=training_data_set, id=list(method="y"))
Boxplot( ~ total_sales, data=training_data_set, id=list(method="y"))
library(aplpack, pos=70)
library(gganimate, pos=71)
library(ggforce, pos=72)
summary(training_data_set)
Boxplot( ~ Total.Families, data=training_data_set, id=list(method="y"))
Boxplot( ~ Population.Density, data=training_data_set, id=list(method="y"))
Boxplot( ~ land_area, data=training_data_set, id=list(method="y"))
Boxplot( ~ Households.with.Under.18, data=training_data_set, 
  id=list(method="y"))
scatterplotMatrix(~Households.with.Under.18+land_area+Population.Density+Total.Families+total_sales,
   regLine=FALSE, smooth=FALSE, diagonal=list(method="density"), 
  data=training_data_set)
scatterplotMatrix(~Households.with.Under.18+land_area+Population.Density+Total.Families+total_sales,
   regLine=FALSE, smooth=FALSE, diagonal=list(method="boxplot"), 
  data=training_data_set)
scatterplotMatrix(~Households.with.Under.18+land_area+Population.Density+Total.Families+total_sales,
   regLine=FALSE, smooth=FALSE, diagonal=list(method="oned"), 
  data=training_data_set)
scatterplotMatrix(~Households.with.Under.18+land_area+Population.Density+Total.Families+total_sales,
   regLine=FALSE, smooth=FALSE, diagonal=list(method="oned"), 
  data=training_data_set)
scatterplotMatrix(~Households.with.Under.18+land_area+Population.Density+Total.Families+total_sales,
   regLine=TRUE, smooth=FALSE, diagonal=list(method="oned"), 
  data=training_data_set)
scatterplotMatrix(~Households.with.Under.18+land_area+Population.Density+Total.Families+total_sales,
   regLine=TRUE, smooth=FALSE, diagonal=list(method="oned"), 
  data=training_data_set)

