
#In the initial  part of this data science project, we will first get familiarized with all available datasets. 
#This will help in planning data cleaning and formatting activities. 
#Following are the datasets available for this project, lets explore and clean/format them one by one:

#monthly-sales-2010.csv - This dataset contains monthly sales for all Pawdacity stores for year 2010.

pawdacity_sales_2010 <- read.csv(file.choose())
View(pawdacity_sales_2010)

#Sales data available here is divided into each month, it will be useful if we can summarize this data to get total_sales per city for year 2010.

city_sales <- select(pawdacity_sales_2010, -c('NAME','ADDRESS','STATE','ZIP'))
View(city_sales)
summary(city_sales)

total_sales <- mutate(city_sales, total_sales=rowSums(city_sales[2:13]) )
total_sales <- select(total_sales, -c(2:13))
View(total_sales)

#demographic-data.csv - This file contains demographic data(Households with individuals under 18, Land Area, Population Density, and Total Families) for each city and county in Wyoming. 
#Since this data is already provided on city level, we can use this as it is.

demography <- read.csv(file.choose())
View(demography)



#NAICS data for sales of all competitor stores where total sales is equal to 12 months of sales.
#From this dataset we can find total annual sales for each city. This will be used in our competitor sales analysis.
# We need to summarize the data City name and sum of sales volume.
naics_data <- read.csv(file.choose())
View(naics_data)
com_sales_data <- select(naics_data, -c('BUSINESS.NAME','CASS_LastLine'))
tot_com_sales_data <- com_sales_data%>% group_by(PHYSICAL.CITY.NAME) %>% summarise_all(funs(sum))
View(tot_com_sales_data)



#partially-parsed-wy-web-scrape.csv - This file contains population data crawled/collected from webpages. 
#It is a partially parsed and need to be cleaned up
parsed_data <- read.csv(file.choose())
View(parsed_data)

city_parsed_data <- select(parsed_data, -c('X2014.Estimate','X2000.Census'))
View(city_parsed_data)
df<-separate(city_parsed_data, City.County, into=c('city','County'))
View(df)

#Create a Function get rid of HTML strings

strip_html <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}


#apply the function to clean the column
df$X2010.Census <- strip_html(df$X2010.Census)

cleaned_parsed_data <- select(df, -c(County))
cleaned_parsed_data[7,2] = 1285
cleaned_parsed_data[90,2] = 366

View(cleaned_parsed_data) 

# now transfer all celaned data in to csv files as a reference

write.csv(cleaned_parsed_data, file = "D:/Predictive Analytics/Udacity/Predictive Analytics for Business/Project 3/cleaned/cleaned_parsed_data.csv")
write.csv(tot_com_sales_data, file = "D:/Predictive Analytics/Udacity/Predictive Analytics for Business/Project 3/cleaned/tot_com_sales_data.csv")
write.csv(demography, file = "D:/Predictive Analytics/Udacity/Predictive Analytics for Business/Project 3/cleaned/demography.csv")
write.csv(total_sales, file = "D:/Predictive Analytics/Udacity/Predictive Analytics for Business/Project 3/cleaned/total_sales.csv")

# For the further analysis it will be easy if we merge 
#demographics, 
#census and 
#total sales (year 2010) data into one file, mapping it w.r.t. city.
demo <- read.csv(file.choose())
census <- read.csv(file.choose())
sales <- read.csv(file.choose())

View(demo)
View(census)
View(sales)

data_set <- inner_join(demo,sales, by = 'city')
View(data_set)
training_data_set <- left_join(data_set, census, by= 'city')
View(training_data_set)

write.csv(training_data_set, file = "D:/Predictive Analytics/Udacity/Predictive Analytics for Business/Project 3/cleaned/training_data_set.csv")


summary(training_data_set)
type_of(training_data_set$X2010.Census)
as.numeric(as.character(training_data_set$X2010.Census))

# Summary of the totals as follows,

census_pop <- sum(training_data_set$X2010.Census)
tot_pawdacity_sales <- sum(training_data_set$total_sales)
household_u_18 <- sum(training_data_set$Households.with.Under.18)
land_area <- sum(training_data_set$land_area)
pop_den <- sum(training_data_set$Population.Density)
tot_families <- sum(training_data_set$Total.Families)

census_pop
tot_pawdacity_sales
household_u_18
land_area
pop_den
tot_families

# undersatnd the outliers by box plots

Boxplot( ~ total_sales, data=training_data_set, id=list(method="y"))
Boxplot( ~ Total.Families, data=training_data_set, id=list(method="y"))
Boxplot( ~ Population.Density, data=training_data_set, id=list(method="y"))
Boxplot( ~ land_area, data=training_data_set, id=list(method="y"))
Boxplot( ~ Households.with.Under.18, data=training_data_set, 
         id=list(method="y"))

scatterplot(total_sales~land_area, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, data=training_data_set)
scatterplot(total_sales~Population.Density, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, data=training_data_set)
scatterplot(total_sales~Total.Families, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, data=training_data_set)
scatterplot(total_sales~Households.with.Under.18, regLine=TRUE, 
            smooth=FALSE, boxplots=FALSE, data=training_data_set)
