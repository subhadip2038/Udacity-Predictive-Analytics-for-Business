 # upload the file customer

#p1-customers.xlsx - This dataset includes the following information on about 2,300 customers.

customer <- read_excel(file.choose())
View(customer)

mail_list<- read_excel(file.choose())
View(mail_list)

summary(customer)

scatterplotMatrix(~Avg_Num_Products_Purchased+Avg_Sale_Amount+Customer_ID+Store_Number+X._Years_as_Customer+ZIP,
                  regLine=FALSE, smooth=FALSE, diagonal=list(method="oned"), data=customer)

ggplot(customer,aes(Avg_Num_Products_Purchased,Avg_Sale_Amount))+geom_point()

ggplot(customer, aes(Customer_Segment,Avg_Sale_Amount))+geom_point()

linear_model <- lm(formula = Avg_Sale_Amount ~ Avg_Num_Products_Purchased + Customer_Segment,data = customer)
scatterplot(Avg_Sale_Amount~Avg_Num_Products_Purchased, regLine=FALSE, smooth=FALSE, boxplots='xy', data=customer)

mail_list_p <- mutate(mail_list, predicted_sales= case_when(
  mail_list_p$Customer_Segment=='Loyalty Club and Credit Card' ~ 303.46 + 66.98*mail_list_p$Avg_Num_Products_Purchased + 281.84,
  mail_list_p$Customer_Segment=='Loyalty Club Only' ~303.46 + 66.98 *mail_list_p$Avg_Num_Products_Purchased-149.36,
  mail_list_p$Customer_Segment=='Store Mailing List' ~303.46 + 66.98*mail_list_p$Avg_Num_Products_Purchased-245.42,
  mail_list_p$Customer_Segment== 'Credit Card Only' ~ 303.46 + 66.98*mail_list_p$Avg_Num_Products_Purchased,
))


View(mail_list_p)


mail_list_profit <- mutate(mail_list_p, predicted_rev = mail_list_p$predicted_sales*mail_list_p$Score_Yes)
mail_list_profit <- mutate(mail_list_profit, predicted_profit = mail_list_profit$predicted_rev*0.5 -6.5)
View(mail_list_profit)
total_profit <- sum(mail_list_profit$predicted_profit)
total_profit

write.csv(mail_list_profit, file = "D:/Predictive Analytics/Udacity/Predictive Analytics for Business/Project 2/mail_list_profit.csv")                       
