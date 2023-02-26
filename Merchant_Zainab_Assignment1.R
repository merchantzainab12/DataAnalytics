#############################PART 1######################################

# 2.11
#A)
Toyota.df <- read.csv("ToyotaCorolla.csv", header = TRUE)
View(Toyota.df)
cor(Toyota.df[c(3,4,5,7,9,13,16,17,18)])
heatmap(cor(Toyota.df[c(3,4,5,7,9,13,16,17,18)]))

#B)
xtotal <- model.matrix(~ 0 + Fuel_Type + Color , data = Toyota.df)
xtotal

#C)
train.rows <- sample(row.names(Toyota.df), dim(Toyota.df)[1]*0.5) 
valid.rows <- sample(setdiff(rownames(Toyota.df), train.rows), dim(Toyota.df)[1]*0.3)
test.rows <- setdiff(rownames(Toyota.df), union(train.rows, valid.rows))

#################################### PART -2 ##############################33
car_df <- read.csv("C:/Users/merch/OneDrive/Desktop/UTD/BAR/car_insurance.csv")
view(car_df)

install.packages("ggplot")
library(ggplot2)

# Q1) A
ggplot(car_df, aes(Total_Claim_Amount,Income, colour= Response)) +
  geom_point(alpha = 0.6, size=3)  + geom_text(aes(label = paste(" ", Response)), size = 4, angle=25)

??geom_point

#### Q2 ######

# Problem 3.3
laptop_sales <- read.csv("C:/Users/merch/OneDrive/Desktop/UTD/BAR/LaptopSalesJanuary2008.csv")
View(laptop_sales)
install.packages("dplyr")
library(dplyr)

### Finding average price by store postcode ########

#A)
df_average_retail_price = laptop_sales %>% group_by(Store.Postcode)  %>% summarise(avg_sales = mean(Retail.Price))

barplot(df_average_retail_price$avg_sales, names.arg = as.factor(df_average_retail_price$Store.Postcode),las=2,space =1,col="blue")
library(ggplot2)
#B)
#side by side boxplot
laptop_sales %>% ggplot(aes(x=reorder(Store.Postcode,-Retail.Price, na.rm = TRUE), y=Retail.Price)) +
  geom_boxplot() + 
  labs(y="Average Retail Prices", x="Store Postcode", 
       subtitle="Average retail price by Store ")

############## PART-3 ##############################################
movies_df <- read.csv("C:/Users/merch/OneDrive/Desktop/UTD/BAR/Movies2016.csv")
?hist

#####Histograms ###########

view(movies_df)
# 1) Total Gross Sales
hist(movies_df$Total.Gross.Sales....millions.,main = "Total Gross Sales", labels=TRUE, xlab="Total Gross Sales in millions of dollars", col = "light blue", plot= TRUE)

# 2) Opening Gross Sales
hist(movies_df$Opening.Gross.Sales....millions, main = "Opening Gross Sales", labels=TRUE, xlab="Opening Gross Sales in millions", col="light blue" )

# 3) Number of theaters
hist(movies_df$Number.of.Theaters,main = "Number of theaters", labels=TRUE, xlab= "Number of theaters", col="light blue")

# 4) Number of weeks
hist(movies_df$Weeks.in.Release, main = "Number of weeks", labels=TRUE, xlab= "Number of weeks",col="light blue")

############# Scatter Plots ##############

# 1) Total Sales ~ Opening Gross Sales
plot(movies_df$Total.Gross.Sales....millions. ~ movies_df$Opening.Gross.Sales....millions., ylab = "Total sales", xlab = "Opening gross sales", col="purple", pch=2
,main="Total Sales ~ Opening Gross Sales")
abline()
#2) Total Sales ~ Number of theaters
plot(movies_df$Total.Gross.Sales....millions. ~ movies_df$Number.of.Theaters, ylab = "Total sales", xlab = "Number of theaters", col="red", pch=9, main= "Total Sales ~ Number of theaters")

#3) Total Sales ~ Weeks in Release
plot(movies_df$Total.Gross.Sales....millions. ~ movies_df$Weeks.in.Release, ylab = "Total sales", xlab = "Weeks in Release", col="navy blue",pch=12, main="Total sales Vs Week in Release")


