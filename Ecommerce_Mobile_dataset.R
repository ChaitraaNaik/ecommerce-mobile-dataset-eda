# Import libraries
library(FSA)        #Import FSA   library
library(FSAdata)    
library(magrittr)   #Import magrittr library
library(plyr)       #Import plyr  library
library(dplyr)      #Import dplyr  library
library(tidyr)      #Import tidyr  library
library(tidyverse)  #Import tidyverse  library
library(plotrix)    #Import plotrix  library


# Set the current working directory
getwd()
setwd("C:/Users/chait/Desktop/CPS-NEU/Assignments/Module6")

# read the dataset
ph <- read.csv("mobile_dataset.csv",header = TRUE ,sep = ",")
class(ph)   #dataframe

#CLEANING THE DATASET
ph1 <- ph %>% na.omit()  # omit the entries with missing values
view(ph1)

ph2<-ph1 %>%
 group_by(Brand,Model,Memory,Storage) %>%
  summarise_at(vars(Original.Price), list(name = mean))  # find the avg of the original price column
colnames(ph2)[colnames(ph2) == "name"] <- "Original_Price_Mean" 

ph3 <- merge(x = ph, y = ph2, by = c("Brand","Model","Memory","Storage"), all.x = TRUE)

ph4 <- ph3 %>% 
  mutate(Original.Price = coalesce(Original.Price,Original_Price_Mean))#fill the missing values of the org price with the mean of the org price

df = subset(ph4, select = -c(Original_Price_Mean) )   #Final cleaned dataset

summary(df)                                           #Summary of the cleaned dataset 

# Find the count of each brand
ph_count <- table(df$Brand)
ph_count

ph_count <- 100*prop.table(ph_count)

# Sort the data by the increasing order of the percent count
ph_count <- ph_count[order(ph_count)]

# Bar plot of different brands
barplot(ph_count,names.arg = paste(names(ph_count)),horiz = TRUE,cex.names = 0.8,las=2,col = "orange",xlim = c(0,30),
        main = "Market share of Mobile Brands",xlab="Share in %")


#Filtering out the popular brands based on ratings
pop_brands <- filter(df, Rating >= 4.5)


pop_table <- group_by(pop_brands,Brand)
pop_table <- summarise(pop_table, count=n())
per <-  round(100*prop.table(pop_table$count),1)
pop_br_tab <- pop_table %>% add_column(per)

#bar chart of the highest rated brands in market 

ggplot(data=pop_br_tab,aes(x=reorder(Brand,-per),y=per)) + geom_bar(stat="identity")+ 
  labs(title = "Highly Rated Brands in the Market",
     x = "Brand", y = "Percentage of share") 

#filter the data by the popular 3 brands 
pop_apple <-pop_brands %>% filter(Brand == "Apple" | Brand == "realme" | Brand =="SAMSUNG")

pop_min_SP <- pop_apple %>%
  group_by(Brand) %>%
  summarise(min_Selling_Price = min(Selling.Price,na.rm = TRUE))


pop_max_SP <- pop_apple %>%
  group_by(Brand) %>%
  summarise(max_Selling_Price = max(Selling.Price,na.rm = TRUE))


# finding mean and median of popular 3 brands

#Mean
mean_pop <- pop_apple %>%
            group_by(Brand) %>%
            summarise_at(vars(Selling.Price), list(name = mean))
colnames(mean_pop)[colnames(mean_pop) == "name"] <- "Mean" 


#Median
median_pop <- pop_apple %>%
  group_by(Brand) %>%
  summarise_at(vars(Selling.Price), list(name = median))
colnames(median_pop)[colnames(median_pop) == "name"] <- "Median" 

#Summarise the min,max,mean and median values into single dataframe df_value
df_values <- data.frame(pop_min_SP,pop_max_SP,mean_pop,median_pop)

df_values <- merge(pop_min_SP, pop_max_SP,mean_pop,median_pop, by = 'Brand')
df_values <- subset (df_values, select = -c(Brand.1,Brand.2,Brand.3))
view(df_values)


# Are the popular phones expensive? what is the relation between popular brands Vs prices
ggplot(pop_apple,aes(x = Brand , y= Selling.Price,fill = Brand )) + geom_boxplot()

#histogram
pop_brands %>% 
  filter(Brand == "Apple" | Brand == "realme" | Brand =="SAMSUNG") %>%
  ggplot(aes(x = Selling.Price,fill = Brand)) + geom_histogram(na.rm = TRUE)   + facet_wrap(~Brand)
  

#check the most preferred model of each brand

# Apple
pop_apple1 <- pop_brands %>% filter(Brand == "Apple") 

# order in decreasing order of count
pop_apple11 <- pop_apple1 %>%
  group_by(Model) %>%
  summarise(number = length(Model)) 

ggplot(data=pop_apple11,aes(x=reorder(Model,number),y=number)) + geom_bar(stat = "identity") + xlab("Models") +ylab("Number of Models")  + coord_flip()

#color most preferred

# order in decreasing order of count
pop_apple_col <- pop_apple1 %>%
  group_by(Color) %>%
  summarise(number = length(Color))

ggplot(data=pop_apple_col,aes(x=reorder(Color,number),y=number)) + geom_bar(stat = "identity") + xlab("Color") +ylab("Number of Models")+ coord_flip()


# Realme
pop_real <- pop_brands %>% filter(Brand == "realme")  
pop_real_model <- pop_real  %>%  group_by(Model) %>% summarise(number = length(Model))
ggplot(data=pop_real_model,aes(x=reorder(Model,-number),y=number)) + geom_bar(stat = "identity") + xlab("Models") +ylab("Number of Models")

#color most preferred
pop_real_col <- pop_real  %>%  group_by(Color) %>% summarise(number = length(Color))
ggplot(data=pop_real_col,aes(x=reorder(Color,number),y=number)) + geom_bar(stat = "identity") + xlab("color") +ylab("Number of Models") + coord_flip()

#Samsung
pop_sam <- pop_brands %>% filter(Brand == "SAMSUNG")
pop_sam_model <- pop_real  %>%  group_by(Model) %>% summarise(number = length(Model))
ggplot(data=pop_sam_model,aes(x=reorder(Model,number),y=number)) + geom_bar(stat = "identity") + xlab("Models") +ylab("Number of Models") + coord_flip()

# most preferred color
pop_sam_col <- pop_sam  %>%  group_by(Color) %>% summarise(number = length(Color))
ggplot(data=pop_sam_col,aes(x=reorder(Color,number),y=number)) + geom_bar(stat = "identity") + xlab("Color") +ylab("Number of Models") +coord_flip()

#Brand with the most product offerings to the market
No_of_models <- df %>%
  group_by(Brand) %>%
  summarise(sum_of_models = length(unique(Model)))
No_of_models

# Bar plot of the brand with the most product offerings 
ggplot(data=No_of_models,aes(x=reorder(Brand,sum_of_models),y=sum_of_models)) + geom_bar(stat="identity") +
  labs(title = "Product Share Offering of Each Brand",
       x = "Brand", y = "Number Of Models") + coord_flip()

#Now that we have found the brands with most product offerings,find out the min cost of each brand

min_SP <- df %>% filter(Brand== "SAMSUNG" | Brand == "Nokia" | Brand =="OPPO") %>%
  group_by(Brand) %>%
  summarise(min_Selling_Price = min(Selling.Price,na.rm = TRUE))
min_SP

#Find the max cost of each brand
max_SP <- df %>% filter(Brand== "SAMSUNG" | Brand == "Nokia" | Brand =="OPPO") %>%
  group_by(Brand) %>%
  summarise(max_Selling_Price = max(Selling.Price,na.rm = TRUE))
max_SP

#merge the values into a dataframe
df_model <- data.frame(min_SP,max_SP)

df_model <- merge( min_SP,max_SP,by = 'Brand')
view(df_model)


############################################
# calculate the percent change in the price

df1 <- df%>% group_by(Brand,Model,Memory,Storage) %>% 
       mutate(pct_change = ((Selling.Price - Original.Price)/Original.Price ) * 100)

view(df1)

# plot the graph of Profit Vs Original Price
df1 %>% 
  filter(Brand == "Apple" | Brand == "realme" | Brand =="SAMSUNG") %>%
  ggplot(aes(x = Original.Price,y = pct_change,col=Brand)) + geom_point(na.rm = TRUE) + ylim(-70,140)+
  labs(title = "Percentage Change in Selling Price over Original Price of High Rated Brands",
       x = "Original Price", y = "Percentage Change") 



