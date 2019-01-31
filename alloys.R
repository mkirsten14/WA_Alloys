##### Group Project
##### January 29th 2019
##### TEAM 2 
##### Web Analytics Alloy 

library(ggplot2)
library(plotly)
library(sqldf)
library(readxl)
weekly_visits <- read_excel("Web Analytics Case Student Spreadsheet.xls", 
                            sheet = "Weekly Visits", skip = 4)
colnames(weekly_visits)<-c("week","visits","unique_visits","pageviews",
                           "page_visits","avg_time_on_site",
                           "bounce_rate","%_new_visits")

library(readxl)
fin <- read_excel("Web Analytics Case Student Spreadsheet.xls", 
                  sheet = "Financials", skip = 4)

colnames(fin)[4]<-c("pounds_sold")
colnames(fin)[1]<-c("week")

library(readxl)
pounds <- read_excel("Web Analytics Case Student Spreadsheet.xls", 
                     sheet = "Lbs. Sold", skip = 4)
colnames(pounds)<-c("week","pounds_sold")


library(readxl)
daily_visits <- read_excel("Web Analytics Case Student Spreadsheet.xls", 
                           sheet = "Daily Visits", skip = 4)


# Combine weekly visists and financials tab

#install.packages("sqldf")


weekly <- sqldf("SELECT * 
                    FROM weekly_visits a
                    LEFT JOIN fin b
                    ON a.week = b.week
                   ")

#Summary Stats
summary(weekly)
summary(pounds)
summary(daily_visits)

#Standard Deviation 
sd(weekly$visits)
sd(weekly$unique_visits)
sd(weekly$Revenue)
sd(weekly$Profit)
sd(pounds$pounds_sold)


##Promotion Schedule
intial_promotion<- weekly[1:14,]
pre_promotion<- weekly[15:35,]
promotion<- weekly[36:52,]
post_promotion<- weekly[53:66,]

#Pre Website subsets
preweb_2005<-pounds[1:52,]
preweb_2006<-pounds[53:105,]
preweb_2007<-pounds[106:157,]
preweb_2008<-pounds[158:177,]

#Website pounds
initial_pounds<-weekly[1:14,]
pre_pounds<- weekly[15:35,]
pro_pounds<- weekly[36:52,]
post_pounds<- weekly[53:66,]

#means for weekly visits
ini_v_mean<-mean(intial_promotion$visits)
pre_v_mean<- mean(pre_promotion$visits)
promo_v_mean<- mean(promotion$visits)
post_v_mean<- mean(post_promotion$visits)

#means for Unique Visits
ini_uq_mean<-mean(intial_promotion$unique_visits)
pre_uq_mean<- mean(pre_promotion$unique_visits)
promo_uq_mean<- mean(promotion$unique_visits)
post_uq_mean<- mean(post_promotion$unique_visits)

#means for revenue
ini_r_mean<-mean(initial_pounds$Revenue)
pre_r_mean<- mean(pre_pounds$Revenue)
promo_r_mean<- mean(pro_pounds$Revenue)
post_r_mean<- mean(post_pounds$Revenue)

#means for Profit
ini_p_mean<-mean(initial_pounds$Profit)
pre_p_mean<- mean(pre_pounds$Profit)
promo_p_mean<- mean(pro_pounds$Profit)
post_p_mean<- mean(post_pounds$Profit)

#means for LBS Sold
ini_lbs_mean<-mean(initial_pounds$pounds_sold)
pre_lbs_mean<- mean(pre_pounds$pounds_sold)
promo_lbs_mean<- mean(pro_pounds$pounds_sold)
post_lbs_mean<- mean(post_pounds$pounds_sold)

pre_2005_mean<- mean(preweb_2005$pounds_sold)
pre_2006_mean<- mean(preweb_2006$pounds_sold)
pre_2007_mean<- mean(preweb_2007$pounds_sold)
pre_2008_mean<- mean(preweb_2008$pounds_sold)

#Plots

# Revenue vs pounds sold scatter plot
revenue_pounds <- ggplot(weekly, aes(pounds_sold, Revenue)) + geom_point()
print(revenue_pounds)

# Revenue vs pounds sold correlation coefficient
w_pounds <- weekly$pounds_sold
w_Revenue <- weekly$Revenue
revenue_pounds_cor <- cor(w_pounds, w_Revenue)
print(revenue_pounds)

# Revenue vs visits scatter plot
revenue_visits <- ggplot(weekly, aes(visits, Revenue)) + geom_point()
print(revenue_visits)

# Revenue vs visits correlation coefficient
w_visits <- weekly$visits
revenue_visits_cor <- cor(w_visits, w_Revenue)
print(revenue_visits_cor)

# pounds sold vs visits scatter plot
pounds_visits <- ggplot(weekly, aes(visits, pounds_sold)) + geom_point()
print(pounds_visits)

# Revenue vs visits correlation coefficient
pounds_visits_cor <- cor(w_visits, w_pounds)
print(pounds_visits_cor)

# Revenue vs avg time on site scatter plot
revenue_time <- ggplot(weekly, aes(avg_time_on_site, Revenue)) + geom_point()
print(revenue_time)

# Revenue vs avg time on site correlation coefficient
w_time <- weekly$avg_time_on_site
revenue_time_cor <- cor(w_time, w_Revenue)
print(revenue_time_cor)

# Revenue vs page/visit scatter plot
revenue_pv <- ggplot(weekly, aes(page_visits, Revenue)) + geom_point()
print(revenue_pv)

# Revenue vs page/visit correlation coefficient
w_pv <- weekly$page_visits
revenue_pv_cor <- cor(w_pv, w_Revenue)
print(revenue_pv_cor)








