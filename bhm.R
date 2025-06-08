#Name: Jennifer Otiono
#ILRST 2100 - Introduction to Data Science and Statistics
#Final Project


#This is where i got the dataset: https://zenodo.org/records/4650046
#This is the github with the documentation: https://github.com/epfl-dlab/YouNiverse/tree/master?tab=readme-ov-file


#eCornell Hex Codes: 
crimson = '#b31b1b'   #crimson
lightGray = '#cecece' #lightGray
darkGray = '#606366'  #darkGray
skyBlue = '#92b2c4'   #skyblue
gold = '#fbb040'      #gold
ecBlack = '#393f47'   #ecBlack

library(lubridate)

#load dataset

ts_yt <- read.csv("ts_yt.csv")


#number of rows
nrow(ts_yt)

#header
head(ts_yt)


# if filtering just to constrain it to february
#filtered_data <- subset(ts_yt, datetime >= as.POSIXct("2018-02-01") & datetime <= as.POSIXct("2018-03-1"))


#if filter based on content in title and tags
#filtered_data <-ts_yt[grepl("hair",ts_yt$titlE, ignore.case = TRUE) | grepl("hair",ts_yt$tags, ignore.case = TRUE), ]

# I removed dates not within my scope manually using the filter function in excel...cheating but yeah...it allowed me to import file to R by doing the heavy work



#----------HANDLING DATE OBJECT ------------#
#handle date to change label
ts_yt$datetime <- as.Date(ts_yt$datetime, format = "%m/%d/%Y")

ts_yt$period <- NA
  
  
#check for january
ts_yt$period[format(ts_yt$datetime, "%m") == "01"] <- "Before Black History Month"

#check for february
ts_yt$period[format(ts_yt$datetime, "%m") == "02"] <- "Black History Month"

#check for march
ts_yt$period[format(ts_yt$datetime, "%m") == "03"] <- "Post-Black History Month"

#----------HANDLING DATE OBJECT - DONE ------------#



#Univariate Analysis
#Summarization:
#Visualisation: Histogram of Univariate Variable

#of views - Total number of views the channel had this week.
summary(ts_yt$views)
hist(log(ts_yt$views), xlab = "Log of Views", main = "Pre-, During-, Post- Black History Month", breaks = 30)



# Breaking DOWN analysis to TIME PERIODS

#determine average views for the before black history month period
before_bhm <-ts_yt[ts_yt$period == "Before Black History Month",]
average_view_before_bhm <- mean(before_bhm$views)
average_view_before_bhm
median_view_before_bhm <- median(before_bhm$views)
hist(log(before_bhm$views))

#determine average views for the  black history month period
during_bhm <-ts_yt[ts_yt$period == "Black History Month",]
average_view_during_bhm <- mean(during_bhm$views)
average_view_during_bhm
median_view_during_bhm <- median(during_bhm$views)
hist(log(during_bhm$views))

#determine average views for the after black history month period
post_bhm <-ts_yt[ts_yt$period == "Post-Black History Month",]
average_view_post_bhm <- mean(post_bhm$views)
average_view_post_bhm
median_view_post_bhm <- median(post_bhm$views)
hist(log(post_bhm$views))


#-------Bivariate Analysis Visualisation: Side-by-Side Boxplots -------------------#

boxplot(log(before_bhm$views), log(during_bhm$views), log(post_bhm$views), names = c("Before BHM", "BHM", "Post-BHM"), 
        ylab = "Density of Views",
        main = "Differences in the Log of Views by Time Period ")


#------------uncertainty quantification - permutation tool of two numerical groups------------#
obs_stat = mean(before_bhm$views, na.rm=TRUE) - mean(post_bhm$views, na.rm=TRUE)
obs_stat

obs_stat2 = mean(during_bhm$views, na.rm=TRUE) - mean(post_bhm$views, na.rm=TRUE)
obs_stat2

set.seed(1)
P = 1000
store_mean_diff1 = rep(0, P)
store_mean_diff2 = rep(0, P)

for (n in 1:P){
  # Create a data set called before_bhm.perm that will be permuted
  ts_yt_perm = ts_yt
  
  # Permute the data set by randomly shuffling prestige values
  ts_yt_perm$views = sample(ts_yt$views, replace = FALSE)
  
  #Split the permuted views by time period (before, during, after BHM)
  before_views_perm = ts_yt_perm$views[ts_yt_perm$period == "Before Black History Month"]
  during_views_perm = ts_yt_perm$views[ts_yt_perm$period == "Black History Month"]
  post_views_perm = ts_yt_perm$views[ts_yt_perm$period == "Post-Black History Month"]
  
  #mean differences between the differences before and after, during and after
  store_mean_diff1[n] = (mean(before_views_perm, na.rm=TRUE) - mean(post_views_perm, na.rm=TRUE)) + 1e-6
  store_mean_diff2[n] = (mean(during_views_perm, na.rm=TRUE) - mean(post_views_perm, na.rm=TRUE)) + 1e-6
  
}


#permute the dataset many times and create a histogram of the mean sample statistic
hist(log(store_mean_diff1), breaks = 20, freq = FALSE, col = ecBlack, border = 'white',
     xlab = 'Mean Diff of Views',
     main = 'Histogram of View Diff (Permuted Data)')
abline(v = obs_stat, col = skyBlue, lwd = 3)



hist(log(store_mean_diff2), breaks = 20, freq = FALSE, col = ecBlack, border = 'white',
     xlab = 'Mean Diff of Views',
     main = 'Histogram of View Diff (Permuted Data)')
abline(v = obs_stat2, col = skyBlue, lwd = 3)


#CI
#before - post
ci.0.025 =quantile(store_mean_diff1, 0.025)
ci.0.975 =quantile(store_mean_diff1, 0.975)

#during - post
ci2.0.025 =quantile(store_mean_diff2, 0.025)
ci2.0.975 =quantile(store_mean_diff2, 0.975)


# Calculate the p-value: 
pvalue = mean(abs(store_mean_diff1) >= abs(obs_stat))
pvalue


pvalue2 = mean(abs(store_mean_diff2) >= abs(obs_stat))
pvalue2

# Create a boxplot of the permuted data
boxplot(log(before_views_perm), log(during_views_perm), log(post_views_perm), names = c('before', 'during', 'post'),
        ylab = 'Views',
        main = 'Views over Duration (Permuted Data)', col = ecBlack)

#-----------------ADVANCED ANALYSIS-------------------------------------------------------#
#One Way ANOVA
anova_bhm <- aov(views ~ period, data = ts_yt)
summary(anova_bhm)



#--------------------ADVANCED PLOT----------------------------------------------------#

library(ggplot2)

ggplot(ts_yt, aes(x = views)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_grid(period ~ category) +  # Assuming you have a 'category' variable
  labs(title = "Histogram of Views by Period and Category",
       x = "Views", y = "Frequency") +
  theme_minimal()


#-------brain dump --------#
