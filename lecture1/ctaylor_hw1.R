
# load data & ibraries

library(ggplot2)
library(plyr)
library(RCurl)
link <- getURL("https://raw.githubusercontent.com/jlaurito/CUNY_IS608/master/lecture1/data/inc5000_data.csv")
inc_all <- read.csv(text = link)

# data exploration
head(inc_all)
summary(inc_all)

# check for missing value
sapply(inc_all,function(x) sum(is.na(x)))

# count companies by state
grpby_state <- count(inc_all, "State")

# find state with 3rd most companies
sort_state <- arrange(grpby_state, freq)
head(sort_state, 3)

# make a horizontal bar chart by descending order
sort_state$State <- factor(sort_state$State, levels=sort_state$State)
ggplot(sort_state, aes(x=State, y=freq)) + geom_bar(stat='identity') + coord_flip()

# remove NAs
inc <- inc_all[complete.cases(inc_all), ]

# subset ny data
nyemp <- subset(x = inc, State == 'NY')

# check range of variables
ggplot(nyemp, aes(factor(Industry), Employees)) + geom_boxplot() + coord_flip()

# use boxplot's stats function to remove outliers
rm_o <- function(x) {
  x[x %in% boxplot.stats(x)$out] <- NA
  return(x)
}

# do this for every industry
ny_no <- data.frame()
industries <- levels(nyemp$Industry)
for (industry in industries) {
  sub <- subset(x = nyemp, Industry == industry)
  sub$Employees <- rm_o(sub$Employees)
  ny_no <- rbind(ny_no, sub)
}

# plot the new data
ny_no <- ny_no[complete.cases(ny_no), ]
ggplot(ny_no, aes(Industry, Employees)) + geom_boxplot() + coord_flip() 

# calculate ranges and spread
ny_avg <- ddply(ny_no, .(Industry), summarize, mean <- mean(Employees), sd <- sd(Employees), median <- median(Employees),lower <- quantile(Employees)[2], upper <- quantile(Employees)[4]
) 
names(ny_avg) <- c('Industry', 'mean', 'sd', 'median', 'lower', 'upper')

# plot error bars
ny_avg$ind = reorder(ny_avg$Industry, ny_avg$median)
ggplot(ny_avg, aes(x = ind, y = median)) + geom_bar(stat = "identity") + geom_errorbar(ymin = ny_avg$lower, ymax = ny_avg$upper, width = 0.1,     color = "coral") + coord_flip() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# data exploration on revenye / employee
ggplot(inc, aes(x = Industry, y = Employees)) + geom_point() + coord_flip()
ggplot(inc, aes(x = Industry, y = Revenue)) + geom_point() + coord_flip()

# there are significant outliers. normalize the data 
inc_no <- data.frame()
for (industry in industries) {
  s <- subset(x = inc, Industry == industry)
  s$Employees <- rm_o(s$Employees)
  s$Revenue <- rm_o(s$Revenue)
  inc_no <- rbind(inc_no, s)
}

inc_no <- inc_no[complete.cases(inc_no), ]

# total revenue against total employees
rev_emp <- aggregate(cbind(Employees, Revenue) ~ Industry, data=inc_no, sum, na.rm=TRUE)
ggplot(rev_emp, aes(x=Industry, y=rev_emp$Revenue/rev_emp$Employees)) + geom_bar(stat='identity') + coord_flip()

# spread of revenue per employee
ggplot(inc_no,aes(x = Industry, y = inc_no$Revenue/inc_no$Employees)) + geom_boxplot() + coord_flip()

