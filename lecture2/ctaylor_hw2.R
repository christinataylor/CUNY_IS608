
# load data & ibraries

library(ggplot2)
library(plyr)
library(dplyr)
library(data.table)
library(RColorBrewer)
files <- list.files(path = getwd(), pattern = ".csv")
temp <- lapply(files, fread, sep=",", stringsAsFactors = TRUE)
data <- rbindlist(temp, use.names = TRUE, fill = TRUE)

# data exploration on building year
summary(data$YearBuilt)

# the data was published in 2016. however, there are entries "built in year 2040"
# only look at buildings between 1850 and 2016
buildings <- data[which(data$YearBuilt >= 1850 & data$YearBuilt <= 2016),]
ggplot(buildings, aes(YearBuilt)) + geom_histogram(binwidth = 5) 

# 1930s saw the most amount of constructions

buildings$count <- 1 
yr_sum <- aggregate(cbind(count) ~ YearBuilt, data = buildings, sum)

# calculate running total by year
yr_sum$cumsum <- cumsum(yr_sum$count)
ggplot(yr_sum, aes(x=YearBuilt, y=cumsum/nrow(buildings))) + geom_line() + scale_y_continuous(labels = scales::percent)

# cumulative sum confirms most buildings were constructed by the middle of the 20th century

## to capture the last spike in the histogram, we can choose 1960 as the cutoff date

# quick data exploration on number of floors
boxplot(buildings$NumFloors)
summary(buildings$NumFloors)

# select buildings with more than 20 floors
tall <- buildings[which(buildings$NumFloors >= 20),]

# put number floors in bins of 10
tall$bin <- cut(tall$NumFloors, c(0, seq(20, 120, 10), Inf))

# create a graph to show when the tallest buildings were built each decade
getPalette = colorRampPalette(brewer.pal(9, "Greys"))
ggplot(tall, aes(YearBuilt)) + geom_histogram(aes(fill = bin), binwidth=10, colour="grey20", lwd=0.2) + scale_fill_manual(values = getPalette(12)) + scale_y_sqrt()

## skyscreapers first appeared in the 1930s, although the tallest ones were built in the last 10 yrs                     
 
summary(buildings$NumFloors)
# number of floors have zero values. remove those observations
value <- buildings[which(buildings$NumFloors != 0),]

# bin YearsBuilt into 5-yr intervals
value$bin <- cut(value$YearBuilt, c(0, seq(1850, 2020, 5), Inf))
value$fl <- value$AssessTot/value$NumFloors

#average value per 5 year period
val_avg <- aggregate(cbind(fl) ~ bin, data = value, FUN = mean)
ggplot(val_avg, aes(x = bin, y = fl)) + geom_bar(stat = 'identity') + coord_flip()

## indeed buildings constructed between 1940-45 has low average access value. this is, nonetheless, not unique: 1915 - 25 and 1900 - 1905 also produced low value buildings.
