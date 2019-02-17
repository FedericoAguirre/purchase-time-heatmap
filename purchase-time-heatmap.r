# Author:	Federico Aguirre
# Email:	federico.aguirre.cardiel@gmail.com
# Paypayl:	https://www.paypal.me/FAguirreCardiel
# LinkedIn:	https://www.linkedin.com/in/federico-aguirre-4199933/
# Github:	https://github.com/FedericoAguirre
# Blog:		https://programandochacharas.blogspot.com/

# This script reads two files: dates.csv and products.csv located in subdirectory: data
# After manipulating the data with package dplyr, it creates a dataframe that produces
# a heatmap graphic for analysis.

library("dplyr") # Used for manipulating data
library("ggplot2") # Used for plotting data
library("ggpubr") # Used for organizing graphics display

# Remember to change and set the path, according to your needs
setwd("/path/to/purchase-time-heatmap")
#setwd("/Users/federicoaguirre/Documents/blog/r/purchase-time-heatmap")


# Create hour dataframe
hourDF = data.frame(hour_id=c(0:23), hour=c(0:23))
hourDF$hour =  as.factor(hourDF$hour)

# Read products data, set sku column as factor
productsDF = read.csv("data/products.csv", header = TRUE,
	colClasses = c("integer", "character", "character"))

datesDF = read.csv("data/dates.csv", header = TRUE)
ticketsDF = read.csv("data/tickets.csv", header = TRUE)

# Filtering dates for Octuber 2017, with dplyr
oct17DF = select(datesDF, "date_id", "iso_day") %>%
	filter(date_id >= 20171001 & date_id <20171101)

# Create matrix cross joining days, hours and products
oct17DF$cj = 1
hourDF$cj = 1
productsDF$cj = 1
dayHourProdDF = oct17DF %>% inner_join(hourDF, by='cj') %>%
	inner_join(productsDF, by='cj') %>% select(-cj)

# Create heatmap summary table
# Join of dayHourDF, ticketsDF, productsDF
# Sum of pieces and select of product, hour, iso_day, pieces (sum of pieces)
heatmapDF = left_join(dayHourProdDF, ticketsDF,
	by=c("date_id", "hour_id", "product_id")) %>%
	select("iso_day", "hour", "sku", "pieces") %>%
	mutate(day = iso_day, pieces = coalesce(pieces, 0)) %>%
	group_by(day, hour, sku) %>%
	summarize(pieces = sum(pieces))

# Creating a heatmap for each product
product1HeatmapDF = filter(heatmapDF, sku == "7501064101205")
product2HeatmapDF = filter(heatmapDF, sku == "7501064173202")

# Plotting 2 heatmaps in 2 columns, 1 row image
g1 = ggplot(data = product1HeatmapDF, aes(x = day, y = hour, fill= pieces)) +
	geom_tile() + scale_fill_gradient(low = "white", high = "red") +
	scale_x_continuous("day", labels = as.character(product1HeatmapDF$day),
		breaks = product1HeatmapDF$day, expand = c(0,0)) +
	scale_y_discrete(expand = c(0,0)) +
	ggtitle("SKU 7501064101205 Purchase Time") +
	theme(plot.title = element_text(hjust = 0.5))
g2 = ggplot(data = product2HeatmapDF, aes(x = day, y = hour, fill= pieces)) +
	geom_tile() + scale_fill_gradient(low = "white", high = "red") +
	scale_x_continuous("day", labels = as.character(product2HeatmapDF$day),
		breaks = product2HeatmapDF$day, expand = c(0,0)) +
	scale_y_discrete(expand = c(0,0)) +
	ggtitle("SKU 7501064173202 Purchase Time") +
	theme(plot.title = element_text(hjust = 0.5))
ggarrange(g1, g2, ncol = 2, nrow = 1)
