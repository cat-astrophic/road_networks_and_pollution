# This script performs the econometric analysis for the road networks and pollution project

# Loading libraries

library(stargazer)
library(lmtest)
library(sandwich)
library(jtools)
library(kableExtra)
library(ggplot2)
library(dplyr)
library(modelsummary)
library(systemfit)
library(AER)

# Defining the data input filepath

filepath <- paste('C:/Users/', username, '/Documents/Data/road_networks/data/', sep = '')

# Reading in the data

cities <- read.csv(paste(filepath, 'road_network_statistics_mechanisms.csv', sep = ''))
mechdata <- read.csv(paste(filepath, 'mechanism_data.csv', sep = ''))

# Merging data

mc <- merge(cities, mechdata, by.x = 'Locations', by.y = 'Location')

# Running regressions

c1 <- ivreg(Congestion2 ~ Edge.Betweenness.Centrality + Capital + log(Trips) + log(Population) | . - Edge.Betweenness.Centrality + Density.sqmi + log(Time), data = mc)
c2 <- ivreg(Congestion2 ~ Load.Centrality + Capital + log(Trips) + log(Population) | . - Load.Centrality + Density.sqmi + log(Time), data = mc)
c3 <- ivreg(Congestion2 ~ Circuity + Capital + log(Trips) + log(Population) | . - Circuity + Density.sqmi + log(Time), data = mc)

c1x <- coeftest(c1, vcov = vcovCL, cluster = ~Capital)
c2x <- coeftest(c2, vcov = vcovCL, cluster = ~Capital)
c3x <- coeftest(c3, vcov = vcovCL, cluster = ~Capital)

stargazer(c1x,c2x,c3x, type = 'text')

t1 <- ivreg(log(Time) ~ Edge.Betweenness.Centrality + Capital + log(Trips) + log(Population) | . - Edge.Betweenness.Centrality + Density.sqmi + Congestion + Congestion2, data = mc)
t2 <- ivreg(log(Time) ~ Load.Centrality + Capital + log(Trips) + log(Population) | . - Load.Centrality + Density.sqmi + Congestion + Congestion2, data = mc)
t3 <- ivreg(log(Time) ~ Circuity + Capital + log(Trips) + log(Population) | . - Circuity + Density.sqmi + Congestion + Congestion2, data = mc)

t1x <- coeftest(t1, vcov = vcovCL, cluster = ~Capital)
t2x <- coeftest(t2, vcov = vcovCL, cluster = ~Capital)
t3x <- coeftest(t3, vcov = vcovCL, cluster = ~Capital)

stargazer(t1x,t2x,t3x, type = 'text')

# Summary statistics

mc$LnPopulation <- log(mc$Population)
keepers <- c('Edge.Betweenness.Centrality', 'Load.Centrality', 'Circuity',
             'Congestion2', 'Time', 'LnPopulation', 'Density.sqmi', 'Capital', 'Trips')
sd <- mc[,names(mc) %in% keepers]
new_names <- c('Mean Edge Betweenness Centrality', 'Mean Load Centrality', 'Circuity',
               'Congestion', 'Mean Commute Time', 'Density', 'Capital', 'Trips', 'Ln(Populaton)')

names(sd) <- new_names
datasummary_skim(sd, fmt = '%.3f')

