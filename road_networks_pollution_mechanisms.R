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

c1 <- lm(log(Congestion2) ~ log(Edge.Betweenness.Centrality) + log(Population) + Capital + log(Density.sqmi) + log(Trips), data = mc)
c2 <- lm(log(Congestion2) ~ log(Load.Centrality) + log(Population) + Capital + log(Density.sqmi) + log(Trips), data = mc)
c3 <- lm(log(Congestion2) ~ log(Circuity) + log(Population) + Capital + log(Density.sqmi) + log(Trips), data = mc)

c1x <- coeftest(c1, vcov = vcovCL, cluster = ~Capital)
c2x <- coeftest(c2, vcov = vcovCL, cluster = ~Capital)
c3x <- coeftest(c3, vcov = vcovCL, cluster = ~Capital)

stargazer(c1x,c2x,c3x, type = 'text')

t1 <- lm(log(Time) ~ log(Edge.Betweenness.Centrality) + log(Population) + Capital + log(Density.sqmi) + log(Trips), data = mc)
t2 <- lm(log(Time) ~ log(Load.Centrality) + log(Population) + Capital + log(Density.sqmi) + log(Trips), data = mc)
t3 <- lm(log(Time) ~ log(Circuity) + log(Population) + Capital + log(Density.sqmi) + log(Trips), data = mc)

t1x <- coeftest(t1, vcov = vcovCL, cluster = ~Capital)
t2x <- coeftest(t2, vcov = vcovCL, cluster = ~Capital)
t3x <- coeftest(t3, vcov = vcovCL, cluster = ~Capital)

stargazer(t1x,t2x,t3x, type = 'text')

# Running IV regressions

C.E <- log(Congestion2) ~ log(Edge.Betweenness.Centrality) + log(Population) + Capital + log(Trips) + log(Density.sqmi)
T.E <- log(Time) ~ log(Edge.Betweenness.Centrality) + log(Population) + Capital + log(Trips) + log(Density.sqmi)
sys <- list(C.E,T.E)
instr <- ~ log(Population) + Capital + log(Trips) + log(Density.sqmi) + log(Edge.Betweenness.Centrality)
contime.system.ebcm <- systemfit(sys, inst = instr, method = '2SLS', data = mc)
summary(contime.system.ebcm)

C.E <- log(Congestion2) ~ log(Load.Centrality) + log(Population) + Capital + log(Trips) + log(Density.sqmi)
T.E <- log(Time) ~ log(Load.Centrality) + log(Population) + Capital + log(Trips) + log(Density.sqmi)
sys <- list(C.E,T.E)
instr <- ~ log(Population) + Capital + log(Trips) + log(Density.sqmi) + log(Load.Centrality)
contime.system.lcm <- systemfit(sys, inst = instr, method = '2SLS', data = mc)
summary(contime.system.lcm)

C.E <- log(Congestion2) ~ log(Circuity) + log(Population) + Capital + log(Trips) + log(Density.sqmi)
T.E <- log(Time) ~ log(Circuity) + log(Population) + Capital + log(Trips) + log(Density.sqmi)
sys <- list(C.E,T.E)
instr <- ~ log(Population) + Capital + log(Trips) + log(Density.sqmi) + log(Circuity)
contime.system.circuity <- systemfit(sys, inst = instr, method = '2SLS', data = mc)
summary(contime.system.circuity)

