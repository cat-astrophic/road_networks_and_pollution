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
rugged <- read.csv(paste(filepath, 'RuggednessScale2010tracts.csv', sep = ''))

# Merging data

mc <- merge(cities, mechdata, by.x = 'Locations', by.y = 'Location')

for (i in 1:nrow(rugged)) {
  
  if (rugged$CountyFIPS[i] %in% c(36005, 36047, 36061, 36081, 36085)) {
    
    rugged$CountyFIPS[i] <- 36999
    
  }
  
}

locs <- unique(mc$Locations)
fips <- c(13121, 48453, 24005, 01073, 25025, 36029, 37119, 17031, 39061, 39049, 48113, 8031, 26163, 26081, 9003, 48201, 18097, 12031, 29095, 32003, 6037, 21111, 47157, 12025, 55079, 27053, 47037,
          36999, 40109, 12095, 42101, 4013, 42003, 41051, 44007, 37183, 51760, 6065, 36055, 6067, 49035, 48029, 6073, 6075, 6085, 53033, 29510, 12057, 4019, 51820, 11001)

mc.fips <- c()

for (i in 1:nrow(mc)) {
  
  mc.fips <- c(mc.fips, fips[which(locs == mc$Locations[i])])
  
}

mc$FIPS <- mc.fips

rug.area <- c()
rug.road <- c()

for (i in 1:nrow(mc)) {
  
  tmp <- rugged[which(rugged$CountyFIPS == mc$FIPS[i]),]
  rug.area <- c(rug.area, mean(tmp$AreaTRI_Mean))
  rug.road <- c(rug.road, mean(tmp$RoadTRI_Mean))
  
}

mc$Rugged_Area <- rug.area
mc$Rugged_Road <- rug.road

# Running regressions

c1 <- ivreg(Congestion2 ~ Edge.Betweenness.Centrality + Capital + log(Trips) + log(Population) | . - Edge.Betweenness.Centrality + Rugged_Area + Density.sqmi + log(Time), data = mc)
c2 <- ivreg(Congestion2 ~ Load.Centrality + Capital + log(Trips) + log(Population) | . - Load.Centrality + Rugged_Area + Density.sqmi + log(Time), data = mc)
c3 <- ivreg(Congestion2 ~ Circuity + Capital + log(Trips) + log(Population) | . - Circuity + Rugged_Area + Density.sqmi + log(Time), data = mc)

c1x <- coeftest(c1, vcov = vcovCL, cluster = ~Capital)
c2x <- coeftest(c2, vcov = vcovCL, cluster = ~Capital)
c3x <- coeftest(c3, vcov = vcovCL, cluster = ~Capital)

stargazer(c1x, c2x, c3x, type = 'text')

t1 <- ivreg(log(Time) ~ Edge.Betweenness.Centrality + Capital + log(Trips) + log(Population) | . - Edge.Betweenness.Centrality + Rugged_Area + Congestion + Congestion2 + Density.sqmi, data = mc)
t2 <- ivreg(log(Time) ~ Load.Centrality + Capital + log(Trips) + log(Population)  | . - Load.Centrality + Rugged_Area + Congestion + Congestion2 + Density.sqmi, data = mc)
t3 <- ivreg(log(Time) ~ Circuity + Capital + log(Trips) + log(Population)  | . - Circuity + Rugged_Area + Congestion + Congestion2 + Density.sqmi, data = mc)

t1x <- coeftest(t1, vcov = vcovCL, cluster = ~Capital)
t2x <- coeftest(t2, vcov = vcovCL, cluster = ~Capital)
t3x <- coeftest(t3, vcov = vcovCL, cluster = ~Capital)

stargazer(t1x, t2x, t3x, type = 'text')

stargazer(c1x, c2x, c3x, t1x, t2x, t3x, type = 'text')

# Summary statistics

mc$LnPopulation <- log(mc$Population)
keepers <- c('Edge.Betweenness.Centrality', 'Load.Centrality', 'Circuity',
             'Congestion2', 'Time', 'LnPopulation', 'Rugged_Area', 'Capital', 'Trips')
sd <- mc[,names(mc) %in% keepers]
new_names <- c('Mean Edge Betweenness Centrality', 'Mean Load Centrality', 'Circuity', 'Congestion',
               'Mean Commute Time (minutes)', 'Capital', 'Trips (per capita)', 'Ruggedness', 'Ln(Populaton)')

names(sd) <- new_names
datasummary_skim(sd, fmt = '%.3f')

