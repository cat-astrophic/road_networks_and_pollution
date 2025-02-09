# This script performs the econometric analysis for the road networks and pollution project

# Loading libraries

library(plm)
library(stargazer)
library(lmtest)
library(sandwich)
library(jtools)
library(kableExtra)
library(ggplot2)
library(dplyr)
library(modelsummary)

# Defining the data input filepath

filepath <- paste('C:/Users/', username, '/Documents/Data/road_networks/data/', sep = '')

# Reading in the data

regdata <- read.csv(paste(filepath, 'complete_data.csv', sep = ''))
covdata <- read.csv(paste(filepath, 'coVAriates.csv', sep = ''))
rugged <- read.csv(paste(filepath, 'RuggednessScale2010tracts.csv', sep = ''))

# Creating a list of network descriptors

netvars <- colnames(regdata)[3:44]

# Creating additional variables

time <- c()
timebase <- unique(regdata$Date)

for (i in 1:dim(regdata)[1]) {
  
  time <- c(time, which(timebase == regdata$Date[i])[[1]])
  
}

regdata$Month <- floor(regdata$Date / 100)
regdata$Time <- time
regdata$Weekday <- ((regdata$Time %% 7 + 2) %% 7) + 1 # Sunday <- 1, ..., Saturday <- 7

week <- c(1)
w <- 1

for (i in 2:dim(regdata)[1]) {
  
  if (regdata$Weekday[i] == 1 && regdata$Weekday[i-1] == 7) {
    
    w <- w + 1
    
  }
  
  week <-c(week, w)
  
}

regdata$Week <- week

# Merging data.frames

dens <- c()
pops <- c()
grow <- c()
cars <- c()
uic <- c()
mhhi <- c()

for (i in 1:dim(regdata)[1]) {
  
  loc <- regdata$Locations[i]
  idx <- which(covdata$Locations == loc)
  dens <- c(dens, covdata$Density[idx])
  pops <- c(pops, covdata$Population[idx])
  grow <- c(grow, covdata$Growth.Rate[idx])
  cars <- c(cars, covdata$Vehicles[idx])
  uic <- c(uic, covdata$Urban.Influence.Code[idx])
  mhhi <- c(mhhi, covdata$Median.Household.Income[idx])
  
}

regdata$Density <- dens
regdata$Population <- pops
regdata$GR <- grow
regdata$Vehicles <- cars
regdata$UIC <- uic
regdata$Income <- mhhi

# Adding ruggedness data

rugged <- rugged[which(rugged$State == 'VA'),]

r.fips <- unique(rugged$CountyFIPS)
r.area <- c()
r.road <- c()
r.area.sd <- c()
r.road.sd <- c()

for (f in r.fips) {
  
  tmp <- rugged[which(rugged$CountyFIPS == f),]
  r.area <- c(r.area, mean(tmp$AreaTRI_Mean))
  r.road <- c(r.road, mean(tmp$RoadTRI_Mean))
  r.area.sd <- c(r.area.sd, mean(tmp$AreaTRI_StdDev))
  r.road.sd <- c(r.road.sd, mean(tmp$RoadTRI_StdDev))
  
}

rugged.df <- as.data.frame(cbind(r.fips, r.area, r.road, r.area.sd, r.road.sd))

names <- c('Albemarle County', 'Arlington County', 'Caroline County', 'Carroll County', 'Charles City County', 'Chesterfield County', 'Culpeper County', 'Fairfax County', 'Fauquier County', 'Frederick County', 'Giles County', 'Hanover County', 'Henrico County', 'King William County', 'Loudoun County', 'Madison County', 'Page County', 'Prince Edward County', 'Prince William County', 'Roanoke County', 'Rockbridge County', 'Rockingham County', 'Stafford County', 'Warren County', 'Wythe County', 'Alexandria', 'Fredericksburg', 'Hampton', 'Hopewell', 'Lynchburg', 'Newport News', 'Norfolk', 'Richmond', 'Roanoke', 'Salem', 'Suffolk', 'Virginia Beach', 'Winchester')
fips <- c(51003, 51013, 51033, 51035, 51036, 51041, 51047, 51059, 51061, 51069, 51071, 51085, 51087, 51101, 51107, 51113, 51139, 51147, 51153, 51161, 51163, 51165, 51179, 51187, 51197, 51510, 51630, 51650, 51670, 51680, 51700, 51710, 51760, 51770, 51775, 51800, 51810, 51840)
reg.fips <- c()

for (i in 1:nrow(regdata)) {
  
  reg.fips <- c(reg.fips, fips[which(names == regdata$Locations[i])])
  
}

regdata$FIPS <- reg.fips

reg.area <- c()
reg.road <- c()
reg.area.sd <- c()
reg.road.sd <- c()

for (i in 1:nrow(regdata)) {
  
  tmp <- rugged.df[which(rugged.df$r.fips == regdata$FIPS[i]),]
  reg.area <- c(reg.area, tmp$r.area[1])
  reg.road <- c(reg.road, tmp$r.road[1])
  reg.area.sd <- c(reg.area.sd, tmp$r.area.sd[1])
  reg.road.sd <- c(reg.road.sd, tmp$r.road.sd[1])
  
}

regdata$Rugged_Area <- reg.area
regdata$Rugged_Road <- reg.road
regdata$Rugged_Area_SD <- reg.area.sd
regdata$Rugged_Road_SD <- reg.road.sd

# Sorting the data

regdata <- regdata[order(regdata$Locations, regdata$Time),]

# Creating a date index

lx <- unique(regdata$Date)
ud <- c()

for (i in 1:dim(regdata)[1]) {
  
  ud <- c(ud,which(lx == regdata$Date[i]))
  
}

regdata$Date_ID <- ud

# Creating differenced data

pm_diffs <- c(NA)

for (i in 2:dim(regdata)[1]) {
  
  if (regdata$Locations[i] == regdata$Locations[i-1]) {
    
    did1 <- regdata$Date_ID[i]
    did0 <- regdata$Date_ID[i-1]
    diff <- did1 - did0
    
    if (diff == 1) {
      
      pm_diffs <- c(pm_diffs, regdata$PM2.5[i] - regdata$PM2.5[i-1])

    } else {
      
      pm_diffs <- c(pm_diffs,NA)
      
      }
    
  } else {
    
    pm_diffs <- c(pm_diffs,NA)
    
    }
  
}

regdata$PM2.5_Diff <- pm_diffs

# Running Hausman-Taylor IV regressions for stocks

ebcm_mod_pm <- plm(PM2.5 ~ edge_betweenness_centrality_mean + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday) | . - edge_betweenness_centrality_mean + Rugged_Area + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
lcm_mod_pm <- plm(PM2.5 ~ load_centrality_mean + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday) | . - load_centrality_mean + Rugged_Area + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
spm_mod_pm <- plm(PM2.5 ~ shortest_path_mean + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday) | . - shortest_path_mean + Rugged_Area + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
circuity_mod_pm <- plm(PM2.5 ~ circuity + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday) | . - circuity + Rugged_Area + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
pct3_mod_pm <- plm(PM2.5 ~ pct_3way_intersections + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday) | . - pct_3way_intersections + Rugged_Area + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')

stargazer(ebcm_mod_pm, lcm_mod_pm, spm_mod_pm, circuity_mod_pm, pct3_mod_pm, type = 'text', omit = c('Week', 'Locations'))

# Running Hausman-Taylor IV regressions for flows

ebcm_mod_pmd <- plm(PM2.5_Diff ~ edge_betweenness_centrality_mean + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday) | . - edge_betweenness_centrality_mean + Rugged_Area + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
lcm_mod_pmd <- plm(PM2.5_Diff ~ load_centrality_mean + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday) | . - load_centrality_mean + Rugged_Area + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
spm_mod_pmd <- plm(PM2.5_Diff ~ shortest_path_mean + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday) | . - shortest_path_mean + Rugged_Area + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
circuity_mod_pmd <- plm(PM2.5_Diff ~ circuity + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday) | . - circuity + Rugged_Area + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
pct3_mod_pmd <- plm(PM2.5_Diff ~ pct_3way_intersections + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday) | . - pct_3way_intersections + Rugged_Area + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')

stargazer(ebcm_mod_pmd, lcm_mod_pmd, spm_mod_pmd, circuity_mod_pmd, pct3_mod_pmd, type = 'text', omit = c('Week', 'Locations'))

# Cute sum stats table example

keepers <- c('PM2.5', 'edge_betweenness_centrality_mean', 'load_centrality_mean', 'circuity',
   'pct_3way_intersections', 'Rugged_Area', 'TEMP', 'PRCP', 'WDSP')

sumdata <- regdata[,names(regdata) %in% keepers]

new_names <- c('Mean Edge Betweenness Centrality', 'Mean Load Centrality',
     'Circuity', 'Percent 3-way Intersections',  'Particulate Matter (PM2.5) (ug/m^3)',
     'Temperature (°F)', 'Precipitaton (inches)', 'Wind Speed (mph)', 'Ruggedness')

names(sumdata) <- new_names
datasummary_skim(sumdata, fmt = '%.3f')

# Creating a correlation table to justify the HT instrument

corvars <- c('edge_betweenness_centrality_mean', 'load_centrality_mean', 'circuity', 'pct_3way_intersections',
   'PM2.5', 'TEMP', 'PRCP', 'WDSP', 'Rugged_Area')
cordata <- regdata[,which(names(regdata) %in% corvars)]
cordata <- cordata[which(complete.cases(cordata) == TRUE),]
names(cordata) <- c('MEBC', 'MLC', 'Circuity', 'Pct 3-way', 'PM2.5', 'Temp', 'Precip', 'Wind', 'Ruggedness')
cor(cordata)
cor(cordata[,which(names(cordata) %in% c('MEBC', 'MLC', 'Circuity', 'Pct 3-way', 'Ruggedness', 'PM2.5'))])

