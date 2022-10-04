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

ebcm_mod_pm <- plm(PM2.5 ~ edge_betweenness_centrality_mean + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday) | . - edge_betweenness_centrality_mean + Density + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
lcm_mod_pm <- plm(PM2.5 ~ load_centrality_mean + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday) | . - load_centrality_mean + Density + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
spm_mod_pm <- plm(PM2.5 ~ shortest_path_mean + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday) | . - shortest_path_mean + Density + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
circuity_mod_pm <- plm(PM2.5 ~ circuity + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday) | . - circuity + Density + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
pct3_mod_pm <- plm(PM2.5 ~ pct_3way_intersections + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday) | . - pct_3way_intersections + Density + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')

stargazer(ebcm_mod_pm, lcm_mod_pm, spm_mod_pm, circuity_mod_pm, pct3_mod_pm, type = 'text')

# Running Hausman-Taylor IV regressions for flows

ebcm_mod_pmd <- plm(PM2.5_Diff ~ edge_betweenness_centrality_mean + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday) | . - edge_betweenness_centrality_mean + Density + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
lcm_mod_pmd <- plm(PM2.5_Diff ~ load_centrality_mean + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday) | . - load_centrality_mean + Density + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
spm_mod_pmd <- plm(PM2.5_Diff ~ shortest_path_mean + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday) | . - shortest_path_mean + Density + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
circuity_mod_pmd <- plm(PM2.5_Diff ~ circuity + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday) | . - circuity + Density + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
pct3_mod_pmd <- plm(PM2.5_Diff ~ pct_3way_intersections + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday) | . - pct_3way_intersections + Density + TEMP + PRCP + WDSP + factor(Locations) + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')

stargazer(ebcm_mod_pmd, lcm_mod_pmd, spm_mod_pmd, circuity_mod_pmd, pct3_mod_pmd, type = 'text')

# Cute sum stats table example

keepers <- c('PM2.5', 'edge_betweenness_centrality_mean', 'load_centrality_mean', 'circuity',
             'pct_3way_intersections', 'Density', 'TEMP', 'PRCP', 'WDSP')

sumdata <- regdata[,names(regdata) %in% keepers]

new_names <- c('Edge Betweenness Centrality (Mean)', 'Load Centrality (Mean)',
               'Circuity', 'Percent 3-way Intersections',  'Particulate Matter (PM2.5)',
               'Temperature', 'Precipitaton', 'Wind Speed', 'Density')

names(sumdata) <- new_names
datasummary_skim(sumdata, fmt = '%.3f')

# Creating a correlation table to justify the HT instrument

corvars <- c('edge_betweenness_centrality_mean', 'load_centrality_mean', 'circuity', 'pct_3way_intersections',
             'PM2.5', 'TEMP', 'PRCP', 'WDSP', 'Density')
cordata <- regdata[,which(names(regdata) %in% corvars)]
cordata <- cordata[which(complete.cases(cordata) == TRUE),]
names(cordata) <- c('MEBC', 'MLC', 'Circuity', 'Pct 3-way', 'PM2.5', 'Temp', 'Precip', 'Wind', 'Density')
cor(cordata)
cor(cordata[,which(names(cordata) %in% c('MEBC', 'MLC', 'Circuity', 'Pct 3-way', 'Density', 'PM2.5'))])

