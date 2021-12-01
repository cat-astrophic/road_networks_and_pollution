# This script performs the econometric analysis for the road networks and pollution project

# Loading libraries

library(AER)
library(stargazer)
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
regdata$Weekday <- ((regdata$Time %% 7 + 2) %% 7) + 1 # Sunday <- 1, ..., Satuday <- 7

week <- c(1)
w <- 1

for (i in 2:dim(regdata)[1]) {
  
  if (regdata$Weekday[i] == 1 && regdata$Weekday[i-1] == 7) {
    
    w <- w + 1
    
  }
  
  week <-c(week, w)
  
}

regdata$Week <- week

# Sorting the data

regdata <- regdata[order(regdata$Locations, regdata$Time),]

# Creating differenced data

pm_diffs <- c(NA)
no2_diffs <- c(NA)
co_diffs <- c(NA)

for (i in 2:dim(regdata)[1]) {
  
  if ((regdata$Locations[i] == regdata$Locations[i-1]) && (regdata$Time[i] == regdata$Time[i-1] + 1)) {
    
    pm_diffs <- c(pm_diffs, regdata$PM2.5[i] - regdata$PM2.5[i-1])
    no2_diffs <- c(no2_diffs, regdata$NO2[i] - regdata$NO2[i-1])
    co_diffs <- c(co_diffs, regdata$CO[i] - regdata$CO[i-1])
    
  } else {
    
    pm_diffs <- c(pm_diffs, NA)
    no2_diffs <- c(no2_diffs, NA)
    co_diffs <- c(co_diffs, NA)
    
  }
  
}

regdata$PM2.5_Differenced <- pm_diffs
regdata$NO2_Differenced <- no2_diffs
regdata$CO_Differenced <- co_diffs

# Running first differenced IV regressions

ccm_mod_pm <- ivreg(PM2.5_Differenced ~ closeness_centrality_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - closeness_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
dac_mod_pm <- ivreg(PM2.5_Differenced ~ degree_assortativity_coefficient + factor(Week) + factor(Weekday) + factor(Locations) | . - degree_assortativity_coefficient + IV_Count + factor(Week) + factor(Weekday), data = regdata)
dcm_mod_pm <- ivreg(PM2.5_Differenced ~ degree_centrality_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - degree_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
dm_mod_pm <- ivreg(PM2.5_Differenced ~ degree_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - degree_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
density_mod_pm <- ivreg(PM2.5_Differenced ~ density + factor(Week) + factor(Weekday) + factor(Locations) | . - density + IV_Count + factor(Week) + factor(Weekday), data = regdata)
dsp_mod_pm <- ivreg(PM2.5_Differenced ~ dominating_set_proportion + factor(Week) + factor(Weekday) + factor(Locations) | . - dominating_set_proportion  + IV_Count + factor(Week) + factor(Weekday), data = regdata)
em_mod_pm <- ivreg(PM2.5_Differenced ~ eccentricity_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - eccentricity_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
ebcm_mod_pm <- ivreg(PM2.5_Differenced ~ edge_betweenness_centrality_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - edge_betweenness_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
esm_mod_pm <- ivreg(PM2.5_Differenced ~ effective_size_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - effective_size_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
hcm_mod_pm <- ivreg(PM2.5_Differenced ~ harmonic_centrality_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - harmonic_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
idcm_mod_pm <- ivreg(PM2.5_Differenced ~ in_degree_centrality_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - in_degree_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
lcm_mod_pm <- ivreg(PM2.5_Differenced ~ load_centrality_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - load_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
odcm_mod_pm <- ivreg(PM2.5_Differenced ~ out_degree_centrality_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - out_degree_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
pm_mod_pm <- ivreg(PM2.5_Differenced ~ pagerank_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - pagerank_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
pcm_mod_pm <- ivreg(PM2.5_Differenced ~ percolation_centrality_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - percolation_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
reciprocity_mod_pm <- ivreg(PM2.5_Differenced ~ reciprocity + factor(Week) + factor(Weekday) + factor(Locations) | . - reciprocity + IV_Count + factor(Week) + factor(Weekday), data = regdata)
spm_mod_pm <- ivreg(PM2.5_Differenced ~ shortest_path_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - shortest_path_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
elm_mod_pm <- ivreg(PM2.5_Differenced ~ edge_length_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - edge_length_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
spnm_mod_pm <- ivreg(PM2.5_Differenced ~ streets_per_node_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - streets_per_node_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
slm_mod_pm <- ivreg(PM2.5_Differenced ~ street_length_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - street_length_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
circuity_mod_pm <- ivreg(PM2.5_Differenced ~ circuity + factor(Week) + factor(Weekday) + factor(Locations) | . - circuity + IV_Count + factor(Week) + factor(Weekday), data = regdata)
slp_mod_pm <- ivreg(PM2.5_Differenced ~ self_loop_proportion + factor(Week) + factor(Weekday) + factor(Locations) | . - self_loop_proportion + IV_Count + factor(Week) + factor(Weekday), data = regdata)
ndkm_mod_pm <- ivreg(PM2.5_Differenced ~ node_density_km + factor(Week) + factor(Weekday) + factor(Locations) | . - node_density_km + IV_Count + factor(Week) + factor(Weekday), data = regdata)
idkm_mod_pm <- ivreg(PM2.5_Differenced ~ intersection_density_km + factor(Week) + factor(Weekday) + factor(Locations) | . - intersection_density_km + IV_Count + factor(Week) + factor(Weekday), data = regdata)
edkm_mod_pm <- ivreg(PM2.5_Differenced ~ edge_density_km + factor(Week) + factor(Weekday) + factor(Locations) | . - edge_density_km + IV_Count + factor(Week) + factor(Weekday), data = regdata)
sdkm_mod_pm <- ivreg(PM2.5_Differenced ~ street_density_km + factor(Week) + factor(Weekday) + factor(Locations) | . - street_density_km + IV_Count + factor(Week) + factor(Weekday), data = regdata)
pct4_mod_pm <- ivreg(PM2.5_Differenced ~ pct_4way_intersections + factor(Week) + factor(Weekday) + factor(Locations) | . - pct_4way_intersections + IV_Count + factor(Week) + factor(Weekday), data = regdata)
pct3_mod_pm <- ivreg(PM2.5_Differenced ~ pct_3way_intersections + factor(Week) + factor(Weekday) + factor(Locations) | . - pct_3way_intersections + IV_Count + factor(Week) + factor(Weekday), data = regdata)

ccm_mod_no2 <- ivreg(NO2_Differenced ~ closeness_centrality_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - closeness_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
dac_mod_no2 <- ivreg(NO2_Differenced ~ degree_assortativity_coefficient + factor(Week) + factor(Weekday) + factor(Locations) | . - degree_assortativity_coefficient + IV_Count + factor(Week) + factor(Weekday), data = regdata)
dcm_mod_no2 <- ivreg(NO2_Differenced ~ degree_centrality_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - degree_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
dm_mod_no2 <- ivreg(NO2_Differenced ~ degree_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - degree_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
density_mod_no2 <- ivreg(NO2_Differenced ~ density + factor(Week) + factor(Weekday) + factor(Locations) | . - density + IV_Count + factor(Week) + factor(Weekday), data = regdata)
dsp_mod_no2 <- ivreg(NO2_Differenced ~ dominating_set_proportion + factor(Week) + factor(Weekday) + factor(Locations) | . - dominating_set_proportion  + IV_Count + factor(Week) + factor(Weekday), data = regdata)
em_mod_no2 <- ivreg(NO2_Differenced ~ eccentricity_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - eccentricity_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
ebcm_mod_no2 <- ivreg(NO2_Differenced ~ edge_betweenness_centrality_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - edge_betweenness_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
esm_mod_no2 <- ivreg(NO2_Differenced ~ effective_size_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - effective_size_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
hcm_mod_no2 <- ivreg(NO2_Differenced ~ harmonic_centrality_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - harmonic_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
idcm_mod_no2 <- ivreg(NO2_Differenced ~ in_degree_centrality_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - in_degree_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
lcm_mod_no2 <- ivreg(NO2_Differenced ~ load_centrality_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - load_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
odcm_mod_no2 <- ivreg(NO2_Differenced ~ out_degree_centrality_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - out_degree_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
pm_mod_no2 <- ivreg(NO2_Differenced ~ pagerank_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - pagerank_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
pcm_mod_no2 <- ivreg(NO2_Differenced ~ percolation_centrality_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - percolation_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
reciprocity_mod_no2 <- ivreg(NO2_Differenced ~ reciprocity + factor(Week) + factor(Weekday) + factor(Locations) | . - reciprocity + IV_Count + factor(Week) + factor(Weekday), data = regdata)
spm_mod_no2 <- ivreg(NO2_Differenced ~ shortest_path_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - shortest_path_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
elm_mod_no2 <- ivreg(NO2_Differenced ~ edge_length_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - edge_length_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
spnm_mod_no2 <- ivreg(NO2_Differenced ~ streets_per_node_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - streets_per_node_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
slm_mod_no2 <- ivreg(NO2_Differenced ~ street_length_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - street_length_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
circuity_mod_no2 <- ivreg(NO2_Differenced ~ circuity + factor(Week) + factor(Weekday) + factor(Locations) | . - circuity + IV_Count + factor(Week) + factor(Weekday), data = regdata)
slp_mod_no2 <- ivreg(NO2_Differenced ~ self_loop_proportion + factor(Week) + factor(Weekday) + factor(Locations) | . - self_loop_proportion + IV_Count + factor(Week) + factor(Weekday), data = regdata)
ndkm_mod_no2 <- ivreg(NO2_Differenced ~ node_density_km + factor(Week) + factor(Weekday) + factor(Locations) | . - node_density_km + IV_Count + factor(Week) + factor(Weekday), data = regdata)
idkm_mod_no2 <- ivreg(NO2_Differenced ~ intersection_density_km + factor(Week) + factor(Weekday) + factor(Locations) | . - intersection_density_km + IV_Count + factor(Week) + factor(Weekday), data = regdata)
edkm_mod_no2 <- ivreg(NO2_Differenced ~ edge_density_km + factor(Week) + factor(Weekday) + factor(Locations) | . - edge_density_km + IV_Count + factor(Week) + factor(Weekday), data = regdata)
sdkm_mod_no2 <- ivreg(NO2_Differenced ~ street_density_km + factor(Week) + factor(Weekday) + factor(Locations) | . - street_density_km + IV_Count + factor(Week) + factor(Weekday), data = regdata)
pct4_mod_no2 <- ivreg(NO2_Differenced ~ pct_4way_intersections + factor(Week) + factor(Weekday) + factor(Locations) | . - pct_4way_intersections + IV_Count + factor(Week) + factor(Weekday), data = regdata)
pct3_mod_no2 <- ivreg(NO2_Differenced ~ pct_3way_intersections + factor(Week) + factor(Weekday) + factor(Locations) | . - pct_3way_intersections + IV_Count + factor(Week) + factor(Weekday), data = regdata)

ccm_mod_co <- ivreg(CO_Differenced ~ closeness_centrality_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - closeness_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
dac_mod_co <- ivreg(CO_Differenced ~ degree_assortativity_coefficient + factor(Week) + factor(Weekday) + factor(Locations) | . - degree_assortativity_coefficient + IV_Count + factor(Week) + factor(Weekday), data = regdata)
dcm_mod_co <- ivreg(CO_Differenced ~ degree_centrality_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - degree_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
dm_mod_co <- ivreg(CO_Differenced ~ degree_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - degree_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
density_mod_co <- ivreg(CO_Differenced ~ density + factor(Week) + factor(Weekday) + factor(Locations) | . - density + IV_Count + factor(Week) + factor(Weekday), data = regdata)
dsp_mod_co <- ivreg(CO_Differenced ~ dominating_set_proportion + factor(Week) + factor(Weekday) + factor(Locations) | . - dominating_set_proportion  + IV_Count + factor(Week) + factor(Weekday), data = regdata)
em_mod_co <- ivreg(CO_Differenced ~ eccentricity_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - eccentricity_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
ebcm_mod_co <- ivreg(CO_Differenced ~ edge_betweenness_centrality_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - edge_betweenness_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
esm_mod_co <- ivreg(CO_Differenced ~ effective_size_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - effective_size_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
hcm_mod_co <- ivreg(CO_Differenced ~ harmonic_centrality_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - harmonic_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
idcm_mod_co <- ivreg(CO_Differenced ~ in_degree_centrality_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - in_degree_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
lcm_mod_co <- ivreg(CO_Differenced ~ load_centrality_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - load_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
odcm_mod_co <- ivreg(CO_Differenced ~ out_degree_centrality_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - out_degree_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
pm_mod_co <- ivreg(CO_Differenced ~ pagerank_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - pagerank_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
pcm_mod_co <- ivreg(CO_Differenced ~ percolation_centrality_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - percolation_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
reciprocity_mod_co <- ivreg(CO_Differenced ~ reciprocity + factor(Week) + factor(Weekday) + factor(Locations) | . - reciprocity + IV_Count + factor(Week) + factor(Weekday), data = regdata)
spm_mod_co <- ivreg(CO_Differenced ~ shortest_path_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - shortest_path_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
elm_mod_co <- ivreg(CO_Differenced ~ edge_length_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - edge_length_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
spnm_mod_co <- ivreg(CO_Differenced ~ streets_per_node_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - streets_per_node_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
slm_mod_co <- ivreg(CO_Differenced ~ street_length_mean + factor(Week) + factor(Weekday) + factor(Locations) | . - street_length_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
circuity_mod_co <- ivreg(CO_Differenced ~ circuity + factor(Week) + factor(Weekday) + factor(Locations) | . - circuity + IV_Count + factor(Week) + factor(Weekday), data = regdata)
slp_mod_co <- ivreg(CO_Differenced ~ self_loop_proportion + factor(Week) + factor(Weekday) + factor(Locations) | . - self_loop_proportion + IV_Count + factor(Week) + factor(Weekday), data = regdata)
ndkm_mod_co <- ivreg(CO_Differenced ~ node_density_km + factor(Week) + factor(Weekday) + factor(Locations) | . - node_density_km + IV_Count + factor(Week) + factor(Weekday), data = regdata)
idkm_mod_co <- ivreg(CO_Differenced ~ intersection_density_km + factor(Week) + factor(Weekday) + factor(Locations) | . - intersection_density_km + IV_Count + factor(Week) + factor(Weekday), data = regdata)
edkm_mod_co <- ivreg(CO_Differenced ~ edge_density_km + factor(Week) + factor(Weekday) + factor(Locations) | . - edge_density_km + IV_Count + factor(Week) + factor(Weekday), data = regdata)
sdkm_mod_co <- ivreg(CO_Differenced ~ street_density_km + factor(Week) + factor(Weekday) + factor(Locations) | . - street_density_km + IV_Count + factor(Week) + factor(Weekday), data = regdata)
pct4_mod_co <- ivreg(CO_Differenced ~ pct_4way_intersections + factor(Week) + factor(Weekday) + factor(Locations) | . - pct_4way_intersections + IV_Count + factor(Week) + factor(Weekday), data = regdata)
pct3_mod_co <- ivreg(CO_Differenced ~ pct_3way_intersections + factor(Week) + factor(Weekday) + factor(Locations) | . - pct_3way_intersections + IV_Count + factor(Week) + factor(Weekday), data = regdata)

# Heteroskedasticity robust standard errors

t_stats_pm <- c()

cov <- vcovHC(ccm_mod_pm, type = 'HC0')
t_stats_pm <- c(t_stats_pm, sqrt(diag(cov))[2])

cov <- vcovHC(dac_mod_pm, type = 'HC0')
t_stats_pm <- c(t_stats_pm, sqrt(diag(cov))[2])

cov <- vcovHC(dcm_mod_pm, type = 'HC0')
t_stats_pm <- c(t_stats_pm, sqrt(diag(cov))[2])

cov <- vcovHC(dm_mod_pm, type = 'HC0')
t_stats_pm <- c(t_stats_pm, sqrt(diag(cov))[2])

cov <- vcovHC(density_mod_pm, type = 'HC0')
t_stats_pm <- c(t_stats_pm, sqrt(diag(cov))[2])

cov <- vcovHC(dsp_mod_pm, type = 'HC0')
t_stats_pm <- c(t_stats_pm, sqrt(diag(cov))[2])

cov <- vcovHC(em_mod_pm, type = 'HC0')
t_stats_pm <- c(t_stats_pm, sqrt(diag(cov))[2])

cov <- vcovHC(ebcm_mod_pm, type = 'HC0')
t_stats_pm <- c(t_stats_pm, sqrt(diag(cov))[2])

cov <- vcovHC(esm_mod_pm, type = 'HC0')
t_stats_pm <- c(t_stats_pm, sqrt(diag(cov))[2])

cov <- vcovHC(hcm_mod_pm, type = 'HC0')
t_stats_pm <- c(t_stats_pm, sqrt(diag(cov))[2])

cov <- vcovHC(idcm_mod_pm, type = 'HC0')
t_stats_pm <- c(t_stats_pm, sqrt(diag(cov))[2])

cov <- vcovHC(lcm_mod_pm, type = 'HC0')
t_stats_pm <- c(t_stats_pm, sqrt(diag(cov))[2])

cov <- vcovHC(odcm_mod_pm, type = 'HC0')
t_stats_pm <- c(t_stats_pm, sqrt(diag(cov))[2])

cov <- vcovHC(pm_mod_pm, type = 'HC0')
t_stats_pm <- c(t_stats_pm, sqrt(diag(cov))[2])

cov <- vcovHC(pcm_mod_pm, type = 'HC0')
t_stats_pm <- c(t_stats_pm, sqrt(diag(cov))[2])

cov <- vcovHC(reciprocity_mod_pm, type = 'HC0')
t_stats_pm <- c(t_stats_pm, sqrt(diag(cov))[2])

cov <- vcovHC(spm_mod_pm, type = 'HC0')
t_stats_pm <- c(t_stats_pm, sqrt(diag(cov))[2])

cov <- vcovHC(elm_mod_pm, type = 'HC0')
t_stats_pm <- c(t_stats_pm, sqrt(diag(cov))[2])

cov <- vcovHC(spnm_mod_pm, type = 'HC0')
t_stats_pm <- c(t_stats_pm, sqrt(diag(cov))[2])

cov <- vcovHC(slm_mod_pm, type = 'HC0')
t_stats_pm <- c(t_stats_pm, sqrt(diag(cov))[2])

cov <- vcovHC(circuity_mod_pm, type = 'HC0')
t_stats_pm <- c(t_stats_pm, sqrt(diag(cov))[2])

cov <- vcovHC(slp_mod_pm, type = 'HC0')
t_stats_pm <- c(t_stats_pm, sqrt(diag(cov))[2])

cov <- vcovHC(ndkm_mod_pm, type = 'HC0')
t_stats_pm <- c(t_stats_pm, sqrt(diag(cov))[2])

cov <- vcovHC(idkm_mod_pm, type = 'HC0')
t_stats_pm <- c(t_stats_pm, sqrt(diag(cov))[2])

cov <- vcovHC(edkm_mod_pm, type = 'HC0')
t_stats_pm <- c(t_stats_pm, sqrt(diag(cov))[2])

cov <- vcovHC(sdkm_mod_pm, type = 'HC0')
t_stats_pm <- c(t_stats_pm, sqrt(diag(cov))[2])

cov <- vcovHC(pct4_mod_pm, type = 'HC0')
t_stats_pm <- c(t_stats_pm, sqrt(diag(cov))[2])

cov <- vcovHC(pct3_mod_pm, type = 'HC0')
t_stats_pm <- c(t_stats_pm, sqrt(diag(cov))[2])

t_stats_no2 <- c()

cov <- vcovHC(ccm_mod_no2, type = 'HC0')
t_stats_no2 <- c(t_stats_no2, sqrt(diag(cov))[2])

cov <- vcovHC(dac_mod_no2, type = 'HC0')
t_stats_no2 <- c(t_stats_no2, sqrt(diag(cov))[2])

cov <- vcovHC(dcm_mod_no2, type = 'HC0')
t_stats_no2 <- c(t_stats_no2, sqrt(diag(cov))[2])

cov <- vcovHC(dm_mod_no2, type = 'HC0')
t_stats_no2 <- c(t_stats_no2, sqrt(diag(cov))[2])

cov <- vcovHC(density_mod_no2, type = 'HC0')
t_stats_no2 <- c(t_stats_no2, sqrt(diag(cov))[2])

cov <- vcovHC(dsp_mod_no2, type = 'HC0')
t_stats_no2 <- c(t_stats_no2, sqrt(diag(cov))[2])

cov <- vcovHC(em_mod_no2, type = 'HC0')
t_stats_no2 <- c(t_stats_no2, sqrt(diag(cov))[2])

cov <- vcovHC(ebcm_mod_no2, type = 'HC0')
t_stats_no2 <- c(t_stats_no2, sqrt(diag(cov))[2])

cov <- vcovHC(esm_mod_no2, type = 'HC0')
t_stats_no2 <- c(t_stats_no2, sqrt(diag(cov))[2])

cov <- vcovHC(hcm_mod_no2, type = 'HC0')
t_stats_no2 <- c(t_stats_no2, sqrt(diag(cov))[2])

cov <- vcovHC(idcm_mod_no2, type = 'HC0')
t_stats_no2 <- c(t_stats_no2, sqrt(diag(cov))[2])

cov <- vcovHC(lcm_mod_no2, type = 'HC0')
t_stats_no2 <- c(t_stats_no2, sqrt(diag(cov))[2])

cov <- vcovHC(odcm_mod_no2, type = 'HC0')
t_stats_no2 <- c(t_stats_no2, sqrt(diag(cov))[2])

cov <- vcovHC(pm_mod_no2, type = 'HC0')
t_stats_no2 <- c(t_stats_no2, sqrt(diag(cov))[2])

cov <- vcovHC(pcm_mod_no2, type = 'HC0')
t_stats_no2 <- c(t_stats_no2, sqrt(diag(cov))[2])

cov <- vcovHC(reciprocity_mod_no2, type = 'HC0')
t_stats_no2 <- c(t_stats_no2, sqrt(diag(cov))[2])

cov <- vcovHC(spm_mod_no2, type = 'HC0')
t_stats_no2 <- c(t_stats_no2, sqrt(diag(cov))[2])

cov <- vcovHC(elm_mod_no2, type = 'HC0')
t_stats_no2 <- c(t_stats_no2, sqrt(diag(cov))[2])

cov <- vcovHC(spnm_mod_no2, type = 'HC0')
t_stats_no2 <- c(t_stats_no2, sqrt(diag(cov))[2])

cov <- vcovHC(slm_mod_no2, type = 'HC0')
t_stats_no2 <- c(t_stats_no2, sqrt(diag(cov))[2])

cov <- vcovHC(circuity_mod_no2, type = 'HC0')
t_stats_no2 <- c(t_stats_no2, sqrt(diag(cov))[2])

cov <- vcovHC(slp_mod_no2, type = 'HC0')
t_stats_no2 <- c(t_stats_no2, sqrt(diag(cov))[2])

cov <- vcovHC(ndkm_mod_no2, type = 'HC0')
t_stats_no2 <- c(t_stats_no2, sqrt(diag(cov))[2])

cov <- vcovHC(idkm_mod_no2, type = 'HC0')
t_stats_no2 <- c(t_stats_no2, sqrt(diag(cov))[2])

cov <- vcovHC(edkm_mod_no2, type = 'HC0')
t_stats_no2 <- c(t_stats_no2, sqrt(diag(cov))[2])

cov <- vcovHC(sdkm_mod_no2, type = 'HC0')
t_stats_no2 <- c(t_stats_no2, sqrt(diag(cov))[2])

cov <- vcovHC(pct4_mod_no2, type = 'HC0')
t_stats_no2 <- c(t_stats_no2, sqrt(diag(cov))[2])

cov <- vcovHC(pct3_mod_no2, type = 'HC0')
t_stats_no2 <- c(t_stats_no2, sqrt(diag(cov))[2])

t_stats_co <- c()

cov <- vcovHC(ccm_mod_co, type = 'HC0')
t_stats_co <- c(t_stats_co, sqrt(diag(cov))[2])

cov <- vcovHC(dac_mod_co, type = 'HC0')
t_stats_co <- c(t_stats_co, sqrt(diag(cov))[2])

cov <- vcovHC(dcm_mod_co, type = 'HC0')
t_stats_co <- c(t_stats_co, sqrt(diag(cov))[2])

cov <- vcovHC(dm_mod_co, type = 'HC0')
t_stats_co <- c(t_stats_co, sqrt(diag(cov))[2])

cov <- vcovHC(density_mod_co, type = 'HC0')
t_stats_co <- c(t_stats_co, sqrt(diag(cov))[2])

cov <- vcovHC(dsp_mod_co, type = 'HC0')
t_stats_co <- c(t_stats_co, sqrt(diag(cov))[2])

cov <- vcovHC(em_mod_co, type = 'HC0')
t_stats_co <- c(t_stats_co, sqrt(diag(cov))[2])

cov <- vcovHC(ebcm_mod_co, type = 'HC0')
t_stats_co <- c(t_stats_co, sqrt(diag(cov))[2])

cov <- vcovHC(esm_mod_co, type = 'HC0')
t_stats_co <- c(t_stats_co, sqrt(diag(cov))[2])

cov <- vcovHC(hcm_mod_co, type = 'HC0')
t_stats_co <- c(t_stats_co, sqrt(diag(cov))[2])

cov <- vcovHC(idcm_mod_co, type = 'HC0')
t_stats_co <- c(t_stats_co, sqrt(diag(cov))[2])

cov <- vcovHC(lcm_mod_co, type = 'HC0')
t_stats_co <- c(t_stats_co, sqrt(diag(cov))[2])

cov <- vcovHC(odcm_mod_co, type = 'HC0')
t_stats_co <- c(t_stats_co, sqrt(diag(cov))[2])

cov <- vcovHC(pm_mod_co, type = 'HC0')
t_stats_co <- c(t_stats_co, sqrt(diag(cov))[2])

cov <- vcovHC(pcm_mod_co, type = 'HC0')
t_stats_co <- c(t_stats_co, sqrt(diag(cov))[2])

cov <- vcovHC(reciprocity_mod_co, type = 'HC0')
t_stats_co <- c(t_stats_co, sqrt(diag(cov))[2])

cov <- vcovHC(spm_mod_co, type = 'HC0')
t_stats_co <- c(t_stats_co, sqrt(diag(cov))[2])

cov <- vcovHC(elm_mod_co, type = 'HC0')
t_stats_co <- c(t_stats_co, sqrt(diag(cov))[2])

cov <- vcovHC(spnm_mod_co, type = 'HC0')
t_stats_co <- c(t_stats_co, sqrt(diag(cov))[2])

cov <- vcovHC(slm_mod_co, type = 'HC0')
t_stats_co <- c(t_stats_co, sqrt(diag(cov))[2])

cov <- vcovHC(circuity_mod_co, type = 'HC0')
t_stats_co <- c(t_stats_co, sqrt(diag(cov))[2])

cov <- vcovHC(slp_mod_co, type = 'HC0')
t_stats_co <- c(t_stats_co, sqrt(diag(cov))[2])

cov <- vcovHC(ndkm_mod_co, type = 'HC0')
t_stats_co <- c(t_stats_co, sqrt(diag(cov))[2])

cov <- vcovHC(idkm_mod_co, type = 'HC0')
t_stats_co <- c(t_stats_co, sqrt(diag(cov))[2])

cov <- vcovHC(edkm_mod_co, type = 'HC0')
t_stats_co <- c(t_stats_co, sqrt(diag(cov))[2])

cov <- vcovHC(sdkm_mod_co, type = 'HC0')
t_stats_co <- c(t_stats_co, sqrt(diag(cov))[2])

cov <- vcovHC(pct4_mod_co, type = 'HC0')
t_stats_co <- c(t_stats_co, sqrt(diag(cov))[2])

cov <- vcovHC(pct3_mod_co, type = 'HC0')
t_stats_co <- c(t_stats_co, sqrt(diag(cov))[2])

# Storing results

coeffs_pm <- c(summary(ccm_mod_pm)$coefficients[,1][2], summary(dac_mod_pm)$coefficients[,1][2],
               summary(dcm_mod_pm)$coefficients[,1][2], summary(dm_mod_pm)$coefficients[,1][2],
               summary(density_mod_pm)$coefficients[,1][2], summary(dsp_mod_pm)$coefficients[,1][2],
               summary(em_mod_pm)$coefficients[,1][2], summary(ebcm_mod_pm)$coefficients[,1][2],
               summary(esm_mod_pm)$coefficients[,1][2],
               summary(hcm_mod_pm)$coefficients[,1][2], summary(idcm_mod_pm)$coefficients[,1][2],
               summary(lcm_mod_pm)$coefficients[,1][2], summary(odcm_mod_pm)$coefficients[,1][2],
               summary(pm_mod_pm)$coefficients[,1][2], summary(pcm_mod_pm)$coefficients[,1][2],
               summary(reciprocity_mod_pm)$coefficients[,1][2],
               summary(spm_mod_pm)$coefficients[,1][2],
               summary(elm_mod_pm)$coefficients[,1][2], summary(spnm_mod_pm)$coefficients[,1][2],
               summary(slm_mod_pm)$coefficients[,1][2], summary(circuity_mod_pm)$coefficients[,1][2],
               summary(slp_mod_pm)$coefficients[,1][2], summary(ndkm_mod_pm)$coefficients[,1][2],
               summary(idkm_mod_pm)$coefficients[,1][2], summary(edkm_mod_pm)$coefficients[,1][2],
               summary(sdkm_mod_pm)$coefficients[,1][2], summary(pct4_mod_pm)$coefficients[,1][2],
               summary(pct3_mod_pm)$coefficients[,1][2])

coeffs_no2 <- c(summary(ccm_mod_no2)$coefficients[,1][2], summary(dac_mod_no2)$coefficients[,1][2],
                summary(dcm_mod_no2)$coefficients[,1][2], summary(dm_mod_no2)$coefficients[,1][2],
                summary(density_mod_no2)$coefficients[,1][2], summary(dsp_mod_no2)$coefficients[,1][2],
                summary(em_mod_no2)$coefficients[,1][2], summary(ebcm_mod_no2)$coefficients[,1][2],
                summary(esm_mod_no2)$coefficients[,1][2],
                summary(hcm_mod_no2)$coefficients[,1][2], summary(idcm_mod_no2)$coefficients[,1][2],
                summary(lcm_mod_no2)$coefficients[,1][2], summary(odcm_mod_no2)$coefficients[,1][2],
                summary(pm_mod_no2)$coefficients[,1][2], summary(pcm_mod_no2)$coefficients[,1][2],
                summary(reciprocity_mod_no2)$coefficients[,1][2],
                summary(spm_mod_no2)$coefficients[,1][2],
                summary(elm_mod_no2)$coefficients[,1][2], summary(spnm_mod_no2)$coefficients[,1][2],
                summary(slm_mod_no2)$coefficients[,1][2], summary(circuity_mod_no2)$coefficients[,1][2],
                summary(slp_mod_no2)$coefficients[,1][2], summary(ndkm_mod_no2)$coefficients[,1][2],
                summary(idkm_mod_no2)$coefficients[,1][2], summary(edkm_mod_no2)$coefficients[,1][2],
                summary(sdkm_mod_no2)$coefficients[,1][2], summary(pct4_mod_no2)$coefficients[,1][2],
                summary(pct3_mod_no2)$coefficients[,1][2])

coeffs_co <- c(summary(ccm_mod_co)$coefficients[,1][2], summary(dac_mod_co)$coefficients[,1][2],
               summary(dcm_mod_co)$coefficients[,1][2], summary(dm_mod_co)$coefficients[,1][2],
               summary(density_mod_co)$coefficients[,1][2], summary(dsp_mod_co)$coefficients[,1][2],
               summary(em_mod_co)$coefficients[,1][2], summary(ebcm_mod_co)$coefficients[,1][2],
               summary(esm_mod_co)$coefficients[,1][2],
               summary(hcm_mod_co)$coefficients[,1][2], summary(idcm_mod_co)$coefficients[,1][2],
               summary(lcm_mod_co)$coefficients[,1][2], summary(odcm_mod_co)$coefficients[,1][2],
               summary(pm_mod_co)$coefficients[,1][2], summary(pcm_mod_co)$coefficients[,1][2],
               summary(reciprocity_mod_co)$coefficients[,1][2],
               summary(spm_mod_co)$coefficients[,1][2],
               summary(elm_mod_co)$coefficients[,1][2], summary(spnm_mod_co)$coefficients[,1][2],
               summary(slm_mod_co)$coefficients[,1][2], summary(circuity_mod_co)$coefficients[,1][2],
               summary(slp_mod_co)$coefficients[,1][2], summary(ndkm_mod_co)$coefficients[,1][2],
               summary(idkm_mod_co)$coefficients[,1][2], summary(edkm_mod_co)$coefficients[,1][2],
               summary(sdkm_mod_co)$coefficients[,1][2], summary(pct4_mod_co)$coefficients[,1][2],
               summary(pct3_mod_co)$coefficients[,1][2])

signif_pm <- c()
signif_no2 <- c()
signif_co <- c()

for (t in t_stats_pm) {
  
  if (t > 2.576) {
    
    signif_pm <- c(signif_pm, '***')
    
  } else if (t > 1.96) {
    
    signif_pm <- c(signif_pm, '**')
    
  } else if (t > 1.645) {
    
    signif_pm <- c(signif_pm, '*')
    
  } else {
    
    signif_pm <- c(signif_pm, '')
    
  }
  
}

for (t in t_stats_no2) {
  
  if (t > 2.576) {
    
    signif_no2 <- c(signif_no2, '***')
    
  } else if (t > 1.96) {
    
    signif_no2 <- c(signif_no2, '**')
    
  } else if (t > 1.645) {
    
    signif_no2 <- c(signif_no2, '*')
    
  } else {
    
    signif_no2 <- c(signif_no2, '')
    
  }
  
}

for (t in t_stats_co) {
  
  if (t > 2.576) {
    
    signif_co <- c(signif_co, '***')
    
  } else if (t > 1.96) {
    
    signif_co <- c(signif_co, '**')
    
  } else if (t > 1.645) {
    
    signif_co <- c(signif_co, '*')
    
  } else {
    
    signif_co <- c(signif_co, '')
    
  }
  
}

pm_df <- cbind(coeffs_pm, signif_pm, t_stats_pm)
no2_df <- cbind(coeffs_no2, signif_no2, t_stats_no2)
co_df <- cbind(coeffs_co, signif_co, t_stats_co)

write.csv(pm_df, paste('C:/Users/', username, '/Documents/Data/road_networks/results/1d/pm_results.csv', sep = ''))
write.csv(no2_df, paste('C:/Users/', username, '/Documents/Data/road_networks/results/1d/no2_results.csv', sep = ''))
write.csv(co_df, paste('C:/Users/', username, '/Documents/Data/road_networks/results/1d/co_results.csv', sep = ''))

