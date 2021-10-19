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

regdata$PM2.5 <- pm_diffs
regdata$NO2 <- no2_diffs
regdata$CO <- co_diffs

# Running first differenced IV regressions

ccm_mod_pm <- ivreg(PM2.5 ~ closeness_centrality_mean + factor(Week) + factor(Weekday) | . - closeness_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
dac_mod_pm <- ivreg(PM2.5 ~ degree_assortativity_coefficient + factor(Week) + factor(Weekday) | . - degree_assortativity_coefficient + IV_Count + factor(Week) + factor(Weekday), data = regdata)
dcm_mod_pm <- ivreg(PM2.5 ~ degree_centrality_mean + factor(Week) + factor(Weekday) | . - degree_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
dm_mod_pm <- ivreg(PM2.5 ~ degree_mean + factor(Week) + factor(Weekday) | . - degree_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
density_mod_pm <- ivreg(PM2.5 ~ density + factor(Week) + factor(Weekday) | . - density + IV_Count + factor(Week) + factor(Weekday), data = regdata)
dsp_mod_pm <- ivreg(PM2.5 ~ dominating_set_proportion + factor(Week) + factor(Weekday) | . - dominating_set_proportion  + IV_Count + factor(Week) + factor(Weekday), data = regdata)
em_mod_pm <- ivreg(PM2.5 ~ eccentricity_mean + factor(Week) + factor(Weekday) | . - eccentricity_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
ebcm_mod_pm <- ivreg(PM2.5 ~ edge_betweenness_centrality_mean + factor(Week) + factor(Weekday) | . - edge_betweenness_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
esm_mod_pm <- ivreg(PM2.5 ~ effective_size_mean + factor(Week) + factor(Weekday) | . - effective_size_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
hcm_mod_pm <- ivreg(PM2.5 ~ harmonic_centrality_mean + factor(Week) + factor(Weekday) | . - harmonic_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
idcm_mod_pm <- ivreg(PM2.5 ~ in_degree_centrality_mean + factor(Week) + factor(Weekday) | . - in_degree_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
lcm_mod_pm <- ivreg(PM2.5 ~ load_centrality_mean + factor(Week) + factor(Weekday) | . - load_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
odcm_mod_pm <- ivreg(PM2.5 ~ out_degree_centrality_mean + factor(Week) + factor(Weekday) | . - out_degree_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
pm_mod_pm <- ivreg(PM2.5 ~ pagerank_mean + factor(Week) + factor(Weekday) | . - pagerank_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
pcm_mod_pm <- ivreg(PM2.5 ~ percolation_centrality_mean + factor(Week) + factor(Weekday) | . - percolation_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
reciprocity_mod_pm <- ivreg(PM2.5 ~ reciprocity + factor(Week) + factor(Weekday) | . - reciprocity + IV_Count + factor(Week) + factor(Weekday), data = regdata)
spm_mod_pm <- ivreg(PM2.5 ~ shortest_path_mean + factor(Week) + factor(Weekday) | . - shortest_path_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
elm_mod_pm <- ivreg(PM2.5 ~ edge_length_mean + factor(Week) + factor(Weekday) | . - edge_length_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
spnm_mod_pm <- ivreg(PM2.5 ~ streets_per_node_mean + factor(Week) + factor(Weekday) | . - streets_per_node_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
slm_mod_pm <- ivreg(PM2.5 ~ street_length_mean + factor(Week) + factor(Weekday) | . - street_length_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
circuity_mod_pm <- ivreg(PM2.5 ~ circuity + factor(Week) + factor(Weekday) | . - circuity + IV_Count + factor(Week) + factor(Weekday), data = regdata)
slp_mod_pm <- ivreg(PM2.5 ~ self_loop_proportion + factor(Week) + factor(Weekday) | . - self_loop_proportion + IV_Count + factor(Week) + factor(Weekday), data = regdata)
ndkm_mod_pm <- ivreg(PM2.5 ~ node_density_km + factor(Week) + factor(Weekday) | . - node_density_km + IV_Count + factor(Week) + factor(Weekday), data = regdata)
idkm_mod_pm <- ivreg(PM2.5 ~ intersection_density_km + factor(Week) + factor(Weekday) | . - intersection_density_km + IV_Count + factor(Week) + factor(Weekday), data = regdata)
edkm_mod_pm <- ivreg(PM2.5 ~ edge_density_km + factor(Week) + factor(Weekday) | . - edge_density_km + IV_Count + factor(Week) + factor(Weekday), data = regdata)
sdkm_mod_pm <- ivreg(PM2.5 ~ street_density_km + factor(Week) + factor(Weekday) | . - street_density_km + IV_Count + factor(Week) + factor(Weekday), data = regdata)
pct4_mod_pm <- ivreg(PM2.5 ~ pct_4way_intersections + factor(Week) + factor(Weekday) | . - pct_4way_intersections + IV_Count + factor(Week) + factor(Weekday), data = regdata)
pct3_mod_pm <- ivreg(PM2.5 ~ pct_3way_intersections + factor(Week) + factor(Weekday) | . - pct_3way_intersections + IV_Count + factor(Week) + factor(Weekday), data = regdata)

ccm_mod_no2 <- ivreg(NO2 ~ closeness_centrality_mean + factor(Week) + factor(Weekday) | . - closeness_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
dac_mod_no2 <- ivreg(NO2 ~ degree_assortativity_coefficient + factor(Week) + factor(Weekday) | . - degree_assortativity_coefficient + IV_Count + factor(Week) + factor(Weekday), data = regdata)
dcm_mod_no2 <- ivreg(NO2 ~ degree_centrality_mean + factor(Week) + factor(Weekday) | . - degree_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
dm_mod_no2 <- ivreg(NO2 ~ degree_mean + factor(Week) + factor(Weekday) | . - degree_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
density_mod_no2 <- ivreg(NO2 ~ density + factor(Week) + factor(Weekday) | . - density + IV_Count + factor(Week) + factor(Weekday), data = regdata)
dsp_mod_no2 <- ivreg(NO2 ~ dominating_set_proportion + factor(Week) + factor(Weekday) | . - dominating_set_proportion  + IV_Count + factor(Week) + factor(Weekday), data = regdata)
em_mod_no2 <- ivreg(NO2 ~ eccentricity_mean + factor(Week) + factor(Weekday) | . - eccentricity_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
ebcm_mod_no2 <- ivreg(NO2 ~ edge_betweenness_centrality_mean + factor(Week) + factor(Weekday) | . - edge_betweenness_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
esm_mod_no2 <- ivreg(NO2 ~ effective_size_mean + factor(Week) + factor(Weekday) | . - effective_size_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
hcm_mod_no2 <- ivreg(NO2 ~ harmonic_centrality_mean + factor(Week) + factor(Weekday) | . - harmonic_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
idcm_mod_no2 <- ivreg(NO2 ~ in_degree_centrality_mean + factor(Week) + factor(Weekday) | . - in_degree_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
lcm_mod_no2 <- ivreg(NO2 ~ load_centrality_mean + factor(Week) + factor(Weekday) | . - load_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
odcm_mod_no2 <- ivreg(NO2 ~ out_degree_centrality_mean + factor(Week) + factor(Weekday) | . - out_degree_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
pm_mod_no2 <- ivreg(NO2 ~ pagerank_mean + factor(Week) + factor(Weekday) | . - pagerank_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
pcm_mod_no2 <- ivreg(NO2 ~ percolation_centrality_mean + factor(Week) + factor(Weekday) | . - percolation_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
reciprocity_mod_no2 <- ivreg(NO2 ~ reciprocity + factor(Week) + factor(Weekday) | . - reciprocity + IV_Count + factor(Week) + factor(Weekday), data = regdata)
spm_mod_no2 <- ivreg(NO2 ~ shortest_path_mean + factor(Week) + factor(Weekday) | . - shortest_path_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
elm_mod_no2 <- ivreg(NO2 ~ edge_length_mean + factor(Week) + factor(Weekday) | . - edge_length_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
spnm_mod_no2 <- ivreg(NO2 ~ streets_per_node_mean + factor(Week) + factor(Weekday) | . - streets_per_node_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
slm_mod_no2 <- ivreg(NO2 ~ street_length_mean + factor(Week) + factor(Weekday) | . - street_length_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
circuity_mod_no2 <- ivreg(NO2 ~ circuity + factor(Week) + factor(Weekday) | . - circuity + IV_Count + factor(Week) + factor(Weekday), data = regdata)
slp_mod_no2 <- ivreg(NO2 ~ self_loop_proportion + factor(Week) + factor(Weekday) | . - self_loop_proportion + IV_Count + factor(Week) + factor(Weekday), data = regdata)
ndkm_mod_no2 <- ivreg(NO2 ~ node_density_km + factor(Week) + factor(Weekday) | . - node_density_km + IV_Count + factor(Week) + factor(Weekday), data = regdata)
idkm_mod_no2 <- ivreg(NO2 ~ intersection_density_km + factor(Week) + factor(Weekday) | . - intersection_density_km + IV_Count + factor(Week) + factor(Weekday), data = regdata)
edkm_mod_no2 <- ivreg(NO2 ~ edge_density_km + factor(Week) + factor(Weekday) | . - edge_density_km + IV_Count + factor(Week) + factor(Weekday), data = regdata)
sdkm_mod_no2 <- ivreg(NO2 ~ street_density_km + factor(Week) + factor(Weekday) | . - street_density_km + IV_Count + factor(Week) + factor(Weekday), data = regdata)
pct4_mod_no2 <- ivreg(NO2 ~ pct_4way_intersections + factor(Week) + factor(Weekday) | . - pct_4way_intersections + IV_Count + factor(Week) + factor(Weekday), data = regdata)
pct3_mod_no2 <- ivreg(NO2 ~ pct_3way_intersections + factor(Week) + factor(Weekday) | . - pct_3way_intersections + IV_Count + factor(Week) + factor(Weekday), data = regdata)

ccm_mod_co <- ivreg(CO ~ closeness_centrality_mean + factor(Week) + factor(Weekday) | . - closeness_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
dac_mod_co <- ivreg(CO ~ degree_assortativity_coefficient + factor(Week) + factor(Weekday) | . - degree_assortativity_coefficient + IV_Count + factor(Week) + factor(Weekday), data = regdata)
dcm_mod_co <- ivreg(CO ~ degree_centrality_mean + factor(Week) + factor(Weekday) | . - degree_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
dm_mod_co <- ivreg(CO ~ degree_mean + factor(Week) + factor(Weekday) | . - degree_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
density_mod_co <- ivreg(CO ~ density + factor(Week) + factor(Weekday) | . - density + IV_Count + factor(Week) + factor(Weekday), data = regdata)
dsp_mod_co <- ivreg(CO ~ dominating_set_proportion + factor(Week) + factor(Weekday) | . - dominating_set_proportion  + IV_Count + factor(Week) + factor(Weekday), data = regdata)
em_mod_co <- ivreg(CO ~ eccentricity_mean + factor(Week) + factor(Weekday) | . - eccentricity_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
ebcm_mod_co <- ivreg(CO ~ edge_betweenness_centrality_mean + factor(Week) + factor(Weekday) | . - edge_betweenness_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
esm_mod_co <- ivreg(CO ~ effective_size_mean + factor(Week) + factor(Weekday) | . - effective_size_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
hcm_mod_co <- ivreg(CO ~ harmonic_centrality_mean + factor(Week) + factor(Weekday) | . - harmonic_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
idcm_mod_co <- ivreg(CO ~ in_degree_centrality_mean + factor(Week) + factor(Weekday) | . - in_degree_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
lcm_mod_co <- ivreg(CO ~ load_centrality_mean + factor(Week) + factor(Weekday) | . - load_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
odcm_mod_co <- ivreg(CO ~ out_degree_centrality_mean + factor(Week) + factor(Weekday) | . - out_degree_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
pm_mod_co <- ivreg(CO ~ pagerank_mean + factor(Week) + factor(Weekday) | . - pagerank_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
pcm_mod_co <- ivreg(CO ~ percolation_centrality_mean + factor(Week) + factor(Weekday) | . - percolation_centrality_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
reciprocity_mod_co <- ivreg(CO ~ reciprocity + factor(Week) + factor(Weekday) | . - reciprocity + IV_Count + factor(Week) + factor(Weekday), data = regdata)
spm_mod_co <- ivreg(CO ~ shortest_path_mean + factor(Week) + factor(Weekday) | . - shortest_path_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
elm_mod_co <- ivreg(CO ~ edge_length_mean + factor(Week) + factor(Weekday) | . - edge_length_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
spnm_mod_co <- ivreg(CO ~ streets_per_node_mean + factor(Week) + factor(Weekday) | . - streets_per_node_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
slm_mod_co <- ivreg(CO ~ street_length_mean + factor(Week) + factor(Weekday) | . - street_length_mean + IV_Count + factor(Week) + factor(Weekday), data = regdata)
circuity_mod_co <- ivreg(CO ~ circuity + factor(Week) + factor(Weekday) | . - circuity + IV_Count + factor(Week) + factor(Weekday), data = regdata)
slp_mod_co <- ivreg(CO ~ self_loop_proportion + factor(Week) + factor(Weekday) | . - self_loop_proportion + IV_Count + factor(Week) + factor(Weekday), data = regdata)
ndkm_mod_co <- ivreg(CO ~ node_density_km + factor(Week) + factor(Weekday) | . - node_density_km + IV_Count + factor(Week) + factor(Weekday), data = regdata)
idkm_mod_co <- ivreg(CO ~ intersection_density_km + factor(Week) + factor(Weekday) | . - intersection_density_km + IV_Count + factor(Week) + factor(Weekday), data = regdata)
edkm_mod_co <- ivreg(CO ~ edge_density_km + factor(Week) + factor(Weekday) | . - edge_density_km + IV_Count + factor(Week) + factor(Weekday), data = regdata)
sdkm_mod_co <- ivreg(CO ~ street_density_km + factor(Week) + factor(Weekday) | . - street_density_km + IV_Count + factor(Week) + factor(Weekday), data = regdata)
pct4_mod_co <- ivreg(CO ~ pct_4way_intersections + factor(Week) + factor(Weekday) | . - pct_4way_intersections + IV_Count + factor(Week) + factor(Weekday), data = regdata)
pct3_mod_co <- ivreg(CO ~ pct_3way_intersections + factor(Week) + factor(Weekday) | . - pct_3way_intersections + IV_Count + factor(Week) + factor(Weekday), data = regdata)

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

write.csv(pm_df, paste('C:/Users/', username, '/Documents/Data/road_networks/results/iv/pm_results.csv', sep = ''))
write.csv(no2_df, paste('C:/Users/', username, '/Documents/Data/road_networks/results/iv/no2_results.csv', sep = ''))
write.csv(co_df, paste('C:/Users/', username, '/Documents/Data/road_networks/results/iv/co_results.csv', sep = ''))

