# This script performs the econometric analysis for the road networks and pollution project

# Loading libraries

library(plm)
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

# Running Hausman-Taylor IV regressions

ccm_mod_pm <- plm(PM2.5 ~ closeness_centrality_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - closeness_centrality_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
dac_mod_pm <- plm(PM2.5 ~ degree_assortativity_coefficient + factor(Locations) + factor(Week) + factor(Weekday) | . - degree_assortativity_coefficient + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
dcm_mod_pm <- plm(PM2.5 ~ degree_centrality_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - degree_centrality_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
dm_mod_pm <- plm(PM2.5 ~ degree_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - degree_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
density_mod_pm <- plm(PM2.5 ~ density + factor(Locations) + factor(Week) + factor(Weekday) | . - density + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
dsp_mod_pm <- plm(PM2.5 ~ dominating_set_proportion + factor(Locations) + factor(Week) + factor(Weekday) | . - dominating_set_proportion  + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
em_mod_pm <- plm(PM2.5 ~ eccentricity_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - eccentricity_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
ebcm_mod_pm <- plm(PM2.5 ~ edge_betweenness_centrality_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - edge_betweenness_centrality_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
esm_mod_pm <- plm(PM2.5 ~ effective_size_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - effective_size_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
hcm_mod_pm <- plm(PM2.5 ~ harmonic_centrality_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - harmonic_centrality_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
idcm_mod_pm <- plm(PM2.5 ~ in_degree_centrality_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - in_degree_centrality_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
lcm_mod_pm <- plm(PM2.5 ~ load_centrality_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - load_centrality_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
odcm_mod_pm <- plm(PM2.5 ~ out_degree_centrality_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - out_degree_centrality_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
pm_mod_pm <- plm(PM2.5 ~ pagerank_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - pagerank_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
pcm_mod_pm <- plm(PM2.5 ~ percolation_centrality_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - percolation_centrality_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
reciprocity_mod_pm <- plm(PM2.5 ~ reciprocity + factor(Locations) + factor(Week) + factor(Weekday) | . - reciprocity + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
spm_mod_pm <- plm(PM2.5 ~ shortest_path_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - shortest_path_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
elm_mod_pm <- plm(PM2.5 ~ edge_length_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - edge_length_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
spnm_mod_pm <- plm(PM2.5 ~ streets_per_node_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - streets_per_node_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
slm_mod_pm <- plm(PM2.5 ~ street_length_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - street_length_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
circuity_mod_pm <- plm(PM2.5 ~ circuity + factor(Locations) + factor(Week) + factor(Weekday) | . - circuity + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
slp_mod_pm <- plm(PM2.5 ~ self_loop_proportion + factor(Locations) + factor(Week) + factor(Weekday) | . - self_loop_proportion + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
ndkm_mod_pm <- plm(PM2.5 ~ node_density_km + factor(Locations) + factor(Week) + factor(Weekday) | . - node_density_km + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
idkm_mod_pm <- plm(PM2.5 ~ intersection_density_km + factor(Locations) + factor(Week) + factor(Weekday) | . - intersection_density_km + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
edkm_mod_pm <- plm(PM2.5 ~ edge_density_km + factor(Locations) + factor(Week) + factor(Weekday) | . - edge_density_km + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
sdkm_mod_pm <- plm(PM2.5 ~ street_density_km + factor(Locations) + factor(Week) + factor(Weekday) | . - street_density_km + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
pct4_mod_pm <- plm(PM2.5 ~ pct_4way_intersections + factor(Locations) + factor(Week) + factor(Weekday) | . - pct_4way_intersections + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
pct3_mod_pm <- plm(PM2.5 ~ pct_3way_intersections + factor(Locations) + factor(Week) + factor(Weekday) | . - pct_3way_intersections + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')

ccm_mod_no2 <- plm(NO2 ~ closeness_centrality_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - closeness_centrality_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
dac_mod_no2 <- plm(NO2 ~ degree_assortativity_coefficient + factor(Locations) + factor(Week) + factor(Weekday) | . - degree_assortativity_coefficient + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
dcm_mod_no2 <- plm(NO2 ~ degree_centrality_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - degree_centrality_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
dm_mod_no2 <- plm(NO2 ~ degree_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - degree_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
density_mod_no2 <- plm(NO2 ~ density + factor(Locations) + factor(Week) + factor(Weekday) | . - density + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
dsp_mod_no2 <- plm(NO2 ~ dominating_set_proportion + factor(Locations) + factor(Week) + factor(Weekday) | . - dominating_set_proportion  + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
em_mod_no2 <- plm(NO2 ~ eccentricity_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - eccentricity_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
ebcm_mod_no2 <- plm(NO2 ~ edge_betweenness_centrality_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - edge_betweenness_centrality_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
esm_mod_no2 <- plm(NO2 ~ effective_size_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - effective_size_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
hcm_mod_no2 <- plm(NO2 ~ harmonic_centrality_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - harmonic_centrality_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
idcm_mod_no2 <- plm(NO2 ~ in_degree_centrality_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - in_degree_centrality_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
lcm_mod_no2 <- plm(NO2 ~ load_centrality_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - load_centrality_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
odcm_mod_no2 <- plm(NO2 ~ out_degree_centrality_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - out_degree_centrality_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
pm_mod_no2 <- plm(NO2 ~ pagerank_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - pagerank_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
pcm_mod_no2 <- plm(NO2 ~ percolation_centrality_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - percolation_centrality_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
reciprocity_mod_no2 <- plm(NO2 ~ reciprocity + factor(Locations) + factor(Week) + factor(Weekday) | . - reciprocity + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
spm_mod_no2 <- plm(NO2 ~ shortest_path_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - shortest_path_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
elm_mod_no2 <- plm(NO2 ~ edge_length_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - edge_length_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
spnm_mod_no2 <- plm(NO2 ~ streets_per_node_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - streets_per_node_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
slm_mod_no2 <- plm(NO2 ~ street_length_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - street_length_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
circuity_mod_no2 <- plm(NO2 ~ circuity + factor(Locations) + factor(Week) + factor(Weekday) | . - circuity + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
slp_mod_no2 <- plm(NO2 ~ self_loop_proportion + factor(Locations) + factor(Week) + factor(Weekday) | . - self_loop_proportion + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
ndkm_mod_no2 <- plm(NO2 ~ node_density_km + factor(Locations) + factor(Week) + factor(Weekday) | . - node_density_km + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
idkm_mod_no2 <- plm(NO2 ~ intersection_density_km + factor(Locations) + factor(Week) + factor(Weekday) | . - intersection_density_km + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
edkm_mod_no2 <- plm(NO2 ~ edge_density_km + factor(Locations) + factor(Week) + factor(Weekday) | . - edge_density_km + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
sdkm_mod_no2 <- plm(NO2 ~ street_density_km + factor(Locations) + factor(Week) + factor(Weekday) | . - street_density_km + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
pct4_mod_no2 <- plm(NO2 ~ pct_4way_intersections + factor(Locations) + factor(Week) + factor(Weekday) | . - pct_4way_intersections + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
pct3_mod_no2 <- plm(NO2 ~ pct_3way_intersections + factor(Locations) + factor(Week) + factor(Weekday) | . - pct_3way_intersections + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')

ccm_mod_co <- plm(CO ~ closeness_centrality_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - closeness_centrality_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
dac_mod_co <- plm(CO ~ degree_assortativity_coefficient + factor(Locations) + factor(Week) + factor(Weekday) | . - degree_assortativity_coefficient + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
dcm_mod_co <- plm(CO ~ degree_centrality_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - degree_centrality_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
dm_mod_co <- plm(CO ~ degree_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - degree_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
density_mod_co <- plm(CO ~ density + factor(Locations) + factor(Week) + factor(Weekday) | . - density + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
dsp_mod_co <- plm(CO ~ dominating_set_proportion + factor(Locations) + factor(Week) + factor(Weekday) | . - dominating_set_proportion  + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
em_mod_co <- plm(CO ~ eccentricity_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - eccentricity_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
ebcm_mod_co <- plm(CO ~ edge_betweenness_centrality_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - edge_betweenness_centrality_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
esm_mod_co <- plm(CO ~ effective_size_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - effective_size_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
hcm_mod_co <- plm(CO ~ harmonic_centrality_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - harmonic_centrality_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
idcm_mod_co <- plm(CO ~ in_degree_centrality_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - in_degree_centrality_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
lcm_mod_co <- plm(CO ~ load_centrality_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - load_centrality_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
odcm_mod_co <- plm(CO ~ out_degree_centrality_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - out_degree_centrality_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
pm_mod_co <- plm(CO ~ pagerank_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - pagerank_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
pcm_mod_co <- plm(CO ~ percolation_centrality_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - percolation_centrality_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
reciprocity_mod_co <- plm(CO ~ reciprocity + factor(Locations) + factor(Week) + factor(Weekday) | . - reciprocity + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
spm_mod_co <- plm(CO ~ shortest_path_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - shortest_path_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
elm_mod_co <- plm(CO ~ edge_length_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - edge_length_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
spnm_mod_co <- plm(CO ~ streets_per_node_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - streets_per_node_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
slm_mod_co <- plm(CO ~ street_length_mean + factor(Locations) + factor(Week) + factor(Weekday) | . - street_length_mean + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
circuity_mod_co <- plm(CO ~ circuity + factor(Locations) + factor(Week) + factor(Weekday) | . - circuity + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
slp_mod_co <- plm(CO ~ self_loop_proportion + factor(Locations) + factor(Week) + factor(Weekday) | . - self_loop_proportion + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
ndkm_mod_co <- plm(CO ~ node_density_km + factor(Locations) + factor(Week) + factor(Weekday) | . - node_density_km + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
idkm_mod_co <- plm(CO ~ intersection_density_km + factor(Locations) + factor(Week) + factor(Weekday) | . - intersection_density_km + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
edkm_mod_co <- plm(CO ~ edge_density_km + factor(Locations) + factor(Week) + factor(Weekday) | . - edge_density_km + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
sdkm_mod_co <- plm(CO ~ street_density_km + factor(Locations) + factor(Week) + factor(Weekday) | . - street_density_km + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
pct4_mod_co <- plm(CO ~ pct_4way_intersections + factor(Locations) + factor(Week) + factor(Weekday) | . - pct_4way_intersections + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')
pct3_mod_co <- plm(CO ~ pct_3way_intersections + factor(Locations) + factor(Week) + factor(Weekday) | . - pct_3way_intersections + factor(Locations) + IV2 + factor(Week) + factor(Weekday), data = regdata, random.method = 'ht', model = 'random')

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

write.csv(pm_df, paste('C:/Users/', username, '/Documents/Data/road_networks/results/ht/pm_results.csv', sep = ''))
write.csv(no2_df, paste('C:/Users/', username, '/Documents/Data/road_networks/results/ht/no2_results.csv', sep = ''))
write.csv(co_df, paste('C:/Users/', username, '/Documents/Data/road_networks/results/ht/co_results.csv', sep = ''))

# Check the validity of the instrument

F_tests <- c()

check_iv <- lm(closeness_centrality_mean ~ factor(Week) + factor(Weekday) + IV2, data = regdata)
check_lm <- lm(closeness_centrality_mean ~ factor(Week) + factor(Weekday), data = regdata)
F_tests <- c(F_tests, anova(check_iv,check_lm)[5]$F[2])

check_iv <- lm(degree_assortativity_coefficient ~ factor(Week) + factor(Weekday) + IV2, data = regdata)
check_lm <- lm(degree_assortativity_coefficient ~ factor(Week) + factor(Weekday), data = regdata)
F_tests <- c(F_tests, anova(check_iv,check_lm)[5]$F[2])

check_iv <- lm(degree_centrality_mean ~ factor(Week) + factor(Weekday) + IV2, data = regdata)
check_lm <- lm(degree_centrality_mean ~ factor(Week) + factor(Weekday), data = regdata)
F_tests <- c(F_tests, anova(check_iv,check_lm)[5]$F[2])

check_iv <- lm(degree_mean ~ factor(Week) + factor(Weekday) + IV2, data = regdata)
check_lm <- lm(degree_mean ~ factor(Week) + factor(Weekday), data = regdata)
F_tests <- c(F_tests, anova(check_iv,check_lm)[5]$F[2])

check_iv <- lm(density ~ factor(Week) + factor(Weekday) + IV2, data = regdata)
check_lm <- lm(density ~ factor(Week) + factor(Weekday), data = regdata)
F_tests <- c(F_tests, anova(check_iv,check_lm)[5]$F[2])

check_iv <- lm(dominating_set_proportion ~ factor(Week) + factor(Weekday) + IV2, data = regdata)
check_lm <- lm(dominating_set_proportion ~ factor(Week) + factor(Weekday), data = regdata)
F_tests <- c(F_tests, anova(check_iv,check_lm)[5]$F[2])

check_iv <- lm(eccentricity_mean ~ factor(Week) + factor(Weekday) + IV2, data = regdata)
check_lm <- lm(eccentricity_mean ~ factor(Week) + factor(Weekday), data = regdata)
F_tests <- c(F_tests, anova(check_iv,check_lm)[5]$F[2])

check_iv <- lm(edge_betweenness_centrality_mean ~ factor(Week) + factor(Weekday) + IV2, data = regdata)
check_lm <- lm(edge_betweenness_centrality_mean ~ factor(Week) + factor(Weekday), data = regdata)
F_tests <- c(F_tests, anova(check_iv,check_lm)[5]$F[2])

check_iv <- lm(effective_size_mean ~ factor(Week) + factor(Weekday) + IV2, data = regdata)
check_lm <- lm(effective_size_mean ~ factor(Week) + factor(Weekday), data = regdata)
F_tests <- c(F_tests, anova(check_iv,check_lm)[5]$F[2])

check_iv <- lm(harmonic_centrality_mean ~ factor(Week) + factor(Weekday) + IV2, data = regdata)
check_lm <- lm(harmonic_centrality_mean ~ factor(Week) + factor(Weekday), data = regdata)
F_tests <- c(F_tests, anova(check_iv,check_lm)[5]$F[2])

check_iv <- lm(in_degree_centrality_mean ~ factor(Week) + factor(Weekday) + IV2, data = regdata)
check_lm <- lm(in_degree_centrality_mean ~ factor(Week) + factor(Weekday), data = regdata)
F_tests <- c(F_tests, anova(check_iv,check_lm)[5]$F[2])

check_iv <- lm(load_centrality_mean ~ factor(Week) + factor(Weekday) + IV2, data = regdata)
check_lm <- lm(load_centrality_mean ~ factor(Week) + factor(Weekday), data = regdata)
F_tests <- c(F_tests, anova(check_iv,check_lm)[5]$F[2])

check_iv <- lm(out_degree_centrality_mean ~ factor(Week) + factor(Weekday) + IV2, data = regdata)
check_lm <- lm(out_degree_centrality_mean ~ factor(Week) + factor(Weekday), data = regdata)
F_tests <- c(F_tests, anova(check_iv,check_lm)[5]$F[2])

check_iv <- lm(pagerank_mean ~ factor(Week) + factor(Weekday) + IV2, data = regdata)
check_lm <- lm(pagerank_mean ~ factor(Week) + factor(Weekday), data = regdata)
F_tests <- c(F_tests, anova(check_iv,check_lm)[5]$F[2])

check_iv <- lm(percolation_centrality_mean ~ factor(Week) + factor(Weekday) + IV2, data = regdata)
check_lm <- lm(percolation_centrality_mean ~ factor(Week) + factor(Weekday), data = regdata)
F_tests <- c(F_tests, anova(check_iv,check_lm)[5]$F[2])

check_iv <- lm(reciprocity ~ factor(Week) + factor(Weekday) + IV2, data = regdata)
check_lm <- lm(reciprocity ~ factor(Week) + factor(Weekday), data = regdata)
F_tests <- c(F_tests, anova(check_iv,check_lm)[5]$F[2])

check_iv <- lm(shortest_path_mean ~ factor(Week) + factor(Weekday) + IV2, data = regdata)
check_lm <- lm(shortest_path_mean ~ factor(Week) + factor(Weekday), data = regdata)
F_tests <- c(F_tests, anova(check_iv,check_lm)[5]$F[2])

check_iv <- lm(edge_length_mean ~ factor(Week) + factor(Weekday) + IV2, data = regdata)
check_lm <- lm(edge_length_mean ~ factor(Week) + factor(Weekday), data = regdata)
F_tests <- c(F_tests, anova(check_iv,check_lm)[5]$F[2])

check_iv <- lm(streets_per_node_mean ~ factor(Week) + factor(Weekday) + IV2, data = regdata)
check_lm <- lm(streets_per_node_mean ~ factor(Week) + factor(Weekday), data = regdata)
F_tests <- c(F_tests, anova(check_iv,check_lm)[5]$F[2])

check_iv <- lm(street_length_mean ~ factor(Week) + factor(Weekday) + IV2, data = regdata)
check_lm <- lm(street_length_mean ~ factor(Week) + factor(Weekday), data = regdata)
F_tests <- c(F_tests, anova(check_iv,check_lm)[5]$F[2])

check_iv <- lm(circuity ~ factor(Week) + factor(Weekday) + IV2, data = regdata)
check_lm <- lm(circuity ~ factor(Week) + factor(Weekday), data = regdata)
F_tests <- c(F_tests, anova(check_iv,check_lm)[5]$F[2])

check_iv <- lm(self_loop_proportion ~ factor(Week) + factor(Weekday) + IV2, data = regdata)
check_lm <- lm(self_loop_proportion ~ factor(Week) + factor(Weekday), data = regdata)
F_tests <- c(F_tests, anova(check_iv,check_lm)[5]$F[2])

check_iv <- lm(node_density_km ~ factor(Week) + factor(Weekday) + IV2, data = regdata)
check_lm <- lm(node_density_km ~ factor(Week) + factor(Weekday), data = regdata)
F_tests <- c(F_tests, anova(check_iv,check_lm)[5]$F[2])

check_iv <- lm(intersection_density_km ~ factor(Week) + factor(Weekday) + IV2, data = regdata)
check_lm <- lm(intersection_density_km ~ factor(Week) + factor(Weekday), data = regdata)
F_tests <- c(F_tests, anova(check_iv,check_lm)[5]$F[2])

check_iv <- lm(edge_density_km ~ factor(Week) + factor(Weekday) + IV2, data = regdata)
check_lm <- lm(edge_density_km ~ factor(Week) + factor(Weekday), data = regdata)
F_tests <- c(F_tests, anova(check_iv,check_lm)[5]$F[2])

check_iv <- lm(street_density_km ~ factor(Week) + factor(Weekday) + IV2, data = regdata)
check_lm <- lm(street_density_km ~ factor(Week) + factor(Weekday), data = regdata)
F_tests <- c(F_tests, anova(check_iv,check_lm)[5]$F[2])

check_iv <- lm(pct_4way_intersections ~ factor(Week) + factor(Weekday) + IV2, data = regdata)
check_lm <- lm(pct_4way_intersections ~ factor(Week) + factor(Weekday), data = regdata)
F_tests <- c(F_tests, anova(check_iv,check_lm)[5]$F[2])

check_iv <- lm(pct_3way_intersections ~ factor(Week) + factor(Weekday) + IV2, data = regdata)
check_lm <- lm(pct_3way_intersections ~ factor(Week) + factor(Weekday), data = regdata)
F_tests <- c(F_tests, anova(check_iv,check_lm)[5]$F[2])

F_tests <- data.frame(F_tests)
write.csv(F_tests, paste('C:/Users/', username, '/Documents/Data/road_networks/results/F_stats.csv', sep = ''))

# Cute sum stats table example

keepers <- c('closeness_centrality_mean', 'degree_assortativity_coefficient',
             'degree_centrality_mean', 'degree_mean', 'density', 'dominating_set_proportion',
             'eccentricity_mean', 'edge_betweenness_centrality_mean', 'effective_size_mean',
             'harmonic_centrality_mean', 'in_degree_centrality_mean', 'load_centrality_mean',
             'out_degree_centrality_mean', 'pagerank_mean', 'percolation_centrality_mean',
             'reciprocity', 'shortest_path_mean', 'edge_length_mean', 'streets_per_node_mean',
             'street_length_mean', 'circuity', 'self_loop_proportion', 'node_density_km',
             'intersection_density_km', 'edge_density_km', 'street_density_km',
             'pct_4way_intersections', 'pct_3way_intersections')

sumdata <- regdata[,names(regdata) %in% keepers]
sumdata <- cbind(regdata[,names(regdata) %in% c('PM2.5', 'NO2', 'CO')], sumdata)

new_names <- c('Particulate Matter (PM2.5)', 'Nitrogen Dioxide (NO2)', 'Carbon Monoxide (CO)',
               'Closeness Centrality (Mean)', 'Degree Assortativity Coefficient',
               'Degree Centrality (Mean)', 'Degree (Mean)', 'Density', 'Dominating Set Proportion',
               'Eccentricity (Mean)', 'Edge Betweenness Centrality (Mean)', 'Effective Size (Mean)',
               'Harmonic Centrality (Mean)', 'In Degree Centrality (Mean)', 'Load Centrality (Mean)',
               'Out Degree Centrality (Mean)', 'Pagerank (Mean)', 'Percolation Centrality (Mean)',
               'Reciprocity', 'Shortest Path (Mean)', 'Edge Length (Mean)', 'Streets per Node (Mean)',
               'Street Length (Mean)', 'Circuity', 'Self Loop Proportion', 'Node Density per km',
               'Intersection Density per km', 'Edge Density per km', 'Street Density per km',
               'Percent 4-way Intersections', 'Percent 3-way Intersections')

names(sumdata) <- new_names
datasummary_skim(sumdata, fmt = '%.3f')

