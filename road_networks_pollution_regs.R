# Main econometrics script for a paper on the impact of road network topology on pollution

# Running the individual scripts for each set of analyses -- update username & ensure that the directory is correct

username = ''
diRectoRy <- paste('C:/Users/', username, '/Documents/', sep = '')

# Running the R scripts

source(paste(diRectoRy, 'road_networks_pollution_regs_ht.R', sep = ''))
source(paste(diRectoRy, 'road_networks_pollution_mechanisms.R', sep = ''))

