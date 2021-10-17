# This script creates the final data set for the road networks and pollution project

# Importing required modules

import pandas as pd

# Defining filepaths

username = ''
filepath = 'C:/Users/' + username + '/Documents/Data/road_networks/data/'

# Reading in the input files

iv = pd.read_csv(filepath + 'iv.csv')
rn = pd.read_csv(filepath + 'road_network_statistics.csv')
poll = pd.read_csv(filepath + 'va_pollution_data.csv')

# Creating the main df

dates = list(iv.Date.unique())
locations = list(poll.Location.unique())

time = []
co = []
no2 = []
o3 = []
pm = []
pm10 = []

fn = pd.DataFrame()

iv_bin = []
iv_count = []
iv_la = []

for d in dates:
    
    tmp = poll[poll.Date == d]
    co_tmp = tmp[tmp.Pollutant == 'CO']
    no2_tmp = tmp[tmp.Pollutant == 'NO2']
    o3_tmp = tmp[tmp.Pollutant == 'O3']
    pm_tmp = tmp[tmp.Pollutant == 'PM2.5']
    pm10_tmp = tmp[tmp.Pollutant == 'PM10']
    
    time = time + [d]*38
    co = co + co_tmp.Value.to_list()
    no2 = no2 + no2_tmp.Value.to_list()
    o3 = o3 + o3_tmp.Value.to_list()
    pm = pm + pm_tmp.Value.to_list()
    pm10 = pm10 + pm10_tmp.Value.to_list()
    
    fn = pd.concat([fn,rn], axis = 0).reset_index(drop = True)
    
    pirate_noise = list(iv.iloc[dates.index(d)])
    
    iv_bin = iv_bin + pirate_noise[1:39]
    iv_count = iv_count + pirate_noise[39:77]
    iv_la = iv_la + pirate_noise[77:]
    
time = pd.Series(time, name = 'Date')
pm = pd.Series(pm, name = 'PM2.5')
pm10 = pd.Series(pm10, name = 'PM10')
o3 = pd.Series(o3, name = 'O3')
no2 = pd.Series(no2, name = 'NO2')
co = pd.Series(co, name = 'CO')
iv_bin = pd.Series(iv_bin, name = 'IV_Binary')
iv_count = pd.Series(iv_count, name = 'IV_Count')
iv_la = pd.Series(iv_la, name = 'IV_Lanes_Affected')

main_df = pd.concat([time, fn, pm, pm10, o3, no2, co, iv_bin, iv_count, iv_la], axis = 1)

# Saving main df to file

main_df.to_csv(filepath + 'complete_data.csv', index = False)

