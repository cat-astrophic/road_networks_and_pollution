# This script creates the final pollution data set for the road networks and pollution project

# Importing required modules

import pandas as pd

# Defining username + filepaths

username = ''
filepath = 'C:/Users/' + username + '/Documents/Data/road_networks/'

# Reading in the raw pollution data

o3 = pd.read_csv(filepath + 'data/pollution_data/ozone_data.csv')
pm = pd.read_csv(filepath + 'data/pollution_data/pm_data.csv')
pm10 = pd.read_csv(filepath + 'data/pollution_data/pm10_data.csv')
co = pd.read_csv(filepath + 'data/pollution_data/co_data.csv')
no2 = pd.read_csv(filepath + 'data/pollution_data/no2_data.csv')

# Creating a list of all VA FIPS codes and municipalities

fips_list = [51810, 51013, 51059, 51107, 51003, 51069, 51775, 51165, 51710, 51087,
             51680, 51041, 51036, 51520, 51650, 51161, 51760, 51670, 51840, 51035,
             51179, 51153, 51071, 51113, 51147, 51041, 51163, 51033, 51061, 51800,
             51085, 51197, 51510, 51630, 51700, 51770, 51047, 51101, 51139, 51187]


munic_list = ['VirginiaBeach', 'ArlingtonCounty', 'FairfaxCounty', 'LoudounCounty',
              'AlbemarleCounty', 'FrederickCounty', 'Salem', 'RockinghamCounty',
              'Norfolk', 'HenricoCounty', 'Lynchburg', 'ChesterfieldCounty',
              'CharlesCityCounty', 'Bristol', 'Hampton', 'RoanokeCounty', 'Richmond',
              'Hopewell', 'Winchester', 'CarrollCounty', 'StaffordCounty', 'PrinceWilliamCounty',
              'GilesCounty', 'MadisonCounty', 'PrinceEdwardCounty', 'RockbridgeCounty',
              'CarolineCounty', 'FauquierCounty', 'Suffolk', 'HanoverCounty', 'WytheCounty',
              'Alexandria', 'Fredericksburg', 'NewportNews', 'Roanoke', 'CulpeperCounty',
              'KingWilliamCounty', 'PageCounty', 'WarrenCounty']

# Defining the sample

counties = ['AlbemarleCounty', 'ArlingtonCounty', 'CarolineCounty', 'CarrollCounty',
            'CharlesCityCounty', 'ChesterfieldCounty', 'CulpeperCounty', 'FairfaxCounty',
            'FauquierCounty', 'FrederickCounty', 'GilesCounty', 'HanoverCounty',
            'HenricoCounty', 'KingWilliamCounty', 'LoudounCounty', 'MadisonCounty',
            'PageCounty', 'PrinceEdwardCounty', 'PrinceWilliamCounty', 'RoanokeCounty',
            'RockbridgeCounty', 'RockinghamCounty', 'StaffordCounty', 'WarrenCounty', 'WytheCounty']

cities = ['Alexandria', 'Fredericksburg', 'Hampton', 'Hopewell', 'Lynchburg', 'NewportNews',
          'Norfolk', 'Richmond', 'Roanoke', 'Salem', 'Suffolk', 'VirginiaBeach', 'Winchester']

agg_list = counties + cities

# Creating a list of dates

days = [str(x) if x > 9 else '0' + str(x) for x in range(1,32)]
months = [str(x) if x > 9 else '0' + str(x) for x in range(1,13)]
dates = [x + y for x in months for y in days]
drops = ['0230', '0231', '0431', '0631', '0931', '1131']
dates = [d for d in dates if d not in drops]

# Creating pollution data for each pollutant

pollutants = ['Ozone', 'PM2.5', 'PM10', 'NO2', 'CO']
df_dates = []
locations = []
fips = []
polls = []
vals = []

for d in dates:
    
    print(d) # Visualize progress
    
    for p in range(5):
        
        for c in counties + cities:
            
            locations.append(c)
            fips.append(fips_list[munic_list.index(c)])
            polls.append(pollutants[p])
            df_dates.append(d)
            
            if p == 0:
                
                tmp = o3[o3.Date == int('2020' + d)]
                
            elif p == 1:
                
                tmp = pm[pm.Date == int('2020' + d)]
                
            elif p == 2:
                
                tmp = pm10[pm10.Date == int('2020' + d)]
                
            elif p == 3:
                
                tmp = no2[no2.Date == int('2020' + d)]
                
            else:
                
                tmp = co[co.Date == int('2020' + d)]
                
            try:
                
                tmp = tmp[tmp.FIPS == fips_list[munic_list.index(c)]].reset_index(drop = True)
                vp = tmp.Value[0]
                vals.append(vp)
                
            except:
                
                vals.append(None)
                
df_dates = pd.Series(df_dates, name = 'Date')
locations = pd.Series(locations, name = 'Location')
fips = pd.Series(fips, name = 'FIPS')
polls = pd.Series(polls, name = 'Pollutant')
vals = pd.Series(vals, name = 'Value')

poll_df = pd.concat([df_dates, locations, fips, polls, vals], axis = 1)

# Writing poll_df to csv

poll_df.to_csv(filepath + 'data/va_pollution_data.csv', index = False)

