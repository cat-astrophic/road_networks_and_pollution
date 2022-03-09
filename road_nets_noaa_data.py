# This script creates the weather data set for the road networks and pollution project

# Importing required modules

import pandas as pd
from geopy.distance import geodesic
from geopy.geocoders import Nominatim
from matplotlib import pyplot as plt

# Defining username + filepaths

username = ''
filepath = 'C:/Users/' + username + '/Documents/Data/road_networks/'

# Reading in the 2019 NOAA weather data (large file, stored externally)

noaa = pd.read_csv('F:/NOAA/us_data/NOAA_2020.csv')

# Subset NOAA data for Virginia sites

keepers = [1 if str(noaa.NAME[i])[-5:] == 'VA US' else 0 for i in range(len(noaa))]
noaa = pd.concat([noaa, pd.Series(keepers, name = 'Virginia')], axis = 1)
noaa = noaa[noaa.Virginia == 1].reset_index(drop = True)

# For each municipality in the study, find the nearest weather station and use its data

stations = list(noaa.STATION.unique())
s_lats = []
s_lons = []

for s in stations:
    
    tmp = noaa[noaa.STATION == s].reset_index(drop = True)
    s_lats.append(tmp.LATITUDE[0])
    s_lons.append(tmp.LONGITUDE[0])

counties = ['Albemarle County', 'Arlington County', 'Caroline County', 'Carroll County',
            'Charles City County', 'Chesterfield County', 'Culpeper County', 'Fairfax County',
            'Fauquier County', 'Frederick County', 'Giles County', 'Hanover County',
            'Henrico County', 'King William County', 'Loudoun County', 'Madison County',
            'Page County', 'Prince Edward County', 'Prince William County', 'Roanoke County',
            'Rockbridge County', 'Rockingham County', 'Stafford County', 'Warren County', 'Wythe County']

cities = ['Alexandria', 'Fredericksburg', 'Hampton', 'Hopewell', 'Lynchburg', 'Newport News',
          'Norfolk', 'Richmond', 'Roanoke', 'Salem', 'Suffolk', 'Virginia Beach', 'Winchester']

munics = counties + cities
lats = []
lons = []

geolocator = Nominatim(user_agent = 'geoapiExercises')

for m in munics:
    
    location = geolocator.geocode(str(m + ', Virginia'))
    lats.append(location.latitude)
    lons.append(location.longitude)

matches = []

for m in range(len(munics)):
    
    dists = [geodesic([lats[m], lons[m]], [s_lats[i], s_lons[i]]).mi for i in range(len(stations))]
    idx = dists.index(min(dists))
    matches.append(stations[idx])

xlons = []
xlats = []

for m in matches:
    
    tmp = noaa[noaa.STATION == m].reset_index(drop = True)
    xlats.append(tmp.LATITUDE[0])
    xlons.append(tmp.LONGITUDE[0])

# Quickly visualize some spatial data

fig = plt.figure()
plt.scatter(noaa.LONGITUDE,noaa.LATITUDE, color = 'blue')
plt.scatter(lons,lats, color = 'red')
plt.scatter(xlons,xlats, color = 'green')
plt.show()

# Creating a list of dates

days = [str(x) if x > 9 else '0' + str(x) for x in range(1,32)]
months = [str(x) if x > 9 else '0' + str(x) for x in range(1,13)]
dates = [x + y for x in months for y in days]
drops = ['0230', '0231', '0431', '0631', '0931', '1131']
dates = [d for d in dates if d not in drops]

# Creating weather data set

obs = ['TEMP', 'MAX', 'MIN', 'PRCP', 'WDSP', 'DEWP']
df_dates = []
locations = []
weathers = []
vals = []

for d in dates:
    
    print(d) # Visualize progress
    
    for m in munics:
        
        locations = locations + [m,m,m,m,m,m]
        df_dates = df_dates + [d,d,d,d,d,d]
        tmp = noaa[noaa.DATE == '2020-' + d[:2] + '-' + d[2:]].reset_index(drop = True)
        tmp = tmp[tmp.STATION == matches[munics.index(m)]].reset_index(drop = True)
        weathers = weathers + obs
        
        try:
            
            if tmp.TEMP[0] not in [99.99, 999.9, 9999.9]:
                
                vals.append(tmp.TEMP[0])
                
            else:
                
                vals.append(None)
                
            if tmp.MAX[0] not in [99.99, 999.9, 9999.9]:
                
                vals.append(tmp.MAX[0])
                
            else:
                
                vals.append(None)
                
            if tmp.MIN[0] not in [99.99, 999.9, 9999.9]:
                
                vals.append(tmp.MIN[0])
                
            else:
                
                vals.append(None)
                
            if tmp.PRCP[0] not in [99.99, 999.9, 9999.9]:
                
                vals.append(tmp.PRCP[0])
                
            else:
                
                vals.append(None)
                
            if tmp.WDSP[0] not in [99.99, 999.9, 9999.9]:
                
                vals.append(tmp.WDSP[0])
                
            else:
                
                vals.append(None)
                
            if tmp.DEWP[0] not in [99.99, 999.9, 9999.9]:
                
                vals.append(tmp.DEWP[0])
                
            else:
                
                vals.append(None)
                
        except:
            
            vals.append(None)
            vals.append(None)
            vals.append(None)
            vals.append(None)
            vals.append(None)
            vals.append(None)
            
df_dates = pd.Series(df_dates, name = 'Date')
locations = pd.Series(locations, name = 'Location')
weathers = pd.Series(weathers, name = 'Weather')
vals = pd.Series(vals, name = 'Value')

weather_df = pd.concat([df_dates, locations, weathers, vals], axis = 1)

# Writing poll_df to csv

weather_df.to_csv(filepath + 'data/va_weather_data.csv', index = False)

