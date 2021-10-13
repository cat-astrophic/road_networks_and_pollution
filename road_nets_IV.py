# This script creates the instrument for IV for the road networks and pollution project

# Importing required modules

import pandas as pd
from glob import glob
import re

# Defining username and filepaths

username = ''
filepath = 'C:/Users/' + username + '/Documents/Data/road_networks/raw_closures_data/'

# Initializing data structures

df = pd.DataFrame()
dates = []
ids = []
lanes_affected = []
severity = []
coords = []
locations = []
binvals = []
countvals = []

# Defining a list of all files to read

to_read = [x[len(x)-13:len(x)-4] for x in glob(filepath + '*')]

# Main loop

for read_it in to_read:

    # Visualize progress
    
    print(read_it)
    
    # Read in the file
    
    with open(filepath + read_it + '.txt') as f:
       
        text = f.readlines()
        text = text[0]
    
    # Initializing lists for storing data for this file
    
    # Retrieve the data from the file
    
    flag = True
    
    while flag == True:
        
        start_id = text.find('orci:datagateway_oid')
        
        if start_id == -1:
            
            flag = False
            
        else:
            
            dates.append(read_it[:4])
            
            text = text[start_id+23:]
            end1 = text.find('"')
            ids.append(text[:end1])
            
            start_id = text.find('orci:lanes_affected')
            text = text[start_id+22:]
            end1 = text.find('"')
            lanes_affected.append(text[:end1])
            
            start_id = text.find('orci:severity')
            text = text[start_id+16:]
            end1 = text.find('"')
            severity.append(text[:end1])
            
            start_id = text.find('gml:pos')
            text = text[start_id+10:]
            end1 = text.find('"')
            coords.append(text[:end1])
            
            start_id = text.find('orci:route_name')
            text = text[start_id+17:]
            end1 = text.find('(')
            end2 = text.find(')')
            locations.append(text[end1+1:end2])

# Cleaning locations data

counties = ['AlbemarleCounty', 'ArlingtonCounty', 'CarolineCounty', 'CarrollCounty',
            'CharlesCityCounty', 'ChesterfieldCounty', 'CulpeperCounty', 'FairfaxCounty',
            'FauquierCounty', 'FrederickCounty', 'GilesCounty', 'HanoverCounty',
            'HenricoCounty', 'KingWilliamCounty', 'LoudounCounty', 'MadisonCounty',
            'PageCounty', 'PrinceEdwardCounty', 'PrinceWilliamCounty', 'RoanokeCounty',
            'RockbridgeCounty', 'RockinghamCounty', 'StaffordCounty', 'WarrenCounty', 'WytheCounty']

cities = ['Alexandria', 'Fredericksburg', 'Hampton', 'Hopewell', 'Lynchburg', 'NewportNews',
          'Norfolk', 'Richmond', 'Roanoke', 'Salem', 'Suffolk', 'VirginiaBeach', 'Winchester']

def cc_fx(string):
    
    done = False
    
    while done == False:
        
        for c in counties + cities + ['termination_clause']:
            
            if c in string:
                
                new_string = c
                done = True
                
                break
            
            elif c == 'termination_clause':
                
                new_string = ''
                done = True
                
    return new_string

locs = [re.sub('[^a-zA-Z]+', '', l) for l in locations]
locations = [cc_fx(l) for l in locs]

# Making a dataframe

dates = pd.Series(dates, name = 'Date')
ids = pd.Series(ids, name = 'CLOSURE_ID')
lanes_affected = pd.Series(lanes_affected, name = 'Lanes_Affected')
severity = pd.Series(severity, name = 'Severity')
coords = pd.Series(coords, name = 'Coordinates')
locations = pd.Series(locations, name = 'Locations')

df = pd.concat([df, ids, dates, coords, locations, lanes_affected, severity], axis = 1)

# Cleaning the data and removing duplicates

dup_id = [df.CLOSURE_ID[x] + df.Date[x] for x in range(len(df))]
dup_id = pd.Series(dup_id, name = 'dup_id')
df = pd.concat([df, dup_id], axis = 1)
df = df.drop_duplicates(subset = 'dup_id', keep = 'first').reset_index(drop = True)
df = df.drop(columns = ['dup_id'])
df = df[df.Locations != ''].reset_index(drop = True)

# Creating the true IV variables

days = list(df.Date.unique())
final_col_names = [c + '_bin' for c in counties + cities] + [c + '_count' for c in counties + cities] + [c + '_lanes' for c in counties + cities]
iv = pd.DataFrame()

for cat in range(3):
    
    for c in counties + cities:
        
        col = []
        tmp = df[df.Locations == c]
        
        for d in days:
            
            tmpx = tmp[tmp.Date == d]
            
            if cat == 0:
                
                col.append(int(len(tmpx) > 0))
                
            elif cat == 1:
                
                col.append(len(tmpx))
                
            else:
                
                la = [int(x) if '-' not in x else 0 for x in list(tmpx.Lanes_Affected)]
                col.append(sum(la))
                
        iv = pd.concat([iv, pd.Series(col)], axis = 1)
                
iv.columns = final_col_names
iv = pd.concat([pd.Series(days, name = 'Date'), iv], axis = 1)

# Write raw road closures data to file

iv.to_csv(filepath[:46] + 'data/iv.csv', index = False)

