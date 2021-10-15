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
pb = pd.read_csv(filepath + 'data/pollution_data/pb_data.csv')

# Creating a list of all VA FIPS codes and municipalities

fips_list = [51003,51005,51007,51009,51011,51013,51015,51017,51019,51021,51023,
             51025,51027,51029,51031,51033,51035,51036,51037,51041,51043,51045,
             51047,51049,51051,51053,51057,51059,51061,51063,51065,51067,51069,
             51071,51073,51075,51077,51079,51081,51083,51085,51087,51089,51091,
             51093,51095,51097,51099,51101,51103,51105,51107,51109,51111,51113,
             51115,51117,51119,51121,51125,51127,51133,51135,51137,51139,51141,
             51143,51145,51147,51149,51153,51155,51157,51159,51161,51163,51165,
             51167,51169,51171,51173,51175,51177,51179,51181,51183,51185,51187,
             51191,51193,51195,51197,51199,51510,51520,51530,51540,51550,51570,
             51580,51590,51595,51600,51610,51620,51630,51640,51650,51660,51670,
             51678,51680,51683,51685,51690,51700,51710,51720,51730,51735,51740,
             51750,51760,51770,51775,51790,51800,51810,51820,51830,51840]

munic_list = ['AlbemarleCounty','AlleghanyCounty','AmeliaCounty','AmherstCounty',
              'AppomattoxCounty','ArlingtonCounty','AugustaCounty','BathCounty',
              'BedfordCounty','BlandCounty','BotetourtCounty','BrunswickCounty',
              'BuchananCounty','BuckinghamCounty','CampbellCounty','CarolineCounty',
              'CarrollCounty','CharlesCityCounty','CharlotteCounty','ChesterfieldCounty',
              'ClarkeCounty','CraigCounty','CulpeperCounty','CumberlandCounty',
              'DickensonCounty','DinwiddieCounty','EssexCounty','FairfaxCounty',
              'FauquierCounty','FloydCounty','FluvannaCounty','FranklinCounty',
              'FrederickCounty','GilesCounty','GloucesterCounty','GoochlandCounty',
              'GraysonCounty','GreeneCounty','GreensvilleCounty','HalifaxCounty',
              'HanoverCounty','HenricoCounty','HenryCounty','HighlandCounty',
              'IsleofWightCounty','JamesCityCounty','KingandQueenCounty','KingGeorgeCounty',
              'KingWilliamCounty','LancasterCounty','LeeCounty','LoudounCounty',
              'LouisaCounty','LunenburgCounty','MadisonCounty','MathewsCounty',
              'MecklenburgCounty','MiddlesexCounty','MontgomeryCounty','NelsonCounty',
              'NewKentCounty','NorthumberlandCounty','NottowayCounty',
              'OrangeCounty','PageCounty','PatrickCounty','PittsylvaniaCounty',
              'PowhatanCounty','PrinceEdwardCounty','PrinceGeorgeCounty','PrinceWilliamCounty',
              'PulaskiCounty','RappahannockCounty','RichmondCounty','RoanokeCounty',
              'RockbridgeCounty','RockinghamCounty','RussellCounty','ScottCounty',
              'ShenandoahCounty','SmythCounty','SouthamptonCounty','SpotsylvaniaCounty',
              'StaffordCounty','SurryCounty','SussexCounty','TazewellCounty','WarrenCounty',
              'WashingtonCounty','WestmorelandCounty','WiseCounty','WytheCounty',
              'YorkCounty','Alexandria','Bristol','BuenaVista','Charlottesville',
              'Chesapeake','ColonialHeights','Covington','Danville','Emporia',
              'Fairfax','FallsChurch','Franklin','Fredericksburg','Galax','Hampton',
              'Harrisonburg','Hopewell','Lexington','Lynchburg','Manassas','ManassasPark',
              'Martinsville','NewportNews','Norfolk','Norton','Petersburg','Poquoson',
              'Portsmouth','Radford','Richmond','Roanoke','Salem','Staunton','Suffolk',
              'VirginiaBeach','Waynesboro','Williamsburg','Winchester']

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

pollutants = ['Ozone', 'PM2.5', 'PM10', 'NO2', 'CO', 'Pb']
df_dates = []
locations = []
fips = []
polls = []
vals = []

for d in dates:
    
    print(d) # Visualize progress
    
    for p in range(6):
        
        for c in counties + cities:
            
            locations.append(c)
            fips.append(fips_list[agg_list.index(c)])
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
                
            elif p == 4:
                
                tmp = co[co.Date == int('2020' + d)]
                
            else:
                
                tmp = pb[pb.Date == int('2020' + d)]
            
            try:
                
                tmp = tmp[tmp.FIPS == fips_list[agg_list.index(c)]].reset_index(drop = True)
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

