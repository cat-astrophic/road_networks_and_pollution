# This script gets minute level road closure data from VDOT

# Importing required modules

import time
import urllib
from bs4 import BeautifulSoup as bs

# Where to save data

username = ''
rawclosurespath = 'C:/Users/' + username + '/Documents/Data/road_networks/raw_closures_data/'

# Defining the query components

base = 'https://smarterroads.org/dataset/download/33?token='
token = 'oO9kryrcQxW8CyfYMWwaSzxSi9nkrMeTv094yusDkMSCZ9vvCc0KHLgclwnRlfoP'
mid = '&file=hub_data/RoadClosures/2020/'

# A reference list of days and times to get data for

days = []
m_lens = [31,29,31,30,31,30,31,31,30,31,30,31]

for i in range(12):
    
    if len(str(i+1)) < 2:
        
        x = '0' + str(i+1)
        
    else:
        
        x = str(i+1)
        
    for j in range(m_lens[i]):
        
        if len(str(j+1)) < 2:
            
            y = '0' + str(j+1)
            
        else:
            
            y = str(j+1)
            
        days.append(x+y)

times = []

for i in range(24):
    
    if len(str(i)) < 2:
        
        x = '0' + str(i)
        
    else:
        
        x = str(i)
        
    for j in range(60):
        
        if len(str(j)) < 2:
            
            y = '0' + str(j)
            
        else:
            
            y = str(j)
            
        times.append(x+y)

# Main loop

for d in days:
    
    for t in times:
        
        try: # Exception handling for minutes with no associated .json file
            
            print('Collecting data for ....... Day ' + str(days.index(d) + 1) + ' of ' + str(len(days)) + ' :: Minute ' + str(times.index(t)+1) + ' of ' + str(len(times))) # Visual progress check
            url = base + token + mid + d[:2] + '/' + d[2:] + '/' + t[:2] + '/' + t[2:] + '/RoadClosures_2020' + d + '_' + t + '.json' # Define the url
            page = urllib.request.Request(url, headers = {'User-Agent': 'Mozilla/5.0'}) # Define page
            response = urllib.request.urlopen(page) # Open page
            soup = bs(response, 'html.parser') # Get data from the page
            time.sleep(0.1)
            
            # Write the data to file
            
            with open(rawclosurespath + d + '_' + t + '.txt', 'w') as f:
                
                f.write(str(soup))
                f.close()
                
        except:
            
            continue

