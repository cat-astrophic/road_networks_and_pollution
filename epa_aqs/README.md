This folder contains the scripts used from the ultrapollution project to collect, parse, and compute mean county-day pollution data for the pollutants used in this project.

To create this data, ensure that usernames + output directories are correctly specified within the scripts and run them in the following order:

1. epa_aqs_api.py **
2. epa_aqs_cleaner_pm.py
3. epa_aqs_cleaner_pm.py
4. epa_aqs_cleaner_pm.py
5. epa_aqs_cleaner_pm.py
6. epa_aqs_cleaner_pm.py
7. epa_aqs_county_data_maker.py

** epa_aqs_api.py is currently set up to collect raw data for one pollutant at a time. All relevant metadata to collect raw data for the other pollutants is contained in the script, along with the means to collect raw data on all pollutants in one loop. The total raw data for some pollutants is quite large, hence I have it collecting data for one pollutant at a time. 
