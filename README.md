# Road Networks and Pollution

This repo is for a project studying the impact of road network structure on air pollution. In order to recreate the data there are two phases. First, in the epa_aqs folder there are a series of files which, when run following the instructions in the epa_aqs readme file, will produce a pollution data set with observations at the county-day level. Once this is complete, ensure the username + directory match the folder structure on your machine and run the following scripts:

1. vdot_road_closures_scraper.py *(this script scrapes raw data on road closures from VDOT)*
2. road_nets_IV.py *(this script creates the actual IVs)*
3. road_nets_va_poll_data.py *(this script creates VA pollution data)*
4. road_nets_.py *(this script uses OSMnx to collect road network data from OSM, create network statistics, and create figures)*
5. road_nets_data_maker.py *(this script creates the final data set)*

### Installing OSMnx:

This project uses OSMnx to access the OSM API and retrieve road network data. To install OSMnx:
1. Install Anaconda
2. Open the Anaconda prompt
3. Run the following code:
  * conda create --name geo *(this creates a new environment called geo)*
  * conda acticate geo *(this activates the new environment)*
  * conda install -c conda-forge fiona shapely rasterio pyproj pandas jupyterlab geopandas osmnx *(this installs what is needed for OSMnx in geo)*
  * jupyter lab *(now that everything is installed in geo, run jupyter lab and start working!)*
