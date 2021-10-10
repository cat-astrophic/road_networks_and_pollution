# road_networks_and_pollution

This project uses OSMnx to access the OSM API and retrieve road network data. To install OSMnx:
1. Install Anaconda
2. Open the Anaconda prompt
3. Run the following code:
  * conda create --name geo *(this creates a new environment called geo)*
  * conda acticate geo *(this activates the new environment)*
  * conda install -c conda-forge fiona shapely rasterio pyproj pandas jupyterlab geopandas osmnx *(this installs what is needed for OSMnx in geo)*
  * jupyter lab *(now that everything is installed in geo, run jupyter lab and start working!)*
