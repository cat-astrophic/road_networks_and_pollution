# This script uses OSMnx to generate the road network data for US municipalities

# Importing required modules

import pandas as pd
import osmnx as ox
import networkx as nx
import matplotlib.cm as cm
import matplotlib.colors as colors

# Defining username + directories

username = ''
direc = 'C:/Users/' + username + '/Documents/Data/road_networks/'
figs_direc = direc + 'figures2/'

# Defining the list of locations to include in this study

locations = ['Atlanta, Georgia', 'Austin, Texas', 'Baltimore, Maryland', 
             'Birmingham, Alabama', 'Boston, Massachusetts', 'Buffalo, New York', 
             'Charlotte, North Carolina', 'Chicago, Illinois', 'Cincinnati, Ohio',
             'Columbus, Ohio', 'Dallas, Texas', 'Denver, Colorado', 
             'Detroit, Michigan', 'Grand Rapids, Michigan', 'Hartford, Connecticut', 
             'Houston, Texas', 'Indianapolis, Indiana', 'Jacksonville, Florida', 
             'Kansas City, Missouri', 'Las Vegas, Nevada', 'Los Angeles, California', 
             'Louisville, Kentucky', 'Memphis, Tennessee', 'Miami, Florida', 
             'Milwaukee, Wisconsin', 'Minneapolis, Minnesota', 'Nashville, Tennessee', 
             'New Orleans, Louisana', 'New York, New York', 'Oklahoma City, Oklahoma', 
             'Orlando, Florida', 'Philadelphia, Pennsylvania', 'Phoenix, Arizona', 
             'Pittsburgh, Pennsylvania', 'Portland, Oregon', 'Providence, Rhode Island', 
             'Raleigh, North Carolina', 'Richmond, Virginia', 'Riverside, California', 
             'Rochester, New York', 'Sacramento, California', 'Salt Lake City, Utah', 
             'San Antonio, Texas', 'San Diego, California', 'San Francisco, California', 
             'San Jose, California', 'Seattle, Washington', 'St. Louis, Missouri', 
             'Tampa, Florida', 'Tucson, Arizona', 'Virginia Beach, Virginia', 'Washington, DC']

# Initializing a dataframe

df = pd.DataFrame()

# Declaring some lists for data containment

col_names = []
edge_betweenness_centrality_mean = []
load_centrality_mean = []
circuity = []
pct_4way_intersections = []
pct_3way_intersections = []

# Main loop for data generation

for c in locations:
    
    # Visualize progress...
    
    print(c)
    
    try:
        
        # Generate the network from OSM with OSMnx

        G = ox.graph_from_place(c + ', USA', network_type = 'drive')

        # Impute speed on all edges missing data

        G = ox.add_edge_speeds(G)

        # Calculate travel time (seconds) for all edges

        G = ox.add_edge_travel_times(G)

        # Ensure that it is strongly connected

        Gc = ox.utils_graph.get_largest_component(G, strongly = True)

        # Additional street network / municipality data

        gdf = ox.geocode_to_gdf(c + ', USA')
        area = ox.project_gdf(gdf).unary_union.area

        # Built-in OSMnx stats

        stats = ox.basic_stats(Gc, area = area)
        ox_stats = pd.Series(stats)

        # Create a network figure

        fig, ax = ox.plot_graph(Gc)
        fig.savefig(figs_direc + c.replace(' ', '_') + '__network.eps')
        fig.savefig(figs_direc + c.replace(' ', '_') + '__network.png')

        # Create an edge centrality figure

        edge_centrality = nx.closeness_centrality(nx.line_graph(Gc))
        ev = [edge_centrality[edge + (0,)] for edge in Gc.edges()]
        norm = colors.Normalize(vmin = min(ev)*0.8, vmax = max(ev))
        cmap = cm.ScalarMappable(norm = norm, cmap = cm.inferno)
        ec = [cmap.to_rgba(cl) for cl in ev]
        fig, ax = ox.plot_graph(Gc, bgcolor = 'k', node_size = 0, node_color = 'w',
                                node_edgecolor = 'gray', node_zorder = 2, edge_color = ec,
                                edge_linewidth = 1.5, edge_alpha = 1)
        fig.savefig(figs_direc + c.replace(' ', '_') + '__edge_centrality.eps')
        fig.savefig(figs_direc + c.replace(' ', '_') + '__edge_centrality.png')

        # Setup for nx.percolation_centrality

        nx.set_node_attributes(Gc, 0.1, 'percolation')
        percolation_cent_dict = nx.percolation_centrality(G = Gc, attribute = 'percolation')    

        # Generating network statistics

        ebc = nx.edge_betweenness_centrality(Gc)
        lc = nx.load_centrality(Gc)

        col_names.append(c)
        edge_betweenness_centrality_mean.append(sum([ebc[k] for k in ebc.keys()]) / Gc.size())
        load_centrality_mean.append(sum([lc[k] for k in Gc]) / len(Gc))

        # Grab some stats from ox_stats Series

        circuity.append(ox_stats['circuity_avg'])
        pct_4way_intersections.append(ox_stats['streets_per_node_proportions'][4])
        pct_3way_intersections.append(ox_stats['streets_per_node_proportions'][3])
        
    except:
        
        col_names.append(c)
        edge_betweenness_centrality_mean.append(' ')
        load_centrality_mean.append(' ')
        circuity.append(' ')
        pct_4way_intersections.append(' ')
        pct_3way_intersections.append(' ')
        
# Appending network data to df

edge_betweenness_centrality_mean = pd.Series(edge_betweenness_centrality_mean, name = 'Edge Betweenness Centrality')
load_centrality_mean = pd.Series(load_centrality_mean, name = 'Load Centrality')
circuity = pd.Series(circuity, name = 'Circuity')
pct_4way_intersections = pd.Series(pct_4way_intersections, name = '4-way Intersection Frequency')
pct_3way_intersections = pd.Series(pct_3way_intersections, name = '3-way Intersection Frequency')

df = pd.concat([df, edge_betweenness_centrality_mean, load_centrality_mean, circuity,
                pct_4way_intersections, pct_3way_intersections], axis = 1)

df.index = col_names
df.columns = ['edge_betweenness_centrality_mean', 'load_centrality_mean',
              'circuity', 'pct_4way_intersections', 'pct_3way_intersections']
df.index.name = 'Locations'

# Write network statistics to file

df.to_csv(direc + 'data/road_network_statistics_mechanisms.csv')

