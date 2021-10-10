# This script uses OSMnx to generate the road network data for Virginia municipalities

# Importing required modules

import pandas as pd
import osmnx as ox
import networkx as nx
import matplotlib.cm as cm
import matplotlib.colors as colors
import random

# Defining username + directories

username = ''
direc = 'C:/Users/' + username + '/Documents/Data/road_networks/'
figs_direc = direc + 'figures/'

# Defining the list of counties/cities for this study

counties = ['Albemarle County', 'Arlington County', 'Caroline County', 'Carroll County',
            'Charles City County', 'Chesterfield County', 'Culpeper County', 'Fairfax County',
            'Fauquier County', 'Frederick County', 'Giles County', 'Hanover County',
            'Henrico County', 'King William County', 'Loudoun County', 'Madison County',
            'Page County', 'Prince Edward County', 'Prince William County', 'Roanoke County',
            'Rockbridge County', 'Rockingham County', 'Stafford County', 'Warren County', 'Wythe County']

cities = ['Alexandria', 'Fredericksburg', 'Hampton', 'Hopewell', 'Lynchburg', 'Newport News',
          'Norfolk', 'Richmond', 'Roanoke', 'Salem', 'Suffolk', 'Virginia Beach', 'Winchester']

# Initializing a dataframe

df = pd.DataFrame()

# Declaring some lists for data containment (26 existing measures plus a novel one for road networks!)

col_names = []
closeness_centrality_mean = []
degree_assortativity_coefficient = []
degree_centrality_mean = []
degree_mean = []
density = []
diameter = []
dominating_set_proportion = []
eccentricity_mean = []
edge_betweenness_centrality_mean = []
edge_load_centrality_mean = []
effective_size_mean = []
harmonic_centrality_mean = []
in_degree_centrality_mean = []
load_centrality_mean = []
order = []
order_sc = []
order_sc_ratio = []
out_degree_centrality_mean = []
pagerank_mean = []
percolation_centrality_mean = []
radius = []
reciprocity = []
shortest_path_mean = []
shortest_path_mean2 = []
size = []
size_sc = []
size_sc_ratio = []
wiener_index = []

edge_length_total = []
edge_length_mean = []
streets_per_node_mean = []
intersections = [] 
node_density_km = []
street_length_total = []
street_segment_count = []
street_length_mean = []
circuity = []
self_loop_proportion = []
intersection_density_km = []
edge_density_km = []
street_density_km = []
pct_4way_intersections = []
pct_3way_intersections = []
pct_2way_intersections = []

# Main loop for data generation

for c in counties + cities:
    
    # Visualize progress...
    
    print(c)
    
    # Generate the network from OSM with OSMnx
    
    G = ox.graph_from_place(c + ', Virginia, USA', network_type = 'drive')
    
    # Impute speed on all edges missing data
    
    G = ox.add_edge_speeds(G)
    
    # Calculate travel time (seconds) for all edges
    
    G = ox.add_edge_travel_times(G)
    
    # Ensure that it is strongly connected
    
    Gc = ox.utils_graph.get_largest_component(G, strongly = True)
    
    # Additional street network / municipality data
    
    gdf = ox.geocode_to_gdf(c + ', Virginia, USA')
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
    
    cc = nx.closeness_centrality(Gc)
    dc = nx.degree_centrality(Gc)
    e = nx.eccentricity(Gc)
    ebc = nx.edge_betweenness_centrality(Gc)
    elc = nx.edge_load_centrality(Gc)
    es = nx.effective_size(Gc)
    hc = nx.harmonic_centrality(Gc)
    idc = nx.in_degree_centrality(Gc)
    lc = nx.load_centrality(Gc)
    odc = nx.out_degree_centrality(Gc)
    pr = nx.pagerank(Gc)
    pc = nx.percolation_centrality(Gc)
    
    col_names.append(c)
    closeness_centrality_mean.append(sum([cc[k] for k in Gc]) / len(Gc))
    degree_assortativity_coefficient.append(nx.degree_assortativity_coefficient(Gc))
    degree_centrality_mean.append(sum([dc[k] for k in Gc]) / len(Gc))
    degree_mean.append(Gc.size() / len(Gc))
    density.append(nx.density(Gc))
    diameter.append(nx.diameter(Gc))
    dominating_set_proportion.append(len(nx.dominating_set(Gc)) / len(Gc))
    eccentricity_mean.append(sum([e[k] for k in Gc]) / len(Gc))
    edge_betweenness_centrality_mean.append(sum([ebc[k] for k in ebc.keys()]) / Gc.size())
    edge_load_centrality_mean.append(sum([elc[k] for k in elc.keys()]) / Gc.size())
    effective_size_mean.append(sum([es[k] for k in Gc]) / len(Gc))
    harmonic_centrality_mean.append(sum([hc[k] for k in Gc]) / len(Gc))
    in_degree_centrality_mean.append(sum([idc[k] for k in Gc]) / len(Gc))
    load_centrality_mean.append(sum([lc[k] for k in Gc]) / len(Gc))    
    order.append(len(G))
    order_sc.append(len(Gc))
    order_sc_ratio.append(len(Gc) / len(G))
    out_degree_centrality_mean.append(sum([odc[k] for k in Gc]) / len(Gc))
    pagerank_mean.append(sum([pr[k] for k in Gc]) / len(Gc))
    percolation_centrality_mean.append(sum([pc[k] for k in Gc]) / len(Gc))
    radius.append(nx.radius(Gc))
    reciprocity.append(nx.reciprocity(Gc))
    size.append(G.size())
    size_sc.append(Gc.size())
    size_sc_ratio.append(Gc.size() / G.size())
    wiener_index.append(nx.wiener_index(Gc))
    
    V = list(pc.keys())
    V1 = random.sample(V,100)
    V = [v for v in V if v not in V1]
    V2 = random.sample(V,100)
    
    shortest_path_mean.append(sum([len(ox.distance.shortest_path(Gc, V1[v], V2[v], weight = 'length', cpus = 1)) for v in range(100)]) / 100)
    shortest_path_mean2.append(sum([len(ox.distance.shortest_path(Gc, V1[v], V2[v], weight = 'travel_time', cpus = 1)) for v in range(100)]) / 100)
    
    # Grab some stats from ox_stats Series
    
    edge_length_total.append(ox_stats['edge_length_total'])
    edge_length_mean.append(ox_stats['edge_length_avg'])
    streets_per_node_mean.append(ox_stats['streets_per_node_avg'])
    intersections.append(ox_stats['intersection_count'])
    street_length_total.append(ox_stats['street_length_total'])
    street_segment_count.append(ox_stats['street_segment_count'])
    street_length_mean.append(ox_stats['street_length_avg'])
    circuity.append(ox_stats['circuity_avg'])
    self_loop_proportion.append(ox_stats['self_loop_proportion'])
    node_density_km.append(ox_stats['node_density_km'])
    intersection_density_km.append(ox_stats['intersection_density_km'])
    edge_density_km.append(ox_stats['edge_density_km'])
    street_density_km.append(ox_stats['street_density_km'])
    pct_4way_intersections.append(ox_stats['streets_per_node_proportions'][4])
    pct_3way_intersections.append(ox_stats['streets_per_node_proportions'][3])
    pct_2way_intersections.append(ox_stats['streets_per_node_proportions'][2])

# Appending network data to df

closeness_centrality_mean = pd.Series(closeness_centrality_mean, name = 'Closeness Centrality')
degree_assortativity_coefficient = pd.Series(degree_assortativity_coefficient, name = 'Degree Assortativity Coefficient')
degree_centrality_mean = pd.Series(degree_centrality_mean, name = 'Degree Centrality')
degree_mean = pd.Series(degree_mean, name = 'Average Degree')
density = pd.Series(density, name = 'Density')
diameter = pd.Series(diameter, name = 'Diameter')
dominating_set_proportion = pd.Series(dominating_set_proportion, name = 'Dominating Set Size')
eccentricity_mean = pd.Series(eccentricity_mean, name = 'Eccentricity')
edge_betweenness_centrality_mean = pd.Series(edge_betweenness_centrality_mean, name = 'Edge Betweenness Centrality')
edge_load_centrality_mean = pd.Series(edge_load_centrality_mean, name = 'Edge Load Centrality')
effective_size_mean = pd.Series(effective_size_mean, name = 'Effective Size')
harmonic_centrality_mean = pd.Series(harmonic_centrality_mean, name = 'Harmonic Centrality')
in_degree_centrality_mean = pd.Series(in_degree_centrality_mean, name = 'In-Degree Centrality')
load_centrality_mean = pd.Series(load_centrality_mean, name = 'Load Centrality')
order = pd.Series(order, name = 'Order')
order_sc = pd.Series(order_sc, name = 'Order Sc')
order_sc_ratio = pd.Series(order_sc_ratio, name = 'Order SC Ratio')
out_degree_centrality_mean = pd.Series(out_degree_centrality_mean, name = 'Out-Degree Centrality')
pagerank_mean = pd.Series(pagerank_mean, name = 'Pagerank')
percolation_centrality_mean = pd.Series(percolation_centrality_mean, name = 'Percolation Centrality')
radius = pd.Series(radius, name = 'Radius')
reciprocity = pd.Series(reciprocity, name = 'Reciprocity')
shortest_path_mean = pd.Series(shortest_path_mean, name = 'Shortest Path Length')
shortest_path_mean2 = pd.Series(shortest_path_mean2, name = 'Shortest Path Travel Time')
size = pd.Series(size, name = 'Size')
size_sc = pd.Series(size_sc, name = 'Size SC')
size_sc_ratio = pd.Series(size_sc_ratio, name = 'Size SC Ratio')
wiener_index = pd.Series(wiener_index, name = 'Wiener Index')

edge_length_total = pd.Series(edge_length_total, name = 'Total Edge Length')
edge_length_mean = pd.Series(edge_length_mean, name = 'Average Edge Length')
streets_per_node_mean = pd.Series(streets_per_node_mean, name = 'Streets per Node')
intersections = pd.Series(intersections, name = 'Intersection') 
street_length_total = pd.Series(street_length_total, name = 'Total Street Length')
street_segment_count = pd.Series(street_segment_count, name = 'Street Segments')
street_length_mean = pd.Series(street_length_mean, name = 'Average Street Length')
circuity = pd.Series(circuity, name = 'Circuity')
self_loop_proportion = pd.Series(self_loop_proportion, name = 'Self Loop Proportion')
node_density_km = pd.Series(node_density_km, name = 'Node Density')
intersection_density_km = pd.Series(intersection_density_km, name = 'Intersection Density')
edge_density_km = pd.Series(edge_density_km, name = 'Edge Density')
street_density_km = pd.Series(street_density_km, name = 'Street Density')
pct_4way_intersections = pd.Series(pct_4way_intersections, name = '4-way Intersection Frequency')
pct_3way_intersections = pd.Series(pct_3way_intersections, name = '3-way Intersection Frequency')
pct_2way_intersections = pd.Series(pct_2way_intersections, name = '2-way Intersection Frequency')

df = pd.concat([df, closeness_centrality_mean, degree_assortativity_coefficient, degree_centrality_mean,
                degree_mean, density, diameter, dominating_set_proportion, eccentricity_mean,
                edge_betweenness_centrality_mean, edge_load_centrality_mean, effective_size_mean,
                harmonic_centrality_mean, in_degree_centrality_mean, load_centrality_mean,
                order, order_sc, order_sc_ratio, out_degree_centrality_mean, pagerank_mean,
                percolation_centrality_mean, radius, reciprocity, shortest_path_mean, size,
                size_sc, size_sc_ratio, wiener_index, edge_length_total, edge_length_mean,
                streets_per_node_mean, intersections, street_length_total,
                street_segment_count, street_length_mean, circuity, self_loop_proportion,
                node_density_km, intersection_density_km, edge_density_km, street_density_km,
                pct_4way_intersections, pct_3way_intersections, pct_2way_intersections], axis = 0)

df.columns = col_names

# Write network statistics to file

df.to_csv(direc + 'road_network_statistics.csv')

