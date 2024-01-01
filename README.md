# Epinions-social-network

The dataset is a who-trust-whom online social network of a a general consumer review site Epinions.com. Members of the site can decide whether to ''trust'' each other. All the trust relationships interact and form the Web of Trust which is then combined with review ratings to determine which reviews are shown to the user.

The dataset contains approximately 10 million nodes, with each row representing a single edge in the graph connecting two nodes. The format of each row is <node-1>, <node-2>, #Edges, where #Edges is always 1 for each row. Therefore, the third column of the dataset can be removed once it is loaded as a matrix or a data frame.


The dataset presents an interesting opportunity to analyze the network structure and gain insights into the opinions expressed on the SOC-E website. By analyzing the connectivity of nodes and identifying clusters of nodes with similar connectivity patterns, we can uncover subcommunities of users with similar opinions. Through this project, I have used R and various data analysis tools to explore and visualize the data to gain insights into the structure and properties of the network.

