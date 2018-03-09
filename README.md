## graph similarity analysis

this repository is designated as an auxiliary supplement of the article _"Spatial predictive modeling of prehistoric sites in the Bohemian-Moravian Highlands based on graph similarity analysis"_

### data

**data/geo.csv** - list of cadasters and their unscaled values for geographical variables
**data/components.csv** - list of cadasters and the presence of the archaeological location within each chronological component

### scripts

**find_hd** - the main script that iteratively creates weighted distance matrices and calculates the hdist value
**test.R** - script for preparing the testing dataset for evaluation
**boxplot.R** - calculates the statistical descriptors needed for the boxplot visualisation
