Files included -

1)TwoDim.R - contains code for K Means Clustering algorithm for TwoDimEasy and TwoDimHard datasets 
2)Wine.R- contains code for K Means Clustering algorithm for Wine dataset 

How to run the code ?
Load the script onto R by selecting File->open script->filename(TwoDim.R for TwoDim datasets and Wine.R for Wine dataset)
Select the value for no of clusters K which is present in third line of code for both files (by default k value is 2 for TwoDim.R and 5 for Wine Dataset)
Select the entire script by Ctrl+A and then right click for Run line or Selection to run the script
Once the code starts excuting, first the dataset csv files - TwoDimEasy/Hard for TwoDim.R and Wine.csv for Wine.R have to be selected
Output is exported to .csv files for each dataset respectively -
TwoDim_cluster.csv
Wine_cluster.csv

These files will be generated in your working directory of R - enter getwd() into R console to get the path.
Pls ensure library packages are present for exporting result to csv format by running library(xlsx) in console ,if not install the same package
Packages needed - library(cluster) for generating scatterplots for K means clusters
