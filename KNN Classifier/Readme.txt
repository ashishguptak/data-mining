Files included -

1)Iris_KNN.R - contains code for KNN classifier algorithm for Iris Training and test dataset 
2)Income_KNN.R- contains code for KNN classifier algorithm for Income Training and test dataset 

How to run the code ?
Load the script onto R by selecting File->open script->filename(Iris_HW2.R for iris distance and Income_HW2.R for income dataset)
Select the entire script by Ctrl+A and then right click for Run line or Selection to run the script
Once the code starts excuting, first the Training dataset csv file has to be selected and then in second prompt, Test dataset csv file needs to be given. 
Output is exported to two .csv files for each dataset (Iris and Income) respectively -
Iris -  iris_euclidean_KNN.csv
	iris_manhattan_KNN.csv
Income -income_euclid_KNN.csv
	income_manhattan_KNN.csv

These files will be generated in your working directory of R - enter getwd() into R console to get the path.
Pls ensure library packages are present for exporting result to csv format by running library(xlsx) in console ,if not install the same package
