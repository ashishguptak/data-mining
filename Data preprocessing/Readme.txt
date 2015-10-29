Files included -

1)Iris.R
2)Income.R

How to run the code ?
Load the script onto R by selecting File->open script->filename(iris.R for iris distance and income.R for income dataset)
Select the entire script by Ctrl+A and then right click for Run line or Selection to run the script
Output is exported to two .csv files for each dataset (iris and income) respectively -
Iris - euclid_iris.csv
	manhattan_iris.csv
Income -euclid_income.csv
	manhattan_iris.csv

These files will be generated in your working directory of R - enter getwd() into R console to get the path.
Pls ensure library packages are present for exporting result to csv format by running library(xlsx) in console ,if not install the same package
