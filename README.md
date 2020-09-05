# mice-vs-amelia-sim
Simulation to compare multiple imputation techniques with longitudinal data

# Context
As a project assistant on the Study of Iowa High Schools, I implemented a multiple imputation procedure to handle missing data in student achievement scores. Before I could do so, we needed to choose an imputation technique. One of the more common techniques is called "MICE" and another is called "Amelia". In order to figure out which one to prefer, I setup a simulation to compare the performance of the two techniques. The idea is to generate complete data, introduce some missingness, then run both techniques and see which one can get the closest to the truth with the least amount of variation across the imputed data sets. 
