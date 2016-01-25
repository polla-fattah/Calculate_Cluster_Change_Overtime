## Calculating cluster changes over time ##
This R code generates items into clusters then provides temporal dimension then calculates changes over time.


###generateData.R
Generates required items of data with specified characterstics like number of clusters, distance betweeen clusters then generates next generation by mutating data. By mutation we mean jumping items from one cluster into another.

###calculateChanges.R
Uses Area under the curve and external cluster validity to calculate cluster changes between two time points.
