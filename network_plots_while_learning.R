library(sna)



drugpaj <- read.paj('data_networks/drugnet2.paj')
drug <- drugpaj$networks[[1]]
gender <- drugpaj$partitions[[1]] 
