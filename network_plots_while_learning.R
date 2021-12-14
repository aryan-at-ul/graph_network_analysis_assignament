library(sna)



drugpaj <- read.paj('data_networks/drugnet2.paj')
drug <- drugpaj$networks[[1]]
gender <- drugpaj$partitions[[1]] 

suppressPackageStartupMessages(library(knitr)) 
kable(table(gender), col.names=c("Gender","Frequency"))

ethnicity <- drugpaj$partitions[[2]]
kable(table(ethnicity), col.names=c("Ethnicity","Frequency"))

png(filename="plots/simple_drug_user_plots.png")
plot(drug)
dev.off()


#plot(drug, vertex.col = colors, vertex.side = sides ,)