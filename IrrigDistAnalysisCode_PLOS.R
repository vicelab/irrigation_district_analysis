##########################################################################
#### This is the final script for the Irrigation District Analysis     ###
#### manuscript submitted to PLOS Water                                ###
#### For questions on the content of this script reach out to:         ###
#### Vicky Espinoza (espinoza.vicky42@gmail.com) or                    ###
###  Joshua Viers (jviers@ucmerced.edu)                                ###
##########################################################################

## Cluster Analysis (5 Clusters- No Lat/Lon)
## Analysis using Consensus Cluster Plus 

###Installation of the required libraries and packages
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("ConsensusClusterPlus")

library(tidyverse)
library(dplyr)
library(ConsensusClusterPlus)
library(corrplot)
library(ggplot2)
library(factoextra)
library(FactoMineR)
library(data.table)

setwd("C:/Users/vespinoz/Desktop/FinalScripts/PLOSDataDryad/")
raw<- read.csv("IDManuscript_DataToCluster_Sept262021.csv")

### Refer to manuscript SI Table 2 for a list of the variables (with an asterisk)
### used in the cluster analysis 


################
## Run PCA   ###
################
res.pca <- PCA(dataset, graph = FALSE)
res.hcpc <- HCPC(res.pca, graph = FALSE)
fviz_dend(res.hcpc, 
          cex = 0.7,                     # Label size
          palette = "jco",               # Color palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          labels_track_height = 0.8      # Augment the room for labels
)
## Create a cluster visualization of PCA
fviz_cluster(res.hcpc,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)


##ggbiplot 
library (devtools)
library(ggbiplot)

data.pca<-prcomp(dataset, center = TRUE,scale. = FALSE)
ggbiplot(data.pca)
ggbiplot(data.pca, choices=c(3,4))
## Prepare the PCA results to be used in Consensus Cluster Plus cluster runs
pca_clust <- res.hcpc$data.clust
d = pca_clust
d <- as.matrix(d)
d <-t(d)

###################################
####    correlation plot      #####
###################################
# data2<-na.omit(dataset)
corrmatrix <- cor(dataset)
corrplot(corrmatrix, method = 'color')


######################################
########### Maximum Cluster5 ########
######################################

results5 = ConsensusClusterPlus(d,maxK=5,reps=1000,pItem=0.8,pFeature=1,title="conclust1",
                                distance="euclidean",clusterAlg="km")

resICL5 = calcICL(results5,title="conclust1")

#can assign to a dataframe output
output5 <- results5[[5]]

#assign a portion of output to its own dataframe
conClassF5 <- as.data.table(output5$consensusClass)

#Create append the cluster 5 to the irrigation district data used for cluster run
finaldata<-raw %>%
  na.omit()



finaldata$Cluster5<-conClassF5$V1

#Export the appended cluster data to CSV for further analysis in ArcGIS Pro
write.csv(finaldata, "Clust5.csv")


#########################################
####### ERA Analysis (Create Figure 3b) #
#########################################

setwd("C:/Users/vespinoz/Desktop/FinalScripts/PLOSDataDryad/")
data<- read.csv("EspinozaViers_IrrigDistSupplementalInformation_Table7_IrrigationDistricts.csv")

#exclude Stevinson WD
data<-data[-c(81),]

library(tidyverse)
library (ggplot2)
library(reshape2)
library(patchwork)


# ## Boxplots of reliable and theoretical surface water (Not log-scaling)
water <- melt(data,id.vars='Era', measure.vars=c('SWAlloc_MLHA','SWDelivery_MLHa'))

ggplot(water) +
  geom_boxplot(aes(x=Era, y=value, fill=variable))+ 
  scale_y_continuous(name="Surface Water Allocation/Delivery (ML/Ha)")+
  scale_fill_manual(values=c("Purple","lightblue"), labels=c("Allocation", "Average Delivery"))+
  theme_minimal()+
  theme(legend.title = element_blank())+
  theme(strip.text = element_text(size=20))+
  theme(axis.text = element_text(size = 16))+
  theme(axis.title.y.right = element_text(vjust=1))+
  theme(axis.title = element_text(size = 20))+
  theme(legend.text = element_text(size = 16))+
  theme(legend.position="bottom",legend.box="vertical", legend.margin=margin())

