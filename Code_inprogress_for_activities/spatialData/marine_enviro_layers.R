#install.packages("sdmpredictors")


library(sdmpredictors)
library(tidyverse)
library(terra)
library(ggplot2)
library(sf)
library(tidyterra)

#referring to the following vignette but not following it fully
#vignette: https://cran.r-project.org/web/packages/sdmpredictors/vignettes/quickstart.html

# exploring the marine datasets 
datasets <- list_datasets(terrestrial = FALSE, marine = TRUE)
layers <- list_layers(datasets)

bioOr<-list_layers(datasets="Bio-ORACLE")
SST <- load_layers("BO_sstmax")
SST<-terra::rast(SST) #now is terra compatible

# Heron_extent<-ext(151.90, 152.01, -23.48, -23.43) # order=xmin, xmax, ymin, ymax
# Heron_SST<-terra::crop(SST, Heron_extent)
# plot(Heron_SST)

xbuff<-0.15
ybuff<-0.1
Heron_centered<-ext(151.95-xbuff, 151.95+xbuff, -23.45-ybuff, -23.45+ybuff) # order=xmin, xmax, ymin, ymax
Heron_centered_SST<-terra::crop(SST, Heron_centered)

#my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127"))
#plot(Heron_centered_SST,col=my.colors(1000),axes=FALSE, box=FALSE)


# SST<-ggplot() +
#   geom_spatraster(data = Heron_centered_SST, show.legend = FALSE) +
#   scale_fill_whitebox_c(
#     palette = "bl_yl_rd", direction = 1, n.breaks = 10
#   ) +
#   theme_minimal()





#Get Allen coral atlas data
setwd("~/Documents/GitHub/Misc")
geomorphic<-st_read("../SharedData/Great-Barrier-Reef-and-Torres-Strait-20221208011653/Geomorphic-Map/geomorphic.gpkg")
#box<- c(xmin=151.5, ymin=-24, xmax=153, ymax=-23)
box<- c(xmin=151.95-xbuff, ymin=-23.45-ybuff, xmax=151.95+xbuff, ymax=-23.45+ybuff)

geomorphic.cropped<-st_crop(geomorphic, box)
#rm(geomorphic)

geomorphic.cropped<-geomorphic.cropped %>% 
  filter(class != "Terrestrial Reef Flat")

# geomorph<-ggplot(geomorphic.cropped) +
#   geom_sf(aes(fill = class), linewidth = 0.01, show.legend = FALSE) +
#   coord_sf(xlim = c(151.5, 153), ylim = c(-24, -23)) +
#   scale_fill_manual(name = "Geomorphic zones",
#                     values = grey.colors(9)) +
#   theme_classic(base_size = 12) 


geomorph.vect<-vect(geomorphic.cropped)

ggplot(geomorph.vect) +
  geom_spatraster(data = Heron_centered_SST, show.legend = FALSE) +
  scale_fill_whitebox_c(
    palette = "bl_yl_rd", direction = 1, n.breaks = 10
    ) +
  geom_spatvector(fill = "grey", col="grey")
