# Working through general GDM and GF tutorials 2023

#GDM tutorial from
#https://mfitzpatrick.al.umces.edu/gdm/#section-2---advanced-spatial-analyses-using-gdm

#install.packages("gdm")
library(gdm)


# Prepare data - the site pair table --------------------------------------

# Note that rows much match and be free of NAs

str(southwest)

#create species table by site table in long format
sppTab <- southwest[, c("species", "site", "Long", "Lat")]
# this long form is "bioFormat=2"

#create environmental table
envTab <- southwest[, c(2:ncol(southwest))]


# FORMATTING DATA FROM PRESENCE OBSERVATIONS (bioFormat = 2) --------------

# format data frame - calculate Bray-Curtis distances 
gdmTab <- formatsitepair(bioData=sppTab, 
                         bioFormat=2, #x-y spp list
                         XColumn="Long", 
                         YColumn="Lat",
                         sppColumn="species", 
                         siteColumn="site", 
                         predData=envTab)
#The first column of a site-pair table contains a biological distance measure 
#(the default is Bray-Curtis distance though any measure scaled between 0-1 is 
#acceptable). The second column contains the weight to be assigned to each data 
#point in model fitting (defaults to 1 if equal weighting is used, but can be 
#customized by the user or can be scaled to site richness, see below). 

#A properly formatted site-pair table will have at least six columns (distance, 
#weights, s1.xCoord, s1.yCoord, s2.xCoord, s2.yCoord) and some number more 
#depending on how many predictors are included



# FORMATTING DATA WHEN YOU HAVE A (SQUARE) DISTANCE MATRIX (bioFor --------

dim(gdmDissim) #biological distance matrix 
gdmDissim[1:5, 1:5]
site <- unique(sppTab$site)
gdmDissim <- cbind(site, gdmDissim) #bind site name 

gdmTab.dis <- formatsitepair(bioData=gdmDissim, 
                             bioFormat=3, #diss matrix 
                             XColumn="Long", 
                             YColumn="Lat", 
                             predData=envTab, 
                             siteColumn="site")


#(Skipped section on weighting sites by species richness)


# GDM fitting  ------------------------------------------------------------

gdm.1 <- gdm(data=gdmTab, geo=TRUE)
summary(gdm.1)


length(gdm.1$predictors) # get ideal of number of panels
plot(gdm.1, plot.layout=c(4,3))

# can extract spine info
gdm.1.splineDat <- isplineExtract(gdm.1)
str(gdm.1.splineDat)
plot(gdm.1.splineDat$x[,"bio19"], 
     gdm.1.splineDat$y[,"bio19"], 
     lwd=3,
     type="l", 
     xlab="Winter precipitation (mm)", 
     ylab="Partial ecological distance")


# Using a fitted GDM to predict biological dissimilarity between s --------
# For demonstration purposes, we use the same table as that was used to fit the 
# model, though predictions to new sites (or times) can be made as well assuming 
#the same set of environmental/spatial predictors are available at those locations 
#(or times).

gdm.1.pred <- predict(object=gdm.1, data=gdmTab)

head(gdm.1.pred)

plot(gdmTab$distance, 
     gdm.1.pred, 
     xlab="Observed dissimilarity", 
     ylab="Predicted dissimilarity", 
     xlim=c(0,1), 
     ylim=c(0,1), 
     pch=20, 
     col=rgb(0,0,1,0.5))
lines(c(-1,2), c(-1,2))

# Predicting biological change through time
# fit a new gdm using a table with climate data only (to match rasters)
gdm.rast <- gdm(gdmTab.rast, geo=T). #data not found ... 

# make some fake climate change data
futRasts <- swBioclims
##reduce winter precipitation by 25% & increase temps
futRasts[[3]] <- futRasts[[3]]*0.75
futRasts[[4]] <- futRasts[[4]]+2
futRasts[[5]] <- futRasts[[5]]+3

timePred <- predict(gdm.rast, swBioclims, time=T, predRasts=futRasts)
raster::plot(timePred, col=rgb.tables(1000))

# more code available on transforming spatial predictors and visualising




# Gradient forests --------------------------------------------------------


#install.packages("gradientForest", repos="http://R-Forge.R-project.org")

library(gradientForest)