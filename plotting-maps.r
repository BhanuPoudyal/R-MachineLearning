#Plotting maps

library(ggmap)

indiamap <- qmap("India", zoom=5, legend="bottom")

indiamap

poi <- read.csv("capoi.csv")
summary(poi)

calimap <-qmap("California", zoom=7, 
               legend="bottom", maptype="satellite")
calimap

calimap <-qmap("California", zoom=7, 
               legend="bottom", maptype="toner")
calimap +  geom_point(aes(x=Longitude, y=Latitude,
                          color=PointOfInterest), 
                      shape=20, size=3,
                      data = poi[ poi$PointOfInterest %in% 
                                    c("airport", "dam", "reservoir", "tunnel"), ]) 
