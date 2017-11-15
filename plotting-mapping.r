#------------------------------------------------------------------------------
#                         Statistics in R
#------------------------------------------------------------------------------

iris_df <- iris
summary(iris_df)

mean(iris_df$Sepal.Length)
range(iris_df$Petal.Width)

cor(iris_df[,1:4])

library(psych)
pairs.panels(iris_df)

iris_df$Species <- as.numeric(iris_df$Species)
iris_model <- lm(Species ~ . , iris_df)
summary(iris_model)

#------------------------------------------------------------------------------
#                         Base Plotting System
#------------------------------------------------------------------------------

par(mfrow=c(1,1))
stripchart(iris$Sepal.Width)

hist(iris$Sepal.Width)
hist(iris$Sepal.Width, col="cyan",
     main="Sepal Width", xlab="Dist of width", ylab="Sample Count")

boxplot(iris$Sepal.Width)
boxplot(iris$Sepal.Width ~ iris$Species, col="green")

plot(iris$Sepal.Length, iris$Sepal.Width)
plot(iris$Sepal.Length, iris$Sepal.Width, col=iris$Species)

plot(iris$Sepal.Length,  type="l")
plot(sort(iris$Sepal.Length), type="l")

irisagg <- aggregate(iris[,1:4], by=list(iris$Species), FUN="mean" )
irisagg

barplot(irisagg$Sepal.Length, names.arg=irisagg$Group.1,
        legend.text=irisagg$Group.1, col=irisagg$Group.1)

#------------------------------------------------------------------------------
#                         ggplot
#------------------------------------------------------------------------------

data(mtcars)
summary(mtcars)
head(mtcars)

library(ggplot2)

#line plot
ggplot( data=mtcars, aes( x=mpg, y=wt ) ) + 
  geom_line()


#histogram
ggplot(mtcars, aes(x=cyl)) + 
  geom_histogram(binwidth=1, colour="green") +
  theme_bw()

#density
ggplot(mtcars, aes(x=cyl)) + 
  geom_density()

#box plot
ggplot(mtcars, aes(x=factor(cyl), y=mpg, colour=factor(cyl))) + 
  geom_boxplot() + 
  labs(title="Boxplot", x="# of cylinders", 
       y="Miles per gallon")


# scatter plot
ggplot( data=mtcars, aes( x=mpg, y=wt, 
                          colour=as.factor(cyl), shape=as.factor(gear),
                          label=as.character(rownames(mtcars)) ) ) + 
  geom_point(size=6) +
  geom_text(size=3, colour="black")

#pie chart
ggplot(mtcars, aes(x=factor(1), fill=factor(cyl))) + 
  geom_bar(width=1) +  
  coord_polar(theta="y") 

# Faceting 

ggplot(mtcars, aes(x=hp, y=mpg, colour=factor(am))) + 
  geom_point() + 
  facet_grid( cyl ~ gear ) +
  theme_bw() 


# Heat Maps

library(graphics); 
library(grDevices)

heatmap(as.matrix(mtcars), Rowv = NA, Colv = NA, scale = "column",
        main = "heatmap")

#Time Series

timeseries <- read.csv("timeseries.csv")
head(timeseries)

timeseries$Date2 <- as.Date(timeseries$Date, 
                            format="%m/%d/%Y")

ggplot(timeseries, aes(Date2, 
                       y=Value, colour="blue")) + 
  geom_line()

ggplot(timeseries, aes(format(Date2,'%m'),
                       y=Value, colour=format(Date2,'%b'))) +
  geom_boxplot()










































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


















