#Chan Voong
#PennDesign MUSA '17
#MUSA 620 Data Wrangling with Max Galka
#Hw#9 - Crime in Philadelphia - Animated Viz

install.packages("rgeos")
install.packages("maps")
install.packages("maptools")
install.packages("mapproj")
install.packages("ggplot2")
install.packages("viridis")
install.packages("pylr")
install.packages("animation")

library(rgeos)
library(maps)
library(maptools)
library(mapproj)
library(ggplot2)
library(viridis)
library(pylr)
library(animation)

install.packages("magick")
library(magick)



#This assignment requires ImageMagick
#https://www.imagemagick.org/script/index.php
#for osx users, use homebrew or macports to install 


#Import census tracts and accidents data 
Crime <- read.csv("/Users/Chan/Documents/Classes/Data Wrangling/MUSA-620-Week-9-master/PPD_Crime_Incidents_2006_Present_2_.csv")
Tracts <- readShapeSpatial("/Users/Chan/Documents/Classes/Data Wrangling/MUSA-620-Week-9-master/censustracts_phila/censustractsphilly.shp")

#load in each crime year file
#combine the files
crime <- rbind(crime_2006_sum, crime_2007_sum, crime_2008_sum, crime_2009_sum,
               crime_2010_sum, crime_2011_sum1, crime_2012_sum, crime_2014_sum1, crime_2013_sum1, crime_2015_sum1, crime_2016_sum1)

#SAVE FILE
write.csv(crime, "~/Documents/Classes/Data Wrangling/MUSA-620-Week-9-master/all_crime.csv")

crime <- all_crime


#fortify and join data 
Tracts <- fortify(Tracts, region = "GISJOIN")
Data <- left_join(Tracts, crime, by=c("id" = "GISJOIN"))


#converts data from row to column 
#Data <- mutate(Data, clean1970 = ifelse(is.na(y1970),0,y1970) )
#Data <- mutate(Data, clean2015 = ifelse(is.na(y2015),0,y2015) )


#Data Visualization
map <- ggplot() +
  geom_polygon(data = Data, aes(x = long, y = lat, group = group,
                                    fill = Data$COUNT_PSA), color = "black", size = 0.1) +
  coord_map() # Mercator projection


Data$Buckets <- factor(
  cut(Data$COUNT_PSA, c(-5, 25, 50, 100, 200, 99999)),
  labels = c("Less than 5", "6-25", "26-50", "51-100", "Greater than 101")
)


#create a mapped gif for all years 2006-2016

saveGIF({
  
  for (i in 2006:2016) {
    
    year <- as.character(i)
    year_data <- filter(Data, Year == i)
    
    yearplot <- ggplot(data = Data, aes(x = long, y = lat, group = group,
                                        fill = Data$Buckets) ) +
      # drop shadow: draw another map underneath, shifted and filled in grey
      geom_polygon(aes(x = long + 0.005, y = lat - 0.002), color = "grey50", size = 0.01, fill = "grey50") +
      geom_polygon(color = "grey10", size = 0.01) +
      # set the projection (Mercator in this case)
      coord_map() +
      #set the color scale
      scale_fill_viridis(discrete = TRUE, direction = 1) +
      #scale_fill_brewer(palette="Greens") +
      labs(title = "Philadelphia, 2006",
           subtitle = "Crime Counts by Census Tract",
           caption = "U.S. Census Bureau, 2006 Decennial Census",
           # remove the caption from the legend
           fill = "Crime Counts") +
      #set the plot theme
      theme_void() +
      #theme_bw() +
      theme(text = element_text(size = 8),
            plot.title = element_text(size = 12, face = "bold"),
            panel.background = element_rect(fill = "NA", colour = "#cccccc"),
            plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "in"),
            legend.text = element_text(size = 7),
            legend.position = c(0.8, 0.25))
    
    print(yearplot)
    
  }
  
}, movie.name = ('~/Documents/Classes/Data Wrangling/MUSA-620-Week-9-master/crime_phila.gif'), interval = 0.2, ani.width = 700, ani.height = 600)

