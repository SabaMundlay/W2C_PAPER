#if the packages below are not already installed, they can be installed using 
#install.packages('package_name')

#formatting
library(chron) #for formatting timestamps
library(lubridate) #for extracting dates, years, and months
library(dplyr) #for data frame manipulation 
library(sp) #for creating spatial data frames 
library(rgdal) #for reading in shapefiles
library(geosphere) #to calculate distances between two coordinates

#plotting 
library(ggmap) #access google base maps using API key
library(ggplot2) #for visualising spatial data
library(ggvoronoi) #for voronoi map
library(viridis) #for custom color-scales
library(reshape) #for formatting ggplot2() visualisations
library(ggsn) #for producing scalebars
library(raster) #to clip interpolated grid with a polygon

#analysis
library(ggpubr) #for tests of statistical significance
library(rstatix) #for tests of statistical significance
library(fields) #for spatial binning
library(intervals) #for kriging interpolation
library(automap) #for kriging interpolation
library(moments) #for quantifying skew in distribution 
library(gstat) #for kriging interpolation
library(maptools)

#API access 
register_google(key = "") #replace this with your own API key from google
citation("ggmap") #for attribution guidelines

#all data needs to be in the same folder / directory, and the 
#directory needs to be set here for the code to execute properly
setwd() #in quotations (" ") set the directory to wherever the files are
getwd()

#Load RJMN sensor data
setwd("./GDVR/RJMN") #make sure to replace full filepath
rjmn_sens <- do.call("rbind", lapply(list.files(), read.csv, header = TRUE))
#format date
rjmn_sens$Date <- as.Date(rjmn_sens$Date, format = "%d/%m/%y", origin = "1900-01-01")
rjmn_sens$year <- year(rjmn_sens$Date)
rjmn_sens$mon <- month(rjmn_sens$Date)
names(rjmn_sens) <- c("date", "time", "temp", "pH", "pHmv", "orpmv", "ec", "ecabs", "res", "tds", "sal", "sigma",
                      "press", "do", "doppm", "turb", "nippm", "nimv", "amppm", "ammv", "chlarfu", "cdomrfu", 
                      "tryprfu", "chlamg", "cdomppb", "trypppb", "lat", "long", "year", "mon")
rjmn_sens$time <- as.times(rjmn_sens$time)

a <- subset(rjmn_sens, year == 2019)
table(a$date, a$mon)


#convert CDOM RFU to CDOM PPB
rjmn_sens$cdomugl <- ifelse(rjmn_sens$cdomrfu >= 500, ((rjmn_sens$cdomrfu + 1943.5)/27.224), (rjmn_sens$cdomrfu + 17.033)/5.1707)

#EC HOTSPOT MARCH 23, 2019
#subset RJMN sensor data
rjmn_march23 <- subset(rjmn_sens, date == "2019-03-23")

#EC & CDOM Hotpost 19/12/2017
rjmn_191217 <- subset(rjmn_sens, date == "2017-12-19")
summary(rjmn_191217$cdomugl)

#there are five steps involved in making the our plots, repeated throughout the document
#first, get basemap + add it to a ggmap() instance
#second, add scalebar
#third, add data (either geom_point for 1D for stat_summary_2d for binned data)
#fourth, add the color scale with breaks and limits relavant to the parameter being visualized
#and fifth, formatting the legend

map <- get_map(location = c(lon = 81.762, lat = 17.031), source = "google", zoom = 14, maptype = "satellite")
ggmap(map)
ggmap(map) +
  #scalebar
  ggsn::scalebar(x.min = 81.74, x.max = 81.78,
                 y.min = 17.01, y.max = 17.05, 
                 dist = 0.5, dist_unit = "km", transform = TRUE, model = 'WGS84',
                 box.fill = "white",
                 box.color = "#78909c",
                 st.color = "white",
                 location = "bottomright",
                 st.size = 5)+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  geom_point(x = 81.768, y = 17.029, size = 3, color = "white")


#get basemap
map <- get_map(location = c(lon = 81.77, lat = 17.02), source = "google", zoom = 15, maptype = "satellite")
ggmap(map)
ggmap(map) +
  #scalebar
  ggsn::scalebar(x.min = 81.76, x.max = 81.78,
                 y.min = 17.010, y.max = 17.030, 
                 dist = 0.5, dist_unit = "km", transform = TRUE, model = 'WGS84',
                 box.fill = "white",
                 box.color = "#78909c",
                 st.color = "white",
                 location = "bottomright",
                 st.size = 8) +
  #plot sensor data
  geom_point(data = rjmn_191217, aes(x = long, y = lat, color = ec, stroke = 0), size = 4) +
  #add color scale
  scale_color_viridis(name = "", limits = c(150,500), breaks = c(150,200,250,300,350,400,450,500), option = "plasma", begin = 0.2) +
  xlab("") + ylab("") +
  #format legend
  theme(legend.direction = "horizontal",
        legend.position = "bottom",
        legend.key.width = unit(4,"cm"),
        legend.key.height = unit(0.5, "cm"),
        legend.margin = margin(10,0,0,0), #top, right, bottom, left
        axis.line = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Arial"),
        legend.text = element_text(size = 20),
        legend.justification = "left",
        plot.background = element_rect(fill = "transparent", color = NA)) +
  guides(size = FALSE) +
  #add paper mill point
  geom_point(x = 81.768, y = 17.029, size = 3, color = "white")


#CDOM HOTSPOT 29/12/2017 & 19/12/2017
rjmn_dec29 <- subset(rjmn_sens, date == "2017-12-29")
summary(rjmn_dec29$cdomppb)

#get basemap
map <- get_map(location = c(lon = 81.76, lat = 17.02), source = "google", zoom = 14, maptype = "satellite")
ggmap(map) +
  #scalebar
  ggsn::scalebar(x.min = 81.76, x.max = 81.78,
                 y.min = 17.010, y.max = 17.030, 
                 dist = 0.5, dist_unit = "km", transform = TRUE, model = 'WGS84',
                 box.fill = "white",
                 box.color = "#78909c",
                 st.color = "white",
                 location = "bottomright",
                 st.size = 8) +
  #add sensor data
  geom_point(data = rjmn_191217, aes(x = long, y = lat, color = cdomugl, size = 1, stroke = 0)) +
  #add color scale
  scale_color_viridis(name = "", limits = c(0,140), breaks = c(0,20,40,60,80,100,120,140), option = "plasma", begin = 0.2) +
  xlab("") + ylab("") +
  #format legend
  theme(legend.direction = "horizontal",
        legend.position = "bottom",
        legend.key.width = unit(4,"cm"),
        legend.key.height = unit(0.5, "cm"),
        legend.margin = margin(10,0,0,0), #top, right, bottom, left
        axis.line = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Arial"),
        legend.text = element_text(size = 20),
        legend.justification = "left",
        plot.background = element_rect(fill = "transparent", color = NA)) +
  guides(size = FALSE) +
  #Paper Mill Point
  geom_point(x = 81.768, y = 17.029, size = 3, color = "white")

#RJMN ZONE OF INFLUENCE
#create temporary dataframe from the RJMN sensor data
rjmn_pm <- rjmn_sens
#add distance from specific lat / lon point
rjmn_pm <- mutate(rjmn_pm, dist = distHaversine(cbind(long,lat), cbind(81.768,17.029)))
#convert from m to KM 
rjmn_pm$dist <- rjmn_pm$dist/1000
#create "negative" distance for upstream values
rjmn_pm$dist <- ifelse(rjmn_pm$lat <= 17.029, rjmn_pm$dist, rjmn_pm$dist*-1)
#remove points further than 4km away
rjmn_pm <- subset(rjmn_pm, (dist <= 4 & dist >= -4)) #81,047 observations 


#plot EC as distance from X,Y coordinate
ggplot(data = rjmn_pm) +
  geom_point(mapping = aes(x = dist, y = ec)) +
  scale_x_continuous(name = "", limits = c(-4,4), breaks = c(-4,-3,-2,-1,0,1,2,3,4), labels = c(4,3,2,1,0,1,2,3,4)) +
  scale_y_continuous(name = "", limits = c(0,1750), breaks = c(0,200,400,600,800,1000,1200,1400,1600,1800)) +
  theme_bw() +
  theme(axis.text = element_text(size = 22, color = "black")) +
  geom_vline(xintercept = 1.5, linetype = "dashed", color = "red")

#plot CDOM ugl as distance from X,Y coordinate
ggplot(data = rjmn_pm) +
  geom_point(mapping = aes(x = dist, y = cdomugl)) +
  scale_x_continuous(name = "", limits = c(-4,4), breaks = c(-4,-3,-2,-1,0,1,2,3,4), labels = c(4,3,2,1,0,1,2,3,4)) +
  scale_y_continuous(name = "", limits = c(0,200), breaks = c(0,25,50,75,100,125,150,175,200)) +
  theme_bw() +
  theme(axis.text = element_text(size = 22, color = "black")) +
  geom_vline(xintercept = 1.5, linetype = "dashed", color = "red")


#load VRNS sensor data
setwd("./GNGA/VRNS") #make sure to replace ... with correct, full filepath
vrns_sens <- do.call("rbind", lapply(list.files(), read.csv, header = TRUE))
#format date
vrns_sens$Date <- as.Date(vrns_sens$Date, format = "%d/%m/%y", origin = "1900-01-01")
vrns_sens$Time <- as.times(vrns_sens$Time)
vrns_sens$year <- year(vrns_sens$Date)
vrns_sens$mon <- month(vrns_sens$Date)
names(vrns_sens) <- c("date", "time", "temp", "pH", "pHmv", "orpmv", "ec", "ecabs", "res", "tds", "sal", "sigma",
                      "press", "do", "doppm", "turb", "nippm", "nimv", "amppm", "ammv", "chlarfu", "cdomrfu", 
                      "tryprfu", "chlamg", "cdomppb", "trypppb", "lat", "lon", "year", "mon")

vrns_sens$lat <- as.numeric(as.character(vrns_sens$lat))
vrns_sens$lon <- as.numeric(as.character(vrns_sens$lon))

#SPATIAL BINNING TURBIDITY IN VARANASI
map <- get_map(location = c(lon = 83.02, lat = 25.3), source = "google", zoom = 14, maptype = "satellite")
ggmap(map)

ggmap(map) +
  #scalebar
  ggsn::scalebar(x.min = 83, x.max = 83.04,
                 y.min = 25.28, y.max = 25.32, 
                 dist = 0.5, dist_unit = "km", transform = TRUE, model = 'WGS84',
                 box.fill = "white",
                 box.color = "#78909c",
                 st.color = "white",
                 location = "bottomright",
                 st.size = 8) +
  #stat summary 2D creates the spatial binning
  stat_summary_2d(data = vrns_sens, aes(x = lon, y = lat, z = turb), fun = median, bins = 75) +
  #set color scale
  scale_fill_viridis(name = "", limits = c(0,420), breaks = c(0,60,120,180,240,300,360,420), option = "plasma", begin = 0.2) +
  labs(x = "", y = "", title = "") +
  #format legend
  theme(legend.direction = "horizontal",
        legend.position = "bottom",
        legend.key.width = unit(4,"cm"),
        legend.key.height = unit(0.5, "cm"),
        legend.margin = margin(10,0,0,0), #top, right, bottom, left
        axis.line = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Arial"),
        legend.text = element_text(size = 20),
        legend.justification = "left",
        plot.background = element_rect(fill = "transparent", color = NA)) +
  guides(size = FALSE)


#load YMNA sensor data
getwd()
setwd("./YMNA/DELH")
ymna_delh <- do.call("rbind", lapply(list.files(), read.csv, header = TRUE))

#format date, add year, month, season, daytime, and segment variables
ymna_delh$Date <- as.Date(ymna_delh$Date, format = "%d/%m/%y", origin = "1900-01-01")
ymna_delh$year <- year(ymna_delh$Date)
ymna_delh$mon <- month(ymna_delh$Date)
names(ymna_delh) <- c("date", "time", "temp", "pH", "pHmv", "orpmv", "ec", "ecabs", "res", "tds", "sal", "sigma",
                      "press", "do", "doppm", "turb", "nippm", "nimv", "amppm", "ammv", "chlarfu", "cdomrfu", 
                      "tryprfu", "chlamg", "cdomppb", "trypppb", "lat", "lon", "year", "mon")
ymna_delh$time <- as.times(ymna_delh$time)
ymna_delh <- ymna_delh %>% 
  mutate(segment = case_when(
    .$lat >= 28.70433 ~ "near_barrage",
    .$lat < 28.70733 & .$lat >= 28.67174 ~ "sig_yud",
    .$lat < 28.67174 & .$lat >= 28.66218 ~ "yud_iron",
    .$lat < 28.66218 & .$lat >= 28.62812 ~ "iron_ito",
    .$lat < 28.62812 & .$lat >= 28.59991 ~ "ito_niz",
    .$lat < 28.59991 ~ "niz_okhla",
    TRUE
    ~ "no_defined_segment"
  ))

#DO Change 2018 - 2020
#dec. 2018 - feb 2019
delh_sens_a <- subset(ymna_delh, (date >= "2018-12-06" & date <= "2019-02-17"))
summary(delh_sens_a$lat) #median 28.67
summary(delh_sens_a$lon) #median 77.24

#dec. 2019 - feb 2020
delh_sens_b <- subset(ymna_delh, (date >= "2019-12-10" & date <= "2020-02-13"))
summary(delh_sens_b$lat) #median 28.66
summary(delh_sens_b$lon) #median 77.24

map <- get_map(location = c(lon = 77.23, lat = 28.685), source = "google", zoom = 14, maptype = "satellite")
ggmap(map)

ggmap(map) + 
  #scalebar
  ggsn::scalebar(x.min = 77.21, x.max = 77.25,
                 y.min = 28.67, y.max = 28.7, 
                 dist = 0.5, dist_unit = "km", transform = TRUE, model = 'WGS84',
                 box.fill = "white",
                 box.color = "#78909c",
                 st.color = "white",
                 location = "bottomright",
                 st.size = 6) +
  #add sensor data
  stat_summary_2d(data = delh_sens_b, aes(x = lon, y = lat, z = doppm), fun = median, bins = 75) +
  #set color scale
  scale_fill_viridis(limits = c(0,7), breaks = c(0,1,2,3,4,5,6,7), name = "", option = "plasma", begin = 0.2, direction = -1) + 
  labs(x = "", y = "", title = "") +
  #format legend
  theme(legend.direction = "horizontal",
        legend.position = "bottom",
        legend.key.width = unit(3.8,"cm"),
        legend.margin = margin(10,0,0,0), #top, right, bottom, left
        legend.justification = "left",
        axis.line = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        text = element_text(family = "Arial"),
        legend.text = element_text(size = 20)) +
  guides(size = FALSE)


#DO Hotspots Aug / Sept 2019
delh_aug <- subset(ymna_delh, date == "2019-08-03")
delh_sept <- subset(ymna_delh, date == "2019-09-23")

map <- get_map(location = c(lon = 77.23, lat = 28.7), source = "google", zoom = 15, maptype = "satellite")
ggmap(map)

summary(delh_aug$doppm) #1.7 - 1.98
summary(delh_sept$doppm) #0.05 - 2.7

ggmap(map) +
  #scalebar
  ggsn::scalebar(x.min = 77.22, x.max = 77.24,
                 y.min = 28.69, y.max = 28.71, 
                 dist = 0.5, dist_unit = "km", transform = TRUE, model = 'WGS84',
                 box.fill = "white",
                 box.color = "#4d4d4d",
                 st.color = "white",
                 location = "topright",
                 st.size = 7) +
  #add sensor data
  geom_point(data = delh_sept, aes(x = lon, y = lat, color = doppm, size = 5)) +
  #set color scale
  scale_color_viridis(limits = c(0,3), breaks = c(0,0.5,1,1.5,2,2.5,3), name = "", 
                      option = "plasma", begin = 0.2, direction = -1) +
  xlab("") + ylab("") +
  #format legend
  theme(legend.direction = "horizontal",
        legend.position = "bottom",
        legend.key.width = unit(1.5,"cm"),
        legend.margin = margin(9,9,3,3), #top, right, bottom, left
        axis.line = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        text = element_text(family = "Arial"),
        legend.text = element_text(size = 14)) +
  guides(size = FALSE)


#DO Hotspots July 2017
ymna_do_jul6 <- subset(ymna_delh, date == "2017-07-06")
summary(ymna_do_jul6$doppm) #0.62 - 5.56
ymna_do_jul10 <- subset(ymna_delh, date == "2017-07-10")
summary(ymna_do_jul10$doppm) #1.67 - 5.82
ymna_do_jul11 <- subset(ymna_delh, date == "2017-07-11")
summary(ymna_do_jul11$doppm) #1.82 - 3.77

#1D Maps
map <- get_map(location = c(lon = 77.23, lat = 28.69), source = "google", zoom = 14, maptype = "satellite")
ggmap(map) #77.21-77.25 & 28.67 - 28.71

ggmap(map) +
  #scalebar
  ggsn::scalebar(x.min = 77.21, x.max = 77.25,
                 y.min = 28.67, y.max = 28.71, 
                 dist = 0.5, dist_unit = "km", transform = TRUE, model = 'WGS84',
                 box.fill = "white",
                 box.color = "#4d4d4d",
                 st.color = "white",
                 location = "bottomright",
                 st.size = 7) +
  #add sensor data
  geom_point(data = ymna_do_jul11, aes(x = lon, y = lat, color = doppm), size = 2.5) +
  #set color scale
  scale_color_viridis(limits = c(0,5.5), breaks = c(0,0.5,1,1.5,2,2.5,3, 3.5,4,4.5,5,5.5), name = "", 
                      option = "plasma", begin = 0.2, direction = -1) +
  xlab("") + ylab("") +
  #format legend
  theme(legend.direction = "horizontal",
        legend.position = "bottom",
        legend.key.width = unit(4,"cm"),
        legend.margin = margin(9,9,3,3), #top, right, bottom, left
        legend.justification = "left",
        axis.line = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        text = element_text(family = "Arial"),
        legend.text = element_text(size = 20)) +
  guides(size = FALSE)


#load NRSP data
setwd("./GDVR/NRSP") #make sure to replacewith correct, full filepath
nrsp_sens <- do.call("rbind", lapply(list.files(), read.csv, header = TRUE))
nrsp_sens$Date <- as.Date(nrsp_sens$Date, format = "%d/%m/%y", origin = "1900-01-01")
nrsp_sens$Time <- format(strptime(nrsp_sens$Time, "%H:%M:%S"),"%H:%M:%S")
nrsp_sens$year <- year(nrsp_sens$Date)
nrsp_sens$mon <- month(nrsp_sens$Date)
names(nrsp_sens) <- c("date", "time", "temp", "pH", "pHmv", "orpmv", "ec", "ecabs", "res", "tds", "sal", "sigma",
                      "press", "do", "doppm", "turb", "nippm", "nimv", "amppm", "ammv", "chlarfu", "cdomrfu", 
                      "tryprfu", "chlamg", "cdomppb", "trypppb", "lat", "lon", "year", "mon")


#subset narsapur to get only Aug 24, 2017 MORNING
nrsp_aug <- subset(nrsp_sens, ((date == "2017-08-24") & (time <= "12:00:00")))


#plot 1D EC data
map <- get_map(location = c(lon = 81.715, lat = 16.435), source = "google", zoom = 15, maptype = "satellite")
ggmap(map)
ggmap(map) +
  #scalebar
  ggsn::scalebar(x.min = 81.69, x.max = 81.73,
                 y.min = 16.41, y.max = 16.45,
                 dist = 0.5, dist_unit = "km", transform = TRUE, model = 'WGS84',
                 box.fill = "white",
                 box.color = "#78909c",
                 st.color = "white",
                 location = "bottomright",
                 st.size = 6) +
  #add sensor data
  geom_point(data = nrsp_aug, aes(x = lon, y = lat, color = ec), size = 3) +
  #set color scale
  scale_color_viridis(limits = c(0,960), breaks = c(0,120,240,360,480,600,720,840,960), name = "", 
                      option = "plasma", begin = 0.2) +
  xlab("") + ylab("") +
  theme(legend.direction = "horizontal",
        legend.position = "bottom",
        legend.key.width = unit(3.7,"cm"),
        legend.margin = margin(10,0,0,0), #top, right, bottom, left
        legend.justification = "left",
        axis.line = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        text = element_text(family = "Arial"),
        legend.text = element_text(size = 20)) +
  guides(size = FALSE)


#Plot 1D Nitrate Data
names(nrsp_aug)
summary(nrsp_aug$nippm)
nrsp_aug_nitrate <- subset(nrsp_aug, !is.na(nippm))

ggmap(map) +
  #scalebar
  ggsn::scalebar(x.min = 81.69, x.max = 81.73,
                 y.min = 16.41, y.max = 16.45,
                 dist = 0.5, dist_unit = "km", transform = TRUE, model = 'WGS84',
                 box.fill = "white",
                 box.color = "#78909c",
                 st.color = "white",
                 location = "bottomright",
                 st.size = 6) +
  #add sensor data
  geom_point(data = nrsp_aug_nitrate, na.rm = TRUE, aes(x = lon, y = lat, color = nippm), size = 3) +
  #set color scale
  scale_color_viridis(limits = c(0,3), breaks = c(0,0.5,1,1.5,2,2.5,3), name = "", 
                      option = "plasma", begin = 0.2) +
  xlab("") + ylab("") +
  theme(legend.direction = "horizontal",
        legend.position = "bottom",
        legend.key.width = unit(4,"cm"),
        legend.margin = margin(10,0,0,0), #top, right, bottom, left
        legend.justification = "left",
        axis.line = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        text = element_text(family = "Arial"),
        legend.text = element_text(size = 20)) +
  guides(size = FALSE)


#2D Interpolation (Kriging) for NRSP EC 24/08/2017

#use dataset created earlier nrsp_aug, which has morning data from 24/08/2017
#make one DF with EC only (ec, lat, lon), the autokrige() function assumes that the interprolation is for the first column of data
nrsp_ec_interpolation <- nrsp_aug[,c("ec", "lat", "lon")]

summary(nrsp_ec_interpolation) #helps us see the min / max EC values. 
temp <- subset(nrsp_ec_interpolation, ec == 0) #there are 4 locations where ec == 0
temp <- subset(nrsp_aug, ec == 0) #from 8:19AM - 8:22AM on 24/08/2017
#remove these observations as they are likely sensor errors
nrsp_ec_interpolation <- subset(nrsp_ec_interpolation, (ec != 0 & ec != 34))


#make DF with Nitrate only (nippm, lat, lon)
nrsp_ni_interpolation <- nrsp_aug[,c("nippm", "lat", "lon")]
summary(nrsp_ni_interpolation$nippm) #need to remove NAs
nrsp_ni_interpolation <- nrsp_ni_interpolation[complete.cases(nrsp_ni_interpolation), ]

#check assumptions for kriging
#1. is the data normally distributed? kriging estimator will be sensitive to larger values due to skew
hist(nrsp_ec_interpolation$ec) #skewed right
skewness(nrsp_ec_interpolation$ec, na.rm = TRUE) #2.0507765 (highly skewed)
hist(log10(nrsp_ec_interpolation$ec))
temp <- log10(nrsp_ec_interpolation) #check skewness on log10() values of EC
temp <- temp[!is.infinite(rowSums(temp)),] #there are -Inf values when using log10()
skewness(log10(temp$ec), na.rm = TRUE) #1.288434 (still highly skewed but less so)
#inverse and square-root transformations yield higher skewness values than log10()


hist(nrsp_ni_interpolation$nippm) #skewed right
skewness(nrsp_ni_interpolation$nippm, na.rm = TRUE) #2.060254
hist(log10(nrsp_ni_interpolation$nippm)) #relatively normal looking
skewness(log10(nrsp_ni_interpolation$nippm), na.rm = TRUE) #1.442483 (still highly skewed)

#2. is the data stationary? Using a Voronoi map, we can make sure that local variation is not extreme
#voronoi map requires the removal of duplicate (x,y) coordinates
temp <- distinct(nrsp_ec_interpolation,lon,lat, .keep_all= TRUE)
ggplot(temp) +
  geom_voronoi(aes(x = lon, y = lat, fill = ec))

temp <- distinct(nrsp_ni_interpolation,lon,lat, .keep_all= TRUE) 
ggplot(temp) +
  geom_voronoi(aes(x = lon, y = lat, fill = nippm))

#3. is the data trend-less ? 
#using our 1D maps, we can see that there is no strong east-west or north-south trend

#input data needs to be SpatialPointsDataFrame with projected coordinates
coords <- nrsp_ec_interpolation[,c(3,2)] #store lat & lon coordinates (lon is x, and lat is y)
nrsp_spdf <- SpatialPointsDataFrame(coords = coords, data = nrsp_ec_interpolation, proj4string = CRS("+init=epsg:7755")) #this is the EPSG projection for India
coordinates(nrsp_spdf)


coords <- nrsp_ni_interpolation[,c(3,2)] #store lat & lon coordinates (lon is x, and lat is y)
nrsp_spdf <- SpatialPointsDataFrame(coords = coords, data = nrsp_ni_interpolation, proj4string = CRS("+init=epsg:7755")) #this is the EPSG projection for India


#do the same code ^ for Nitrate

#make a rectangular grid from the input data
nrsp_grid <- spsample(nrsp_spdf, type = "regular", cellsize = c(0.001,0.001)) #cellsize determines resolution
gridded(nrsp_grid) = TRUE

#kriging interpolation
krig <- autoKrige(nrsp_spdf, nrsp_grid) #interpolation
summary(krig$krige_output)
t <- krig$krige_output #tells us the predicted parameter, the variance, and the stdev
t <- as.data.frame(t)
summary(t) 

#remove duplicates before cross-validation
nrsp_spdf_cv = nrsp_spdf[which(!duplicated(nrsp_spdf@coords)), ]
krig_cv <- autoKrige.cv(ec~1,nrsp_spdf_cv) #cross-validation to see how "well" the prediction model performs (Ste)
t_cv <- as.data.frame(krig_cv$krige.cv_output) #look at the residuals
#RMSE has to be looked at in conjuction with the range. Ideally we want an SI (RMSE/MEAN) of <1
rmse(t_cv$observed, t_cv$residual) / mean(nrsp_ec_interpolation$ec) #1.16 when EC = 0 is removed

#load polygon shapefile 
#"/GDVR/nrsp_poly" is the working directory
nrsp_poly <- readOGR(dsn = ".", layer = "POLYGON")


#rasterize the interpolated output & the polygon shapefile of NRSP
krig_raster <- raster(krig$krige_output)
nrsp_raster <- raster(nrsp_poly)
clipped_raster <- mask(krig_raster, nrsp_poly) #clip the interpolated area
k <- rasterToPoints(clipped_raster) #convert back into spatialPoints

#plot with ggmap() + geom_tile() accepts only data frames
k <- as.data.frame(k)
names(k) <- c("lon", "lat", "pred")
summary(k)

#ZOOMED IN 2D MAP 
#map <- get_map(location = c(lon = 81.705, lat = 16.429), source = "google", zoom = 16, maptype = "satellite")
#ggmap(map)

map <- get_map(location = c(lon = 81.71, lat = 16.435), source = "google", zoom = 14, maptype = "satellite")
ggmap(map)

ggmap(map) +
  #scalebar
  ggsn::scalebar(x.min = 81.69, x.max = 81.73,
                 y.min = 16.42, y.max = 16.46,
                 dist = 0.5, dist_unit = "km", transform = TRUE, model = 'WGS84',
                 box.fill = "white",
                 box.color = "#78909c",
                 st.color = "white",
                 #location = "bottom",
                 st.size = 6) +
  #add interpolated data
  geom_tile(data = k, aes(x = lon, y = lat, fill = pred)) + 
  #scale_fill_viridis(limits = c(0,960), breaks = c(0,120,240,360,480,600,720,840,960), name = "", 
  #                   option = "plasma", begin = 0.2) + #make sure to edit the scaale here
  scale_fill_viridis(limits = c(0,3), breaks = c(0,0.5,1,1.5,2,2.5,3), name = "", 
                      option = "plasma", begin = 0.2) +
  xlab("") + ylab("") +
  theme(legend.direction = "horizontal",
        legend.position = "bottom",
        legend.key.width = unit(4,"cm"),
        legend.margin = margin(10,0,0,0), #top, right, bottom, left
        legend.justification = "left",
        axis.line = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        text = element_text(family = "Arial"),
        legend.text = element_text(size = 20)) +
  guides(size = FALSE)

#2D Interpolation (Kriging) for YMNA DO JULY 2017 

#extract relevant dates & parameters
summary(ymna_delh$date)
ymna_do_jul6 <- subset(ymna_delh, date == "2017-07-06")
ymna_do_jul6 <- ymna_do_jul6[,c("doppm", "lat", "lon")]

ymna_do_jul10 <- subset(ymna_delh, date == "2017-07-10")
ymna_do_jul10 <- ymna_do_jul10[,c("doppm", "lat", "lon")]

ymna_do_jul11 <- subset(ymna_delh, date == "2017-07-11")
ymna_do_jul11 <- ymna_do_jul11[,c("doppm", "lat", "lon")]

#convert DFs to SPDF objects with +init=epsg:7755 projection
coords_a <- ymna_do_jul6[,c(3,2)] #store lat & lon coordinates (lon is x, and lat is y)
ymna_spdf_a <- SpatialPointsDataFrame(coords = coords_a, data = ymna_do_jul6, proj4string = CRS("+init=epsg:7755"))
extent(ymna_spdf_a)

coords_b <- ymna_do_jul10[,c(3,2)] #store lat & lon coordinates (lon is x, and lat is y)
ymna_spdf_b <- SpatialPointsDataFrame(coords = coords_b, data = ymna_do_jul10, proj4string = CRS("+init=epsg:7755"))
extent(ymna_spdf_b)

coords_c <- ymna_do_jul11[,c(3,2)] #store lat & lon coordinates (lon is x, and lat is y)
ymna_spdf_c <- SpatialPointsDataFrame(coords = coords_c, data = ymna_do_jul11, proj4string = CRS("+init=epsg:7755"))
extent(ymna_spdf_c)

#use the same prediction grid for all 
ymna_grid <- spsample(ymna_spdf_c, type = "regular", cellsize = c(0.001,0.001)) #cellsize determines resolution
gridded(ymna_grid) = TRUE
plot(ymna_grid)

#run interpolation
jul6_krig <- autoKrige(ymna_spdf_a, ymna_grid)
jul10_krig <- autoKrige(ymna_spdf_b, ymna_grid)
jul11_krig <- autoKrige(ymna_spdf_c, ymna_grid)

#load ymna polygon shapefile
ymna_poly <- readOGR(dsn = "./YMNA/ymna_polygon_shp", layer = "POLYGON")

#rasterize & clip interpolation results (for each date)
ymna_krig_raster <- raster(jul11_krig$krige_output)
ymna_poly_raster <- raster(ymna_poly)
clipped_raster <- mask(ymna_krig_raster, ymna_poly) #clip the interpolated area
y <- rasterToPoints(clipped_raster) #convert back into spatialPoints


#plot with ggmap() + geom_tile() accepts only data frames
y <- as.data.frame(y)
names(y) <- c("lon", "lat", "do_pred")

map <- get_map(location = c(lon = 77.23, lat = 28.695), source = "google", zoom = 14, maptype = "satellite")
ggmap(map)

ggmap(map) +
  #scalebar
  ggsn::scalebar(x.min = 77.21, x.max = 77.25,
                 y.min = 28.68, y.max = 28.71,
                 dist = 0.5, dist_unit = "km", transform = TRUE, model = 'WGS84',
                 box.fill = "white",
                 box.color = "#78909c",
                 st.color = "white",
                 location = "bottomright",
                 st.size = 6) +
  #add interpolated data
  geom_tile(data = y, aes(x = lon, y = lat, fill = do_pred)) + 
  scale_fill_viridis(limits = c(0,5.5), breaks = c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5), name = "", 
                     option = "plasma", begin = 0.2, direction = -1) + #make sure to edit the scaale here
xlab("") + ylab("") +
  theme(legend.direction = "horizontal",
        legend.position = "bottom",
        legend.key.width = unit(4,"cm"),
        legend.margin = margin(10,0,0,0), #top, right, bottom, left
        legend.justification = "left",
        axis.line = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        text = element_text(family = "Arial"),
        legend.text = element_text(size = 20)) +
  guides(size = FALSE)


