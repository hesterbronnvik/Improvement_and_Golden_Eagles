# Exploring the Golden Eagle data
# Fall 2021

# Load libraries
library(knitr)
library(lubridate)
library(maptools)
library(raster)
library(move)
library(mapview)
library(ggmap)
library(tibble)
library(leaflet)
library(adehabitatLT)
library(dplyr)


# Create a login object for a user account at movebank.org
loginStored <- movebankLogin(username=)

# Get overview information about a Movebank study. Be sure to check the citation and license terms if not using your own data.
getMovebankStudy(study="LifeTrack Golden Eagle Alps", login=loginStored) # see study-level info

# Load data from a study in Movebank and create a MoveStack object. For more details and options see https://cran.r-project.org/web/packages/move/index.html.
eagle_move <- getMovebankData(study="LifeTrack Golden Eagle Alps", animalName =  "Adamello20 (eobs 7548)", removeDuplicatedTimestamps=T, login=loginStored)
head(eagle_move)

#create an empty data frame to store results in
eagle_move <- data.frame()
# retrieve information about the individuals in the study
eagle_info <- getMovebankAnimals(study="LifeTrack Golden Eagle Alps", login=loginStored)
# create a vector of unique identifiers for each animal
eagle_names <- unique(eagle_info$local_identifier)
# loop through each individual in the eagle_names vector, retrieving its data and storing them
for (i in eagle_names) {
  move_temp <- getMovebankData(study="LifeTrack Golden Eagle Alps", animalName =  i, removeDuplicatedTimestamps=T, login=loginStored)
  df_temp <- as(move_temp, "data.frame")
  eagle_move <- rbind(df_temp, eagle_move)
}


# Create a data frame from the MoveStack object
eagle_df <- as(eagle_move, "data.frame")

# Data cleaning

# Delete observations missing lat or long or a timestamp.
ind<-complete.cases(eagle_df[,c("location_lat", "location_long", "timestamp")])
eagle_df<-eagle_df[ind==TRUE,]

# Check for duplicated observations (ones with same lat, long, timestamp,
# and individual identifier). 
ind2<-eagle_df %>% select(timestamp, location_long, location_lat, local_identifier) %>%
  duplicated
sum(ind2) # no duplicates
eagle_df<-eagle_df[ind2!=TRUE,]

rm(ind)
rm(ind2)


# Make timestamp a date/time variable
eagle_df$timestamp<-as.POSIXct(eagle_df$timestamp, format="%Y-%m-%d %H:%M:%OS", tz="UTC")

# Look at functions in the move package.
plot(eagle_move)
show(eagle_move)
summary(eagle_move)

# Plots of the data

# plot the track on the map
wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs")
mv <- move(x = eagle_df$location_long, y = eagle_df$location_lat, time = eagle_df$timestamp, data = eagle_df, animal = eagle_df$local_identifier, proj = wgs)
maps::map("world",xlim = c(5,15), ylim = c(44,47))
points(mv, cex = 0.3, pch = 16)

# Now, using leaflet
leaflet(eagle_df) %>% addTiles() %>%
  addCircles(eagle_df$location_long, eagle_df$location_lat)

eagle_move_thin <- eagle_move[timestamps(eagle_move) >= as.POSIXct("2021-04-08 12:44:13") &
                                timestamps(eagle_move) < as.POSIXct("2021-04-08 12:47:47")]
data_sp <- as(eagle_move_thin, "data.frame") # store the data, then create a spatial object for plotting
coordinates(data_sp) <- ~ location_long + location_lat # set coordinates
proj4string(data_sp) <- wgs # set projection
mapView(data_sp, cex = 3, color = rainbow) # plot on a map

# Using ggplot without a background
ggplot(eagle_df, aes(x=location_long, y=location_lat))+geom_point()+
  facet_wrap(~local_identifier, scales="free")
ggplot(eagle_df, aes(x=location_long, y=location_lat, color=as.factor(local_identifier)))+
  geom_point() 

## penalized contrast
eagle_move_thin <- eagle_move[timestamps(eagle_move) >= as.POSIXct("2021-08-23 12:00:00") &
                              timestamps(eagle_move) < as.POSIXct("2021-09-06 12:00:00")]
angle <- turnAngleGc(eagle_move_thin)
change_point <- lavielle(angle, Lmin = 1, Kmax = 3360, type = "var")
head(chooseseg(change_point))

fp <- findpath(change_point, 8)

par(mfrow = c(4,2))
par(mar = c(2,0,0,0))
for (i in 1:8) {
  plot(eagle_move_thin, type = "change_point")
  lines(eagle_move_thin[fp[[i]][1]:fp[[i]][2]], col = "black", lwd = 2)
  legend("topleft", paste("Segment #", i, sep = " "), lty = c(1), lwd = 2, bty = "n")
}

#table(diff(eagle_df$timestamp))

library(bcpa)
eagle_df_test <- as(eagle_move_thin, "data.frame")
eagle_df_test$Time <- 1:nrow(eagle_df_test)
path_char <- bcpa::MakeTrack(eagle_df_test$location_long, eagle_df_test$location_lat, eagle_df_test$Time)
path_char <- bcpa::GetVT(path_char)
# run the bcpa
ws <- WindowSweep(path_char, "V*cos(Theta)", windowsize = 20, progress = FALSE)
plot(ws, type = "flat", clusterwidth = 200,
     xlab = "time", las = 1, ylab = "V*cos(Theta)")

# add trajectory plot
xy_bc <- data.frame(Time = eagle_df_test$Time, X = eagle_df_test$location_long, Y = eagle_df_test$location_lat)
PathPlot(xy_bc,ws, type = "flat", clusterwidth = 20,
         plotlegend = TRUE, tauwhere = "right",
         n.legend = 4, ncol.legend = 2, bty.legend = TRUE)
mtext("A single individual for 12 hours path plotted with BCPA window size 200", side=3)
















