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
library(secr)
library(moveHMM)
library(bcpa)


# Create a login object for a user account at movebank.org
loginStored <- movebankLogin(username=)

# Get overview information about a Movebank study. Be sure to check the citation and license terms if not using your own data.
getMovebankStudy(study="LifeTrack Golden Eagle Alps", login=loginStored) # see study-level info

# Load data from a study in Movebank and create a MoveStack object. For more details and options see https://cran.r-project.org/web/packages/move/index.html.
eagle_move <- getMovebankData(study="LifeTrack Golden Eagle Alps", animalName =  "Adamello20 (eobs 7548)", removeDuplicatedTimestamps=T, login=loginStored)
head(eagle_move)

plot(eagle_move[, 1:2], pch = 20, xlab = "", ylab = "", las = 1, type = "l",
     lwd = 0.5, asp = 1)

eagle_move_thin <- eagle_move[timestamps(eagle_move) >= as.POSIXct("2021-03-20 12:10:14") &
                                timestamps(eagle_move) < as.POSIXct("2021-03-20 12:25:14")]
data_sp <- as(eagle_move_thin, "data.frame") # store the data, then create a spatial object for plotting
coordinates(data_sp) <- ~ location_long + location_lat # set coordinates
proj4string(data_sp) <- wgs # set projection
mapView(data_sp, cex = 3, color = rainbow) # plot on a map

eagle_df <- as(eagle_move_thin, "data.frame")



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

eagle_df$sl <- c(NA, with(eagle_df, sqrt((head(location_long, -1) - tail(location_long, -1))^2 +
                                           (head(location_lat, -1) - tail(location_lat, -1))^2)))
eagle_df$nsd <- with(eagle_df, sqrt((location_long - eagle_df$location_long[1])^2 + (location_lat - eagle_df$location_lat[1])^2))
dat <- prepData(eagle_df[, c("location_long", "location_lat", "sl", "nsd")],coordNames = c("location_long", "location_lat"), type = "UTM")
head(dat)
hist(dat$step, breaks = 50)
hist(dat$angle, breaks = 50)
hist(dat$nsd, breaks = 50)

table(diff(eagle_df$timestamp))
wawotest(dat$step)
wawotest(dat$nsd)

## Penalized contrast
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

## BCPA
eagle_df$timestamp2 <- 1:nrow(eagle_df)
path_char <- bcpa::MakeTrack(eagle_df$location_long, eagle_df$location_lat, eagle_df$timestamp2)
path_char <- bcpa::GetVT(path_char)
path_char$nsd <- eagle_df$nsd[-(1:2)]
# run the bcpa
ws <- WindowSweep(path_char, "Theta", windowsize = 10, progress = T)
plot(ws, type = "flat", clusterwidth = 30,
     xlab = "time", las = 1, ylab = "turning angle")
eagle_bc <- data.frame(Time=eagle_df$timestamp, X=eagle_df$location_long, Y=eagle_df$location_lat)
PathPlot(eagle_bc,ws, type = "flat",clusterwidth = 30,
         plotlegend = TRUE, tauwhere = "right",
         n.legend = 4, ncol.legend = 2, bty.legend = TRUE)