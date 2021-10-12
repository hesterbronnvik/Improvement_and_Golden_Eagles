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
loginStored <- movebankLogin(username="hebronnvik", password="Sapereaude!1")

# Get overview information about a Movebank study. Be sure to check the citation and license terms if not using your own data.
getMovebankStudy(study="LifeTrack Golden Eagle Alps", login=loginStored) # see study-level info

# Load data from a study in Movebank and create a MoveStack object. For more details and options see https://cran.r-project.org/web/packages/move/index.html.
eagle_move <- getMovebankData(study="LifeTrack Golden Eagle Alps", animalName =  "Adamello20 (eobs 7548)", removeDuplicatedTimestamps=T, login=loginStored)
head(eagle_move)


plot(eagle_move[, 1:2], pch = 20, xlab = "", ylab = "", las = 1, type = "l",
     lwd = 0.5, asp = 1)


eagle_df$sl <- c(NA, with(eagle_df, sqrt((head(location_long, -1) - tail(location_long, -1))^2 +
                               (head(location_lat, -1) - tail(location_lat, -1))^2)))
eagle_df$nsd <- with(eagle_df, sqrt((location_long - eagle_df$location_long[1])^2 + (location_lat - eagle_df$location_lat[1])^2))
dat <- prepData(eagle_df[, c("location_long", "location_lat", "sl", "nsd")],coordNames = c("location_long", "location_lat"), type = "UTM")
head(dat)
hist(dat$step, xlim = c(0,0.001), breaks = 50000)
hist(dat$angle, breaks = 50000)
hist(dat$nsd, breaks = 50000)

table(diff(eagle_df$timestamp))
wawotest(dat$step)
wawotest(dat$nsd)

## BCPA
eagle_df_test <- eagle_df
eagle_df <- eagle_df[1:10000,]
eagle_df$timestamp2 <- 1:nrow(eagle_df)
path_char <- bcpa::MakeTrack(eagle_df$location_long, eagle_df$location_lat, eagle_df$timestamp2)
path_char <- bcpa::GetVT(path_char)
path_char$nsd <- eagle_df$nsd[-(1:2)]
# run the bcpa
ws <- WindowSweep(path_char, "Theta", windowsize = 300, progress = T)
plot(ws, type = "flat", clusterwidth = 24 * 7,
     xlab = "time", las = 1, ylab = "turning angle")
eagle_bc <- data.frame(Time=eagle_df$timestamp, X=eagle_df$location_long, Y=eagle_df$location_lat)
PathPlot(eagle_bc,ws, type = "flat",clusterwidth = 24 * 7,
         plotlegend = TRUE, tauwhere = "topleft",
         n.legend = 4, ncol.legend = 2, bty.legend = TRUE)












