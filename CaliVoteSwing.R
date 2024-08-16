#necessary R packages
if(!require("leaflet")) install.packages("leaflet")
if(!require("dplyr")) install.packages("dplyr")
if(!require("htmltools")) install.packages("htmltools")
if(!require("htmlwidgets")) install.packages("htmlwidgets")
if(!require("sf")) install.packages("sf")

library(leaflet)
library(dplyr)
library(htmltools)
library(htmlwidgets) 
library(sf)

#datasetup
california <- sf::st_read("./viz_20110728_q2_cd_finaldraft_shp") %>% st_transform("+proj=longlat +datum=WGS84")
cal_district_num <- 01:53
dem_vote_18 <- c(45.11,77.01,58.07,45.87,78.87,100.00,55.04,0.00,56.49,52.25,74.13,86.82,88.38,79.22,72.97,57.55,75.35,74.49,73.75,81.37,50.38,47.28,36.28,58.56,54.37,61.94,100.00,78.37,80.61,73.40,58.74,68.78,70.03,72.54,69.40,59.02,89.08,68.85,51.56,77.35,65.10,43.50,77.67,100.00,52.05,69.15,64.86,53.55,56.42,48.28,71.20,63.85,69.07)
dem_vote_20 <- c(43.01, 75.74,54.67,44.05,76.09,73.34,56.62,43.94,57.58,55.16,72.99,100.00,90.37, 79.29,70.90,59.38,71.35,100.00,71.68,76.78,49.55,45.77,37.86,58.66,49.95,60.58,69.78,72.67,100.00,69.48,61.29,66.58,67.58,100.00,69.33,60.34,85.94,100.00,49.40,72.74,64.04,42.87,71.68,100.00,53.46,68.75,63.27,48.94,53.13,46.05,68.30,61.58,100.00)
votetable <- data.frame(cal_district_num,dem_vote_18,dem_vote_20)
votetable$swing <- votetable$dem_vote_20 - votetable$dem_vote_18

#districts with swings of NA were uncontested by one major party in one or both elections
votetable$swing[votetable$swing >= 10] <- NA
votetable$swing[votetable$swing <= -20] <- NA
votetable$swing[votetable$swing ==  0] <- NA

calivoteswing <- arrange(california,DISTRICT)
calivoteswing$SWING <- votetable$swing
newpalette <- colorNumeric(palette = "RdBu", domain = NULL)
githubtip <- tags$div(
  HTML("R code and shapefiles <a href = 'https://github.com/nish20e/California-SHP/tree/main'> on Github </a> ")
)  

labels <- sprintf(
  "<strong> District %s</strong><br/>%g",
  calivoteswing$DISTRICT, calivoteswing$SWING
) %>% lapply(htmltools::HTML)

calivotemap <- leaflet(calivoteswing) |>
  addProviderTiles("CartoDB.Voyager") |>
  addPolygons(
    fillColor = ~newpalette(SWING),
    fillOpacity = 0.5,
    weight = 1,
    color = "black",
    highlight = highlightOptions(
      stroke = 4, weight = 2, fillColor = "red", fillOpacity = 0.2),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  ) |>
  addLegend(
    pal = newpalette,
    values = ~SWING,
    bins = 12,
    title = "Dem. Vote Swing % </br> 2018 to 2020"
  ) |>
  addControl(
    html = githubtip, 
    position = "bottomleft"
  ) |>
  addMiniMap(toggleDisplay = TRUE)
calivotemap