library(rgdal)
library(dplyr)
library(leaflet)
library(ggplot2)

make_area_subset_geojson = function(area){
  lsoa_map = readOGR("data", "england_lsoa_2011_sgen_clipped", verbose=FALSE, stringsAsFactors=FALSE)
  area_lsoa_map = subset(lsoa_map, grepl(paste0(area," "),name, ignore.case = TRUE)) 
  area_lsoa_map = spTransform(area_lsoa_map, CRS("+proj=longlat +ellps=WGS84"))
  imd_data = read.csv("data/IMD_2015.csv", stringsAsFactors=FALSE)
  religion_data = read.csv("data/religion.csv", stringsAsFactors=FALSE)
  area_lsoa_map@data = left_join(left_join(area_lsoa_map@data,imd_data,by=c("code" = "LSOA.code..2011.")), religion_data, by=c("code" = "LSOA11CD"))
  writeOGR(area_lsoa_map, dsn=paste0("data/",tolower(area),"_lsoa_map.geojson"), layer="OGRGeoJSON", driver="GeoJSON", check_exists=FALSE)
  return(area_lsoa_map)
}

read_geojson = function(area){
  area_lsoa_map = NULL
  tryCatch(
    {
      area_lsoa_map = readOGR(dsn=paste0("data/",tolower(area),"_lsoa_map.geojson"), verbose=FALSE, stringsAsFactors=FALSE)
    }, error=function(cond){
      area_lsoa_map = make_area_subset_geojson(area)
    }, finally={} 
  )
  return(area_lsoa_map)
}

make_popup_messages = function(lsoa_map){
  popup_messages = paste0("<b>Name: </b>",lsoa_map$name,"<br>",
                          "<b>IMD Decile: </b>",lsoa_map$Index.of.Multiple.Deprivation..IMD..Decile..where.1.is.most.deprived.10..of.LSOAs.,"<br>",
                          "<b>Health Deprivation Decile: </b>", lsoa_map$Health.Deprivation.and.Disability.Decile..where.1.is.most.deprived.10..of.LSOAs.,"<br>",
                          "<b>Education Deprivation Decile: </b>", lsoa_map$Education..Skills.and.Training.Decile..where.1.is.most.deprived.10..of.LSOAs.,"<br>",
                          "<b>Housing Deprivation Decile: </b>", lsoa_map$Barriers.to.Housing.and.Services.Decile..where.1.is.most.deprived.10..of.LSOAs.,"<br>",
                          "<b>Income Deprivation Decile: </b>", lsoa_map$Income.Decile..where.1.is.most.deprived.10..of.LSOAs.,"<br>",
                          "<b>Child Poverty (IDACI) Decile: </b>", lsoa_map$Income.Deprivation.Affecting.Children.Index..IDACI..Decile..where.1.is.most.deprived.10..of.LSOAs.,"<br>",
                          "<b>Total Population: </b>",lsoa_map$Total.population..mid.2012..excluding.prisoners.,"<br>",
                          "<b>Muslim Population (x% of total): </b>",round(lsoa_map$muslim/lsoa_map$total,2)*100,"%<br>")
  return(popup_messages)  
}

make_choropleth_map = function(area, mosque_markers){
  area_lsoa_map = read_geojson(area)
  
  popup_message = make_popup_messages(area_lsoa_map)
  
  imd_pal = colorBin("Oranges", area_lsoa_map$Index.of.Multiple.Deprivation..IMD..Decile..where.1.is.most.deprived.10..of.LSOAs., n=10)
  health_pal = colorBin("Blues", area_lsoa_map$Health.Deprivation.and.Disability.Decile..where.1.is.most.deprived.10..of.LSOAs., n=10, pretty = FALSE)
  education_pal = colorBin("Reds", area_lsoa_map$Education..Skills.and.Training.Decile..where.1.is.most.deprived.10..of.LSOAs., n=10, pretty = FALSE)
  housing_pal = colorBin("Greys", area_lsoa_map$Barriers.to.Housing.and.Services.Decile..where.1.is.most.deprived.10..of.LSOAs., n=10, pretty = FALSE)
  child_pal = colorBin("Purples", area_lsoa_map$Income.Deprivation.Affecting.Children.Index..IDACI..Decile..where.1.is.most.deprived.10..of.LSOAs., n=10, pretty = FALSE)
  muslim_pal = colorBin("Greens", area_lsoa_map$muslim_decile, 10, pretty = FALSE)
  
  choropleth_map = leaflet(area_lsoa_map) %>% 
    addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE)) %>%
    addPolygons(stroke = TRUE, 
                smoothFactor = 1, 
                fillOpacity = 0.7, 
                weight = 1, 
                popup = popup_message, 
                fillColor = imd_pal(area_lsoa_map$Index.of.Multiple.Deprivation..IMD..Decile..where.1.is.most.deprived.10..of.LSOAs.), 
                color="black",
                group="IMD") %>%
    addPolygons(stroke = TRUE, 
                smoothFactor = 1, 
                fillOpacity = 0.7, 
                weight = 1, 
                popup = popup_message, 
                fillColor = health_pal(area_lsoa_map$Health.Deprivation.and.Disability.Decile..where.1.is.most.deprived.10..of.LSOAs.), 
                color="black",
                group="Health") %>%
    addPolygons(stroke = TRUE, 
                smoothFactor = 1, 
                fillOpacity = 0.7, 
                weight = 1, 
                popup = popup_message, 
                fillColor = education_pal(area_lsoa_map$Education..Skills.and.Training.Decile..where.1.is.most.deprived.10..of.LSOAs.), 
                color="black",
                group="Education") %>%
    addPolygons(stroke = TRUE, 
                smoothFactor = 1, 
                fillOpacity = 0.7, 
                weight = 1, 
                popup = popup_message, 
                fillColor = housing_pal(area_lsoa_map$Barriers.to.Housing.and.Services.Decile..where.1.is.most.deprived.10..of.LSOAs.), 
                color="black",
                group="Housing") %>%
    addPolygons(stroke = TRUE, 
                smoothFactor = 1, 
                fillOpacity = 0.7, 
                weight = 1, 
                popup = popup_message, 
                fillColor = child_pal(area_lsoa_map$Income.Deprivation.Affecting.Children.Index..IDACI..Decile..where.1.is.most.deprived.10..of.LSOAs.), 
                color="black",
                group="Child Poverty") %>%    
    addPolygons(stroke = TRUE, 
                smoothFactor = 1, 
                fillOpacity = 0.7, 
                weight = 1, 
                popup = popup_message, 
                fillColor = muslim_pal(area_lsoa_map$muslim_decile), 
                color="black",
                group="Muslim %") %>%
    addLayersControl(
      baseGroups=c("IMD", "Health", "Education", "Housing", "Child Poverty", "Muslim %"),
      position = "topleft",
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    setView(lng=mean(area_lsoa_map@bbox["x",]), lat=mean(area_lsoa_map@bbox["y",]), zoom=11)
  if(mosque_markers){
    mosques = read.csv("data/mosques.csv",stringsAsFactors=FALSE)
    mosques$info = mosques$info %>% gsub("\\[.*\\]","",.) %>% gsub("\\*","",.) %>% trimws()
    choropleth_map = choropleth_map %>% 
      addMarkers(~long, ~lat, popup = ~as.character(info), data=mosques, clusterOptions=markerClusterOptions())
  }
  return(choropleth_map)
}

make_data_table = function(area){
  area_lsoa_map = read_geojson(area)
  data = area_lsoa_map@data
  colnames(data) = gsub("\\."," ",colnames(data)) %>% gsub("  "," ",.) %>% trimws()
  data = data %>% 
    mutate(`Percent Christian` = round((christian/total)*100,1),
           `Percent Buddhist` = round((buddhist/total)*100,1),
           `Percent Hindu` = round((hindu/total)*100,1),
           `Percent Jewish` = round((jewish/total)*100,1),
           `Percent Muslim` = round((muslim/total)*100,1),
           `Percent None` = round((none/total)*100,1),
           `Percent Not Stated` = round((not_stated/total)*100,1))
  # clean up colum names and columns to return
  return(data %>% arrange(name))
}

get_lad_list = function(){
  lad_list = as.data.frame(read.csv("data/lad_list.csv", stringsAsFactors=FALSE))
  return(lad_list)
}

get_var_list = function(){
  lsoa_data = make_data_table("harrow")
  return(colnames(lsoa_data))
}

make_scatter_plot = function(area,xvar,yvar){
  lsoa_data = make_data_table(area)
  lsoa_data$Population = lsoa_data$total / 1500
  plot = ggplot(lsoa_data, aes(x=get(xvar), y=get(yvar))) +
    geom_point(alpha=0.6, aes(colour=get("Percent Muslim"),size=total)) +
    xlab(xvar) +
    ylab(yvar) +
    stat_smooth(aes(weight=total), method='lm', colour="darkred", size=0.5, linetype=2, alpha=0.5, se=FALSE) +
    theme_minimal() +
    scale_color_distiller("Percent Muslim", palette="YlGn", direction=1) +
    scale_size_continuous("LSOA Population")
  return(plot)
}
