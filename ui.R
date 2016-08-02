# Harrow shiny app
# 
# Author: Miqdad Asaria
# Date: 02/08/2016
###############################################################################

library(shiny)
library(leaflet)
library(DT)
source("Harrow.R")

shinyUI(fluidPage(theme = "sandstone.css",
			
	titlePanel("Local Authority Deprivation Explorer"),
	
	mainPanel(
	      tabsetPanel(id="tabset",
	        tabPanel("Map", leafletOutput("lsoa_map")),
				  tabPanel("Raw Data", dataTableOutput("lsoa_data"))
				)
		)
))