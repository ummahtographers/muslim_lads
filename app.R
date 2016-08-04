# Harrow shiny app
# 
# Author: Miqdad Asaria
# Date: 02/08/2016
###############################################################################

library(shiny)
library(shinythemes)
library(leaflet)
library(DT)
source("Harrow.R")

server = shinyServer(function(input, output) {
  
  output$lsoa_data = renderDataTable({
    withProgress(message = 'Loading LSOA data table',{
      table = make_data_table(input$lad_name)
      datatable(table,
                style = 'bootstrap',
                rownames = FALSE,
                options = list(pageLength = 25, autoWidth = TRUE, dom='ftrpi'))
    })
  })
  
  output$lsoa_map = renderLeaflet({
    withProgress(message = 'Generating interactive LSOA map...',
                 make_choropleth_map(input$lad_name, input$mosque_markers)
    )
  })
  
  output$scatter = renderPlot({
    withProgress(message = 'Generating interactive LSOA map...',
                 make_scatter_plot(input$lad_name, input$xvar, input$yvar)
                 
    )
  })
  
})


ui = shinyUI(fluidPage(theme = "sandstone.css",
                  
                  titlePanel("Local Authority Deprivation Explorer"),
                  
                  sidebarPanel(
                    selectInput("lad_name", "Local Authority:",
                                get_lad_list(),
                                selected = "Harrow"),
                    
                    radioButtons("mosque_markers", "Show mosque markers on map:",
                                 list(TRUE,FALSE),
                                 selected=TRUE),
                    
                    selectInput("xvar", "X variable for scatter plot:",
                                get_var_list(),
                                selected = "Percent Muslim"
                    ),
                    
                    selectInput("yvar", "Y variable for scatter plot:",
                                get_var_list(),
                                selected = "Index of Multiple Deprivation IMD Score"
                    ),
                    
                    tags$div(
                      HTML("<p>Source code available <a href='https://github.com/ummahtographers/muslim_lads'>here</a>
                           <p><p>For details of deprivation variables <a href='https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/464597/English_Indices_of_Deprivation_2015_-_Research_Report.pdf'>click here</a>")
                    )
                    
                  ),
                  
                  mainPanel(
                    tabsetPanel(id="tabset",
                                tabPanel("Map", leafletOutput("lsoa_map")),
                                tabPanel("Scatter Plots", plotOutput("scatter")),
                                tabPanel("Raw Data", dataTableOutput("lsoa_data"))
                    )
                  )
))

shinyApp(ui = ui, server = server)