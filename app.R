# Harrow shiny app
# 
# Author: Miqdad Asaria
# Date: 02/08/2016
###############################################################################

library(shiny)
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
                 make_choropleth_map(input$lad_name)
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
                    selectInput("xvar", "X variable for scatter plot",
                                list("Index of Multiple Deprivation IMD Score",                                                       
                                     "Index of Multiple Deprivation IMD Rank where 1 is most deprived",
                                     "Index of Multiple Deprivation IMD Decile where 1 is most deprived 10 of LSOAs",                 
                                     "Income Score rate",                                                                             
                                     "Income Rank where 1 is most deprived",                                                          
                                     "Income Decile where 1 is most deprived 10 of LSOAs",                                            
                                     "Employment Score rate",                                                                         
                                     "Employment Rank where 1 is most deprived",                                                      
                                     "Employment Decile where 1 is most deprived 10 of LSOAs",                                        
                                     "Education Skills and Training Score",                                                           
                                     "Education Skills and Training Rank where 1 is most deprived",                                   
                                     "Education Skills and Training Decile where 1 is most deprived 10 of LSOAs",                     
                                     "Health Deprivation and Disability Score",                                                       
                                     "Health Deprivation and Disability Rank where 1 is most deprived",                               
                                     "Health Deprivation and Disability Decile where 1 is most deprived 10 of LSOAs",                 
                                     "Crime Score",                                                                                   
                                     "Crime Rank where 1 is most deprived",                                                           
                                     "Crime Decile where 1 is most deprived 10 of LSOAs",                                             
                                     "Barriers to Housing and Services Score",                                                        
                                     "Barriers to Housing and Services Rank where 1 is most deprived",                                
                                     "Barriers to Housing and Services Decile where 1 is most deprived 10 of LSOAs",                  
                                     "Living Environment Score",                                                                      
                                     "Living Environment Rank where 1 is most deprived",                                              
                                     "Living Environment Decile where 1 is most deprived 10 of LSOAs",                                
                                     "Income Deprivation Affecting Children Index IDACI Score rate",                                  
                                     "Income Deprivation Affecting Children Index IDACI Rank where 1 is most deprived",               
                                     "Income Deprivation Affecting Children Index IDACI Decile where 1 is most deprived 10 of LSOAs", 
                                     "Income Deprivation Affecting Older People IDAOPI Score rate",                                   
                                     "Income Deprivation Affecting Older People IDAOPI Rank where 1 is most deprived",                
                                     "Income Deprivation Affecting Older People IDAOPI Decile where 1 is most deprived 10 of LSOAs",  
                                     "Children and Young People Sub domain Score",                                                    
                                     "Children and Young People Sub domain Rank where 1 is most deprived",                            
                                     "Children and Young People Sub domain Decile where 1 is most deprived 10 of LSOAs",              
                                     "Adult Skills Sub domain Score",                                                                 
                                     "Adult Skills Sub domain Rank where 1 is most deprived",                                         
                                     "Adult Skills Sub domain Decile where 1 is most deprived 10 of LSOAs",                           
                                     "Geographical Barriers Sub domain Score",                                                        
                                     "Geographical Barriers Sub domain Rank where 1 is most deprived",                                
                                     "Geographical Barriers Sub domain Decile where 1 is most deprived 10 of LSOAs",                  
                                     "Wider Barriers Sub domain Score",                                                               
                                     "Wider Barriers Sub domain Rank where 1 is most deprived",                                       
                                     "Wider Barriers Sub domain Decile where 1 is most deprived 10 of LSOAs",                         
                                     "Indoors Sub domain Score",                                                                      
                                     "Indoors Sub domain Rank where 1 is most deprived",                                              
                                     "Indoors Sub domain Decile where 1 is most deprived 10 of LSOAs",                                
                                     "Outdoors Sub domain Score",                                                                     
                                     "Outdoors Sub domain Rank where 1 is most deprived",                                             
                                     "Outdoors Sub domain Decile where 1 is most deprived 10 of LSOAs",                               
                                     "Total population mid 2012 excluding prisoners",                                                 
                                     "Dependent Children aged 0 15 mid 2012 excluding prisoners",                                     
                                     "Population aged 16 59 mid 2012 excluding prisoners",                                            
                                     "Older population aged 60 and over mid 2012 excluding prisoners",                                
                                     "Working age population 18 59 64 for use with Employment Deprivation Domain excluding prisoners",
                                     "christian",                                                                                     
                                     "buddhist",                                                                                      
                                     "hindu",                                                                                         
                                     "jewish",                                                                                        
                                     "muslim",                                                                                        
                                     "sikh",                                                                                          
                                     "other",                                                                                         
                                     "none",                                                                                          
                                     "not_stated",                                                                                    
                                     "total",                                                                                         
                                     "muslim_decile",                                                                                 
                                     "Percent Christian",                                                                             
                                     "Percent Buddhist",                                                                              
                                     "Percent Hindu",                                                                                 
                                     "Percent Jewish",                                                                                
                                     "Percent Muslim",                                                                                
                                     "Percent None",                                                                                  
                                     "Percent Not Stated"),
                                selected = "Percent Muslim"
                    ),
                    
                    selectInput("yvar", "Y variable for scatter plot",
                                list("Index of Multiple Deprivation IMD Score",                                                       
                                     "Index of Multiple Deprivation IMD Rank where 1 is most deprived",
                                     "Index of Multiple Deprivation IMD Decile where 1 is most deprived 10 of LSOAs",                 
                                     "Income Score rate",                                                                             
                                     "Income Rank where 1 is most deprived",                                                          
                                     "Income Decile where 1 is most deprived 10 of LSOAs",                                            
                                     "Employment Score rate",                                                                         
                                     "Employment Rank where 1 is most deprived",                                                      
                                     "Employment Decile where 1 is most deprived 10 of LSOAs",                                        
                                     "Education Skills and Training Score",                                                           
                                     "Education Skills and Training Rank where 1 is most deprived",                                   
                                     "Education Skills and Training Decile where 1 is most deprived 10 of LSOAs",                     
                                     "Health Deprivation and Disability Score",                                                       
                                     "Health Deprivation and Disability Rank where 1 is most deprived",                               
                                     "Health Deprivation and Disability Decile where 1 is most deprived 10 of LSOAs",                 
                                     "Crime Score",                                                                                   
                                     "Crime Rank where 1 is most deprived",                                                           
                                     "Crime Decile where 1 is most deprived 10 of LSOAs",                                             
                                     "Barriers to Housing and Services Score",                                                        
                                     "Barriers to Housing and Services Rank where 1 is most deprived",                                
                                     "Barriers to Housing and Services Decile where 1 is most deprived 10 of LSOAs",                  
                                     "Living Environment Score",                                                                      
                                     "Living Environment Rank where 1 is most deprived",                                              
                                     "Living Environment Decile where 1 is most deprived 10 of LSOAs",                                
                                     "Income Deprivation Affecting Children Index IDACI Score rate",                                  
                                     "Income Deprivation Affecting Children Index IDACI Rank where 1 is most deprived",               
                                     "Income Deprivation Affecting Children Index IDACI Decile where 1 is most deprived 10 of LSOAs", 
                                     "Income Deprivation Affecting Older People IDAOPI Score rate",                                   
                                     "Income Deprivation Affecting Older People IDAOPI Rank where 1 is most deprived",                
                                     "Income Deprivation Affecting Older People IDAOPI Decile where 1 is most deprived 10 of LSOAs",  
                                     "Children and Young People Sub domain Score",                                                    
                                     "Children and Young People Sub domain Rank where 1 is most deprived",                            
                                     "Children and Young People Sub domain Decile where 1 is most deprived 10 of LSOAs",              
                                     "Adult Skills Sub domain Score",                                                                 
                                     "Adult Skills Sub domain Rank where 1 is most deprived",                                         
                                     "Adult Skills Sub domain Decile where 1 is most deprived 10 of LSOAs",                           
                                     "Geographical Barriers Sub domain Score",                                                        
                                     "Geographical Barriers Sub domain Rank where 1 is most deprived",                                
                                     "Geographical Barriers Sub domain Decile where 1 is most deprived 10 of LSOAs",                  
                                     "Wider Barriers Sub domain Score",                                                               
                                     "Wider Barriers Sub domain Rank where 1 is most deprived",                                       
                                     "Wider Barriers Sub domain Decile where 1 is most deprived 10 of LSOAs",                         
                                     "Indoors Sub domain Score",                                                                      
                                     "Indoors Sub domain Rank where 1 is most deprived",                                              
                                     "Indoors Sub domain Decile where 1 is most deprived 10 of LSOAs",                                
                                     "Outdoors Sub domain Score",                                                                     
                                     "Outdoors Sub domain Rank where 1 is most deprived",                                             
                                     "Outdoors Sub domain Decile where 1 is most deprived 10 of LSOAs",                               
                                     "Total population mid 2012 excluding prisoners",                                                 
                                     "Dependent Children aged 0 15 mid 2012 excluding prisoners",                                     
                                     "Population aged 16 59 mid 2012 excluding prisoners",                                            
                                     "Older population aged 60 and over mid 2012 excluding prisoners",                                
                                     "Working age population 18 59 64 for use with Employment Deprivation Domain excluding prisoners",
                                     "christian",                                                                                     
                                     "buddhist",                                                                                      
                                     "hindu",                                                                                         
                                     "jewish",                                                                                        
                                     "muslim",                                                                                        
                                     "sikh",                                                                                          
                                     "other",                                                                                         
                                     "none",                                                                                          
                                     "not_stated",                                                                                    
                                     "total",                                                                                         
                                     "muslim_decile",                                                                                 
                                     "Percent Christian",                                                                             
                                     "Percent Buddhist",                                                                              
                                     "Percent Hindu",                                                                                 
                                     "Percent Jewish",                                                                                
                                     "Percent Muslim",                                                                                
                                     "Percent None",                                                                                  
                                     "Percent Not Stated"),
                                selected = "Index of Multiple Deprivation IMD Score"
                    ),
                    tags$div(
                      HTML("<img src='mcb_logo.jpg' alt='Muslim Council of Britain' width=90%/>
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