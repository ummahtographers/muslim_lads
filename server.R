# Harrow shiny app
# 
# Author: Miqdad Asaria
# Date: 30/06/2016
###############################################################################

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
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
