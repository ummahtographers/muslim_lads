# Harrow shiny app
# 
# Author: Miqdad Asaria
# Date: 30/06/2016
###############################################################################

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
  selected_lad = reactiveValues(lad_name="Harrow")
  
	output$lsoa_data = renderDataTable({
	  withProgress(message = 'Loading LSOA data table',{
	    table = make_data_table(selected_lad$lad_name)
	    datatable(table,
        style = 'bootstrap',
	      rownames = FALSE,
	      options = list(pageLength = 25, autoWidth = TRUE, dom='ftrpi'))
	  })
	})
	
	output$lsoa_map = renderLeaflet({
	  withProgress(message = 'Generating interactive LSOA map...',
	               make_choropleth_map(selected_lad$lad_name)
	  )
	})
	
})
