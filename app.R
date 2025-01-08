# app.R

# Copyright (C) 2025 Finlay Scott
# 
# This file is licensed under the MIT License <http://opensource.org/licenses/MIT>.
# 
# You should have received a copy of the MIT License along with RcppEigenAutodiff.
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# Shiny app to demonstrate how EEZs can be selected from a map.
# Doesn't actually do anything - could show catches or something.
# It's just a proof of concept.

library(shiny)
library(leaflet)
library(sf)
library(bslib)

# Load the data - see script eez_data.R for its generation
load("data/simple_spc_pict_eez.Rdata")

# Make the map using leaflet - each EEZ polygon will be a polygon
eez_map <- leaflet(simple_spc_pict_eez)
# Add open street map tiles (country maps)
eez_map <- addTiles(eez_map)
# Add the polygons - add a layer ID using the territory name
# There are a bunch of options for this - line weights and colours and so on
eez_map <- addPolygons(eez_map,
                   weight = 1,
                   # Highlight the boxes as you over
                   highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                   # This is the key argument - set the input$id value for IDing an layer
                   # Here we use the eez_name column to ID a layer
                   layerId=simple_spc_pict_eez$eez_name) 
# Have a quick look for a check
# eez_map


# Super simple layout for now 
# Single card with the map
ui <- page_fillable(
  title = "PICT EEZ map test",
  h1("PICT EEZ map test"),
  card(leafletOutput("mymap"))
)

server <- function(input, output, session){
  
  # The map 
  # Each polygon in the EEZ data is treated as a layer
  # Just plot the map created above
  output$mymap <- renderLeaflet({
    return(eez_map)
   }) 
 
 
  # Show a pop-up based for each EEZ based on the eez_name column
  # Could probably add an 'SPC official name' column instead
  # eez is taken from layer id, which atm is the eez_name column
  eez_popup <- function(eez, lng, lat) {
    content <- eez # The text to actually show
    # Here the content is just layer name (eez_name) but could include other info (population size, area, etc)
    # Update the map with the pop-up instead of making a new one
    leafletProxy("mymap") |> addPopups(lng, lat, content, layerId = eez)
  }
 
  # Make an observer() so that When map is clicked, the popup appears
  # Potentially could be something else - bar chart of catches or something
  observe({
    # Get rid of current popups
    leafletProxy("mymap") %>% clearPopups()
    # Things happen if you click on something - could use a different input like rollover etc
    event <- input$mymap_shape_click
    # If you clicked on something with no layer, don't do anything
    if (is.null(event)){
      return()
    }
    # Otherwise get the click info contained in the event object
    # And show a pop-up where you clicked
    # Could show popup in the center of the polygon?
    # Use isolate to stop infinite loop from map updating and triggering itself
    isolate({
      eez_popup(event$id, event$lng, event$lat)
    })
  })
}

shinyApp(ui, server)









