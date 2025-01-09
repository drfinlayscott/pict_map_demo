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
# Doesn't actually do anything - just shows random data - could show catches or something.
# It's just a proof of concept.

library(shiny)
library(leaflet)
library(sf)
library(bslib)
library(ggplot2)

# Load the data - see script eez_data.R for its generation
load("data/simple_spc_pict_eez_no_holes.Rdata") # Looks better with the holes removed
#load("data/simple_spc_pict_eez.Rdata")

# Make the map using leaflet - each EEZ polygon will be a polygon
# Problem is that without the holes in the EEZ data, the polygons lie over the land mass
# Solution would be to find a map tile provider that only has land mass layer and plot that on top
# Or get a map of the world in sf format and plot polygons - very time consuming
# Current approach is physical map, with labels added on last
# Or OpenStreetMap - just set opacity of polygons to low
eez_map <- leaflet() |>
  setView(lat = -5, lng = 180, zoom = 3.1) |>
  addMapPane("background_map", zIndex = 410) |>  # Level 1: bottom
  addMapPane("eez", zIndex = 420) |>  # Level 2: mid
  addMapPane("labels", zIndex = 430) |>          # Level 3: top
  addTiles() |> # Default open street map - high level of zoom - is OK
  # Or do these two together - looks pretty cool
  #addProviderTiles(
  #  providers$Esri.WorldPhysical, # Cool but no country names
  #  options = pathOptions(pane = "background_map")
  #) |> 
  #addProviderTiles(
  #  providers$CartoDB.PositronOnlyLabels,
  #  options = pathOptions(pane = "background_map")
  #) |> 
  addPolygons(data = simple_spc_pict_eez,
                   weight = 1,
                   fillOpacity = 0.05, # 0 = no fill
                   options=pathOptions(pane="eez"),
                   # Highlight the boxes as you over
                   highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                   # This is the key argument - set the input$id value for IDing an layer
                   # Here we use the eez_name column to ID a layer
                   layerId=simple_spc_pict_eez$eez_name)
# Check it out
# eez_mape
  
# Make up some fake data for a demo bar chart
bdat <- expand.grid(eez_name = simple_spc_pict_eez$eez_name,
            year = 2000:2024,
            Set = c("Set 1", "Set 2", "Set 3"))
bdat$data <- runif(n=nrow(bdat), min=1000, max=5000)

# ----------------------------- The app --------------------------------------

# Super simple layout for now 
# Single card with the map
ui <- page_fillable(
  title = "PICT EEZ map demo",
  
  # Column layout with map taking up most of the space
  layout_columns(
    col_widths=c(7,5,7,5),
    card(leafletOutput("mymap")),
    card("Some other plot"),
    card("Example bar chart with random data",
      plotOutput("eez_bar_plot")),
    card("Maybe a table")
  )
)

server <- function(input, output, session){
  
  # The map 
  # Each polygon in the EEZ data is treated as a layer
  # Just plot the map created above
  output$mymap <- renderLeaflet({
    return(eez_map)
   }) 
 
  # Mixing popups and mouseovers doesn't work well - use labels and markers
  observeEvent(input$mymap_shape_mouseover, {
    over_event <- input$mymap_shape_mouseover
    #leafletProxy("mymap") %>% clearMarkers()
    if(is.null(over_event)){
      return()
    }
    leafletProxy("mymap") |> addLabelOnlyMarkers(lng=over_event$lng, lat=over_event$lat, layerId=over_event$id, label=over_event$id, labelOptions = labelOptions(noHide = TRUE))
  }, priority=0)
  
  # Clear out old markers if mouseout
  # Not sure what order of precedent is.
  # If moving from one EEZ to another, then if mouseout is second then all markers will clear
  # If mouseout is first, markers are cleared first, then a new marker put on.
  # Set clearing out to have higher priority so it goes first (though it didn't seem to matter)
  observeEvent(input$mymap_shape_mouseout, {
    out_event <- input$mymap_shape_mouseout
    leafletProxy("mymap") |> clearMarkers()
  }, priority=1)
  
  # Bar plot of random data
  output$eez_bar_plot <- renderPlot({
    eez_click <- input$mymap_shape_click
    if(is.null(eez_click)){
      return()
    }
    eez <- eez_click$id
    p <- ggplot(data=subset(bdat, eez_name == eez), aes(x=year, y=data))
    p <- p + geom_bar(aes(fill=Set), stat="identity")
    p <- p + facet_wrap(~eez_name)
    p <- p + theme_bw()
    p <- p + theme(text = element_text(size = 16))
    p <- p + xlab("Year") + ylab("Random data")
    return(p)
  })
  
  
}

shinyApp(ui, server)









