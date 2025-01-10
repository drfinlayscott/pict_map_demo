# Pict Map Demo
Proof-of-concept of an R Shiny app with a map with clickable EEZs based on SPC (Pacific Community) PICTs.

It uses the `leaflet` package for plotting the interactive map.

The data comes from [Marine Regions](https://www.marineregions.org).

*Flanders Marine Institute (2023). Maritime Boundaries Geodatabase: Maritime Boundaries and Exclusive Economic Zones (200NM), version 12. https://doi.org/10.14284/632*

The map doesn't really do much at the moment. If you hover over an EEZ a label appears with the country / territory name.
If you click on an EEZ a simple bar chart with *random* data appears for that country / territory.
I emphasise that the data in the plot is *random* and means nothing - it's just an example.
A couple of empty placeholder cards are also there to take up space.

The app is live [here](https://drfinlayscott.shinyapps.io/pict_eez_map_demo/).

Disclaimer: All errors and cock-ups, including with EEZ locations, naming etc. are mine and nothing to do with SPC or Marine Boundaries.

