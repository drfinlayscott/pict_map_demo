# eez_data.R

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

# Sorting out the EEZ data for plotting.
# We're just going to do this once and save the result for use in the Shiny app.

# 1. Download the EEZ data

# Head over to the wonderful folks at Marine Regions (https://www.marineregions.org).
# Under 'Downloads' get the latest Marine Boundaries World EEZ data.
# Make sure to get the 0-360 version else the plots get messed up around the data line.
# I'm using v12 from 25/10/2023:
# Flanders Marine Institute (2023). Maritime Boundaries Geodatabase: Maritime Boundaries and Exclusive Economic Zones (200NM), version 12. Available online at https://www.marineregions.org/. https://doi.org/10.14284/632

# I also tried the low res version but some of the EEZs had weird lines crossing them and I don't
# know anything about to GIS to fix them.
# A shame, as the low res ones plot much faster without losing anything.

# The data comes as a zip file. Unzip it, keeping its home folder.
# You will have a folder with a bunch GIS-looking files.
# I know nothing about GIS.

# 2. Read in the data

# We'll use the sf library.

library(sf)

# Try the high resolution data set
# Has geom as polygon - quite large - needed for clicking in Shiny app
eez <- read_sf("data/World_EEZ_v12_20231025_HR_0_360/", layer="eez_v12_0_360")
# Low res - not 0-360
#eez <- read_sf("World_EEZ_v12_20231025_LR/", layer="eez_v12_lowres")
# Just boundaries data set
# Has geom as line - no good for clicking
#eezb <- read_sf("World_EEZ_v12_20231025_HR_0_360/", layer="eez_boundaries_v12_0_360")

# 3. Only include the EEZs of the countries and territories of interest, SPC PICT members.
# Could use TERRITORY1 column
unique(eez$TERRITORY1)
# Or SOVEREIGN
unique(eez$SOVEREIGN1)
# Some SPC members are territories though - can be confusing.
# Check with Kitibati
subset(eez, SOVEREIGN1 == "Kiribati")
# Made up of three Territories: Phoenix, Line , Gilbert
# Subset on territory

sort(unique(eez$TERRITORY1))
sort(unique(eez$SOVEREIGN1))

# SPC PICT members using same name as EEZ data - note inclusion of Guam and Hawaii and other US territories
# Micronesia should be renamed to Federated States of Micronesia
# Kiribati is tricky
subset(eez, SOVEREIGN1 == "Kiribati")$TERRITORY1

spc_pict_territories <- c("American Samoa", "Cook Islands", "Micronesia", "Fiji", "Guam", "Hawaii", subset(eez, SOVEREIGN1 == "Kiribati")$TERRITORY1, "Marshall Islands", "Nauru", "Niue", "Northern Mariana Islands", "New Caledonia", "Palau", "Papua New Guinea", "Pitcairn", "French Polynesia", "Samoa", "Solomon Islands", "Tokelau", "Tonga", "Tuvalu", "Vanuatu", "Wallis and Futuna")

# For checking
# print(subset(eez, TERRITORY1 %in% c(spc_pict_territories)), n=27)
# print(subset(eez, SOVEREIGN1 %in% c(spc_pict_territories)), n=27)


spc_pict_eez <- subset(eez, TERRITORY1 %in% c(spc_pict_territories))
# We only want the 200nm POL_TYPE (PNG has an extra one - something on joint region)
spc_pict_eez <- subset(spc_pict_eez, POL_TYPE=="200NM")

# Quick sanity check - takes a while to plot
plot(spc_pict_eez[,"TERRITORY1"])

# Try with the low res data to speed up plotting
# Low res is not 0-360 but -180 to 180 so the map is not centered.
# EEZs on the boundary having vertical lines to chop them. This is why the weird lines appear on the Shiny map with LR data.

# Can we simplify the HR data
# See: 
# https://gis.stackexchange.com/questions/243569/simplify-polygons-of-sf-object
# Try st_simplify
simple_spc_pict_eez <- st_simplify(spc_pict_eez, preserveTopology = TRUE) # Try with no arguments
object.size(spc_pict_eez)
object.size(simple_spc_pict_eez)
# Made f'all difference and plot is fucked, probably because of the 0-360 thing
plot(simple_spc_pict_eez[,"TERRITORY1"])

# Use rmapshaper package instead
library(rmapshaper)
simple_spc_pict_eez <- rmapshaper::ms_simplify(input = as(spc_pict_eez, 'Spatial')) %>% st_as_sf()

object.size(spc_pict_eez) # 22299776 bytes
object.size(simple_spc_pict_eez) #1313600 bytes
plot(simple_spc_pict_eez[,"TERRITORY1"])
# Looks OK! And is smaller



# Fix up the names of the EEZs
simple_spc_pict_eez$eez_name <- simple_spc_pict_eez$TERRITORY1
# Kiribati
# Oof - this code is grim - should do it with dplyr or something
simple_spc_pict_eez[simple_spc_pict_eez$TERRITORY1 %in% c("Line Group", "Phoenix Group", "Gilbert Islands"), "eez_name"]  <- paste0("Kiribati (", simple_spc_pict_eez[simple_spc_pict_eez$TERRITORY1 %in% c("Line Group", "Phoenix Group", "Gilbert Islands"),]$eez_name, ")")
# FSM
simple_spc_pict_eez[simple_spc_pict_eez$TERRITORY1 %in% c("Micronesia"), "eez_name"] <- "Federated States of Micronesia"

# Check they're OK
sort(simple_spc_pict_eez$eez_name)

# Things look OK but it is possible to simplify further.
# If you plot just one country / territory, e.g. Hawaii you will see that is full of holes where the land is.
# This is probably really helpful for proper GIS work but for the Shiny app it can make a mess, particularly with mouseover and mouseout that get triggered everytime you pass over a hole.
# For our purpose we just want the 'outside' of the EEZ.
# We'll be plotting a map under the polygons with the land mass, so we don't need these holes.
# How to get rid of them?

plot(subset(simple_spc_pict_eez, eez_name == "Hawaii")[, "eez_name"])

# The following is me trying to figure out how sf and simple features work.
# I blunder around, doing stuff in a very wrong way but the result looks OK.
# Skip to the end if your're not interested in my ham-fisted efforts.

# The geometry is described as a multipolygon. What does that mean?
# Look at Niue for simple example
plot(subset(simple_spc_pict_eez, eez_name == "Niue")[, "eez_name"])
niue <- subset(simple_spc_pict_eez, eez_name == "Niue")$geometry

#POLYGON 	geometry with a positive area (two-dimensional); sequence of points form a closed, non-self intersecting ring; the first ring denotes the exterior ring, zero or more subsequent rings denote holes in this exterior ring

# MULTIPOLTGON - set of POLYGONS
# Can we cast MP to just a P?
# Or how to interrogate the MP?

niue[[1]][[1]] # length of 2 - probably inner and outer - second one looks inner
niue[[1]][[1]][2]
plot(st_polygon(niue[[1]][[1]][1])) # Just outer, as a polygon
plot(st_polygon(niue[[1]][[1]][2])) # Just inner, as a polygon

# Assumption - each EEZ geometry is a [[1]][[1]] list with X elements, where the first element is the outer
# Apply this method across the EEZ data
# Could use a for loop - bit embarrassing and not very R
#simple_spc_pict_eez[1,"geometry"][[1]][[1]][1]
# Brutalise it!
new_geometry <- lapply(st_geometry(simple_spc_pict_eez), function(x){
  st_multipolygon(list(st_polygon(x[[1]][1])))
})

# A list of MPs - but how to get this back into the eez data?
simple_spc_pict_eez
st_geometry(simple_spc_pict_eez) <- st_sfc(new_geometry)
plot(simple_spc_pict_eez[, "eez_name"])

# Looks like it worked!

object.size(simple_spc_pict_eez) # 272544 bytes - way smaller too

save(simple_spc_pict_eez, file="data/simple_spc_pict_eez.Rdata")






