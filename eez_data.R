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
eez <- read_sf("World_EEZ_v12_20231025_HR_0_360/", layer="eez_v12_0_360")
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

object.size(spc_pict_eez)
object.size(simple_spc_pict_eez)
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

save(simple_spc_pict_eez, file="data/simple_spc_pict_eez.Rdata")






