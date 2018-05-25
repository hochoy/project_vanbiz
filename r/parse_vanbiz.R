
library(jsonlite)
library(ggplot2)
library(stringr)
library(readr)
library(dplyr)
library(plotly)
library(ggmap)
library(RCurl)
source("~/Developer/project_vanbiz/r/custom_func.R")


#Then, we read in vancouver business licenses from 
# http://data.vancouver.ca/datacatalogue/businessLicence.htm courtesy of the City of Vancouver.
# I've chosen to use the json format of the file here but csv is also available at the vancouver open data website. 

json_path <- "~/Developer/project_vanbiz/r/2016business_licences.json"
vanbiz_raw <- fromJSON(json_path,simplifyDataFrame = TRUE,flatten = FALSE)
str(vanbiz_raw)

# This raw json contains a field for name, type and features. 
# We are only interested in the features field
# Unfortunately, the geometry field in features is missing. There are no coordinates or polygons that
# we can use
vanbiz_raw$features$geometry %>% as.factor() %>% str()

# Instead, that information is stored in "properties" within features.
# That means we have to extract it ourselves. Boo!
vanbiz_raw$features$properties %>% colnames() 
vanbiz_properties <- vanbiz_raw$features$properties %>% 
  mutate(index = 1:nrow(.)) %>% 
  select(index,everything()) %>% 
  mutate(NumberOfEmployees = as.integer(NumberOfEmployees))

vanbiz_properties %>% nrow() #61399 business entries

# There's a crazy amount of registered companies in Vancouver.
# Thus, we're going to have to narrow down the list.
# We assume that companies that have more than 10 employees are more likely to hire.
# We select only businesses that either have an issued license or pending license.

vanbiz_valid <- vanbiz_properties %>% 
  filter(Province == "BC" | is.na(Province)) %>% 
  filter(NumberOfEmployees >= 10) %>% 
  filter(Status %in% c("Issued","Pending")) %>% 
  filter(!is.na(BusinessName))
nrow(vanbiz_valid) # 6131 out of 61399 businesses that pass our filter

# Check to see if these businesses have a valid coordinate that we can use with Mapbox GL JS
vanbiz_latlongcheck <- vanbiz_valid %>% 
  mutate(hascoord = (!is.na(Latitude) & !is.na(Longitude))) %>% 
  mutate(hasaddress = (!is.na(House) & !is.na(Street)))

vanbiz_latlongcheck %>% filter(hascoord) %>% nrow() # 5426 biz have coords -> READY
vanbiz_latlongcheck %>% filter(!hascoord) %>% nrow() # 705 biz have no coords
vanbiz_latlongcheck %>% filter(!hascoord) %>% 
  filter(hasaddress) %>% nrow()                       # 663 biz have address -> GEOCODE
vanbiz_latlongcheck %>% filter(!hascoord) %>% 
  filter(!hasaddress) %>% nrow()                       # 42 biz have no address -> Google API 

# add full address and geocode columns. Needed later
vanbiz_pregeocoded <- vanbiz_latlongcheck %>% 
  mutate(full_address = "not set",
         geocode = "not run")

write_rds(vanbiz_pregeocoded,"~/Developer/project_vanbiz/r/vanbiz_pregeocoded.rds")

# geocode businesses that do not have coordinates. 
vanbiz_pregeocoded <- read_rds("~/Developer/project_vanbiz/r/vanbiz_pregeocoded.rds")

#helper function
isGeocoded <- function(geocode_list) {
  temp_vec <- vector()
  for (i in 1:length(geocode_list)) {
    temp_vec[i] <- !is.na(geocode_list[[i]]$lon)
  }
  temp_vec
}

geocode_biz <- function(vanbiz_df) {
  
  # filter for rows that need geocoding work
  vanbiz_todo <- vanbiz_df %>% 
    filter(geocode == "not run")
  
  # filter df for geocoding and google search api
  vanbiz_hascoord <- vanbiz_todo %>% 
    filter(hascoord) %>% 
    mutate(geocode = "has coord")
  vanbiz_hasaddress <- vanbiz_todo %>% 
    filter(!hascoord) %>% 
    filter(hasaddress)
  vanbiz_noaddress <- vanbiz_todo %>% 
    filter(!hascoord) %>% 
    filter(!hasaddress)
  
  # break the function if there are not enough geocoding queries remaining
  if ((nrow(vanbiz_hasaddress) + nrow(vanbiz_noaddress)) > 
      as.integer(str_extract(geocodeQueryCheck(),"[0-9]+"))) {
    print("2500 geocode limit reached")
    break
  }
  
  #geocode vanbiz_hasaddress
  vanbiz_hasaddress <- vanbiz_hasaddress %>% 
    mutate(full_address = paste(House,Street,City,Province,"Canada"),
           geocode = lapply(full_address,
                            function(x) {geocode(x)})) %>% 
    mutate(Latitude = sapply(geocode,function(x){x$lat})) %>% 
    mutate(Longitude = sapply(geocode,function(x){x$lon}))
  
  vanbiz_hasaddress_success <- vanbiz_hasaddress %>% 
    filter(isGeocoded(geocode))
  vanbiz_hasaddress_failed <- vanbiz_hasaddress %>% 
    filter(!isGeocoded(geocode))
  
  #check for failed geocoding
  vanbiz_googlesearch <- vanbiz_noaddress %>% 
    rbind(vanbiz_hasaddress_failed)
  
  #display remaining query
  print(geocodeQueryCheck())
  #final output
  list(vanbiz_hascoord = vanbiz_hascoord,
       vanbiz_hasaddress_success = vanbiz_hasaddress_success,
       vanbiz_googlesearch = vanbiz_googlesearch)
}

## Uncomment to run geocode
# vanbiz_geocoded <- geocode_biz(vanbiz_pregeocoded)
# write_rds(vanbiz_geocoded,"~/Developer/project_vanbiz/r/vanbiz_geocoded.rds")


# Installing a nominatim server/local instance that runs on OpenStreetMap is an
#  alternative to using the `geocode` package or other hosted geocoding API.
# See: https://wiki.openstreetmap.org/wiki/Nominatim/Installation
# Google and other hosted APIs (i.e. mapquest) limit searches, geocoding and the use of 
# the data on their servers and thus, is not feasable beyond this demo.
# However, google also has the Places Library that lets us find places via bizname when the address is not available.
# For now, we will use a mix of Google geocoding(for address geocoding) and Google Places (for business name geocoding)
















vanbiz_geocoded <- read_rds("~/Developer/project_vanbiz/r/vanbiz_geocoded.rds")

View(vanbiz_geocoded$vanbiz_hascoord) # 5426 biz have valid coordinates
View(vanbiz_geocoded$vanbiz_hasaddress_success) # 567 biz have addresses that were successfully geocoded
View(vanbiz_geocoded$vanbiz_googlesearch) # 138 have no addresses or have addresses that were NOT sucessfully geocoded

nrow(vanbiz_geocoded$vanbiz_hascoord) # 5426 aleady have valid coordinates and didn't need geocoding
nrow(vanbiz_geocoded$vanbiz_hasaddress_success) # 567 biz have addresses that were successfully geocoded
nrow(vanbiz_geocoded$vanbiz_googlesearch) #138 have no addresses or have addresses that were NOT sucessfully geocoded OR
                                            # query went over limit!


# run google search api on these addresses to get addressses for geocoding
# performed via https://developers.google.com/places/web-service/search
# using 'nearbysearch'

google_maps_search <- function(query_df = vanbiz_geocoded$vanbiz_googlesearch,
                                 searchCenter = "49.253386,-123.130621",
                                 searchRadius = 30000,
                                 api_key = "AIzaSyDL7KVzfaUb-17Ew_cycTG0GoAlf9YammY") {
  
  # parse out a hyperlink to access the Google maps api
  vanbiz_googleplaces <- query_df %>% 
    mutate(link = paste0("https://maps.googleapis.com/maps/api/place/nearbysearch/json?",
                         "location=",searchCenter,
                         "&radius=",searchRadius,
                         "&name=",(BusinessName %>% 
                                     str_trim(side="both") %>% 
                                     str_replace_all("[^[:alnum:] ]"," ") %>% 
                                     str_replace_all(" ","+")),
                         "&key=",api_key))

  googlesearch_results <- data.frame(index = vector(),
                                     goo_lat = vector(),
                                     goo_lng = vector(),
                                     goo_address = vector(),
                                     goo_name = vector())
             
  # loop through all rows to query google maps api
  for (i in 1:nrow(vanbiz_googleplaces)) {
    print(i)
    
    ori_index <- vanbiz_googleplaces$index[i]
    goo_query <- vanbiz_googleplaces$BusinessName[i]
    
    # query the API
    goo_response <- vanbiz_googleplaces$link[i] %>%
      getURL() %>%
      fromJSON()
    
    # parse the results
    if (goo_response$status == "OK" && (!is.na(goo_query) && goo_query != "NA")) {
      goo_result <- goo_response$results[1,]
      #desired fields
      goo_lat <- goo_result$geometry$location$lat
      goo_lng <- goo_result$geometry$location$lng
      goo_address <- goo_result$vicinity
      goo_name <- goo_result$name
    } else {
      goo_lat <- 0
      goo_lng <- 0
      goo_address <- "not found"
      goo_name <- "not found"
    }
    
    # add results to the holder df
    googlesearch_results <- rbind(googlesearch_results,
                                  data.frame(index = as.integer(ori_index),
                                             goo_lat = as.double(as.character(goo_lat)),
                                             goo_lng = as.double(as.character(goo_lng)),
                                             goo_address = as.character(goo_address),
                                             goo_name = as.character(goo_name)))
    # randomize the search rate
    Sys.sleep(runif(1,min = 1.2,max = 2))
  }
  
  # combine the results back to the query_df
  vanbiz_googlesearched_output <- left_join(query_df,
                                            googlesearch_results,
                                            by="index")
  vanbiz_googlesearched_output
}

vanbiz_googlesearched <- google_maps_search(query_df=vanbiz_geocoded$vanbiz_googlesearch)
write_rds(vanbiz_googlesearched,
          path = "~/Developer/project_vanbiz/r/vanbiz_googlesearched.rds")

# Look for any disrepancies between our business name and the google maps search
vanbiz_googlesearched <- read_rds("~/Developer/project_vanbiz/r/vanbiz_googlesearched.rds")
vanbiz_view <- vanbiz_googlesearched %>% select(BusinessName,goo_name,everything())

#tester function
google_query <- function(query_string) {
  link = paste0("https://maps.googleapis.com/maps/api/place/nearbysearch/json?",
                "location=","49.253386,-123.130621",
                "&radius=",50000,
                "&name=",(query_string %>% 
                  str_trim(side="both") %>% 
                  str_replace_all("[^[:alnum:] ]"," ") %>% 
                  str_replace_all(" ","+")),
                "&key=","AIzaSyDL7KVzfaUb-17Ew_cycTG0GoAlf9YammY")
  print(link)
  link %>% getURL() %>% fromJSON()
}
abc <- google_query("Karoleena Homes")

# Fill in the missing Latitude and Longitude columns of vanbiz_googlesearched with goo_lat and goo_lng
# Then, remove the goo_ columns
vanbiz_geocoded$vanbiz_googlesearched <- vanbiz_googlesearched %>% 
  mutate(Latitude = goo_lat,
         Longitude = goo_lng,
         geocode = "googlesearched") 

# Keep only vanbiz_hascoord and vanbiz_hasaddress_success
# vanbiz_googlesearch & vanbiz_googlesearched (the same 48 hits). 
# Not all addresses resolved nicely.
# combine vanbiz_hascoord and vanbiz_hasaddress_success

vanbiz_kept <- rbind(vanbiz_geocoded$vanbiz_hascoord,
                     vanbiz_geocoded$vanbiz_hasaddress_success)

nrow(vanbiz_kept) # 6101 businesses

# remove the geocode column which is a list. This would throw errors during geojson_write()
vanbiz_vancouver <- vanbiz_kept %>% 
  select(-geocode)

# write_rds(vanbiz_vancouver,
#           "~/Developer/project_vanbiz/r/vanbiz_vancouver.rds")
vanbiz_vancouver <- read_rds("~/Developer/project_vanbiz/r/vanbiz_vancouver.rds")

# Link to tutorial to convert r dataframe to geojson
#https://ropensci.org/tutorials/geojsonio_tutorial.html
library(rgdal)
library(geojsonio)

# geojson_json(vanbiz_kept[1:2,], lat = 'Latitude', lon = 'Longitude')
# geojson_write(vanbiz_vancouver, lat = 'Latitude', lon = 'Longitude',
#               file = "~/Developer/project_vanbiz/r/vanbiz_vancouver.geojson",
#               overwrite = T)
