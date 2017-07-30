# Parsing Vancouver Business Licenses
David Choy  
2/26/2017  

![](cityofvan_opendata.png)  

***  

See my [mapbox visualization of Vancouver business licenses](https://hochoy.github.io/project_vanbiz/mapbox.html) using cleaned, filtered and geocoded data.  
  

### Main objective  
In this project, we use business licenses from the [City of Vancouver's OpenData portal](http://data.vancouver.ca/datacatalogue/index.htm) to plot vancouver's businesses on a map. Before we do that, the data needs to be cleaned, corrected (if needed) and transformed into geojson format for Mapbox GL JS. Throughout this guide, I will be referring to errors/dirt in the data as "data dirt".
  
### Load R libraries
First, I load the required r libraries for this analysis. Since we are dealing with maps, json files, geocoding and web interaction, we will be needing a lot more libraries than usual.  
  


```r
library(jsonlite) # for reading json and writing geojson
library(ggplot2) # for general plotting and sanity check of the data
library(stringr) # for parsing and cleaning strings such as Business Names
library(readr) # for reading and writing .rds files which is a R formatted object file 
library(dplyr) # the god of simple data manipulation
library(ggmap) # plots maps from R code. in this case, I am only using the google geocoding feature
library(RCurl)
library(gridExtra) #arranging multiple plots together
# library(plotly) #plotly bar graph labels are broken
library(knitr) # for knitting the R markdown file into markdown and html
source("~/Developer/project_vanbiz/r/custom_func.R") # I wrote a set of utility functions for quickly reading/writing tsv files and to play a sound when super-slow functions finish running.
```
  
### Read in the business license file (json) and having a quick look at the data  

Then, I downloaded the 2016 vancouver business licenses from the [City of Vancouver Open Data portal](http://data.vancouver.ca/datacatalogue/businessLicence.htm). I've chosen to use the json format of the file here but csv is also available at the vancouver open data website. After reading the data, I usually add an index column to keep track of rows before doing any manipulation. Additionally, it is a good idea to check the size of a dataset. If there are a million rows in a dataset that should only have 10,000 entries, there is a high chance that the file was not read in correctly.  
  

```r
json_path <- "~/Developer/project_vanbiz/r/2016business_licences.json"
vanbiz_raw <- fromJSON(json_path)
vanbiz_df <- vanbiz_raw$features$properties %>%
  mutate(index = 1:nrow(.)) %>% 
  select(index,everything()) 
vanbiz_df %>% nrow() # 61399 companies registered with the City of Vancouver. That's a lot!
```

```
## [1] 61399
```

####Finding dirt in the data####  

Already, we have stumbled on our first clumps of data dirt. As we can see below:  

1. some businesses do not have a registered address or geocoordinate
2. NumberOfEmployees was read in as a character, not an integer  


```r
vanbiz_df %>% str()
```

```
## 'data.frame':	61399 obs. of  25 variables:
##  $ index                : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ LicenceRSN           : chr  "2827400" "2827855" "2827440" "2836504" ...
##  $ LicenceNumber        : chr  "16-301757" "16-302202" "16-301796" "16-308971" ...
##  $ LicenceRevisionNumber: chr  "00" "00" "00" "00" ...
##  $ BusinessName         : chr  "Mingxia Wang & Yicheng Xie " "LTP Productions Inc " "Everbright Developments Inc " "Qi Bo Wang (Qi Wang)" ...
##  $ BusinessTradeName    : chr  NA NA NA NA ...
##  $ Status               : chr  "Pending" "Issued" "Issued" "Issued" ...
##  $ IssuedDate           : chr  NA "2016-10-28 11:01:01" "2016-10-28 09:36:09" "2016-11-14 12:58:23" ...
##  $ ExpiredDate          : chr  NA "2016-12-31 00:00:00" "2016-12-31 00:00:00" "2016-12-31 00:00:00" ...
##  $ BusinessType         : chr  "Secondary Suite - Permanent" "Electrical-Temporary (Filming)" "Contractor" "One-Family Dwelling" ...
##  $ BusinessSubType      : chr  "Primary (1 Rental) and Laneway" NA "Building" "Primary (1 Rental)" ...
##  $ Unit                 : chr  NA NA NA NA ...
##  $ UnitType             : chr  NA NA NA NA ...
##  $ House                : chr  NA "2601" "3007" NA ...
##  $ Street               : chr  NA "Lougheed Hwy" "Plateau Blvd" NA ...
##  $ City                 : chr  NA "Coquitlam" "Coquitlam" NA ...
##  $ Province             : chr  NA "BC" "BC" NA ...
##  $ Country              : chr  NA "CAN" "CAN" NA ...
##  $ PostalCode           : chr  NA "V3C 4J2" "V3E 2T1" NA ...
##  $ LocalArea            : chr  "05-Hastings-Sunrise" NA NA "18-Oakridge" ...
##  $ NumberOfEmployees    : chr  "000" "000" "2" "000" ...
##  $ Latitude             : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ Longitude            : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ FeePaid              : num  NA 95 94 65 3017 ...
##  $ ExtractDate          : chr  "2017-07-28" "2017-07-28" "2017-07-28" "2017-07-28" ...
```
  
### Fixing integers that are read as characters  
  
Changing the NumberOfEmployees column from a "character" class back to "integer" class is as simple as using the function `as.integer()`. As can be seen in the previous `str()` output, this error was caused by the presence of oddly-formatted numbers such as "000" or "01".  


```r
vanbiz_df <- vanbiz_df%>% 
  mutate(NumberOfEmployees = as.integer(NumberOfEmployees)) %>% 
  mutate(Country = sapply(Country, function(x){ifelse(x == "CAN",
                                                      "Canada",
                                                      x)}))
```

### Filtering the data  

When tackling any big dataset, the first question to ask is if we can focus our analysis at a smaller subset. In this case, I want to find local companies that are likely to hire new graduates this year. Using this as my guide, I chose the following filters:  

1. Keep only businesses with pending or active licenses (can't hire if you're not licensed!)
2. Keep only businesses with more than 10 employees (as they are more likely to hire)  
3. Keep only businesses that have an address in BC  




#### Company status  
![](parse_vanbiz_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

####Companies in Canada  

![](parse_vanbiz_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

####Companies from USA  

![](parse_vanbiz_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

####Companies from other countries  

![](parse_vanbiz_files/figure-html/unnamed-chunk-6-1.png)<!-- -->



After filtering, what we end up with is a list of 6131 companies with registered BC addresses and more than 10 employees (qualifying it as a Small-to-Medium Enterprise). These are the companies that we will focus on.  


```r
vanbiz_valid <- vanbiz_df %>% 
  filter(Province == "BC" | is.na(Province)) %>% 
  filter(NumberOfEmployees >= 10) %>% 
  filter(Status %in% c("Issued","Pending")) %>% 
  filter(!is.na(BusinessName))
nrow(vanbiz_valid) 
```

```
## [1] 6131
```


|BusinessName                                      |Status |BusinessType                |House |Street      |Province | NumberOfEmployees|
|:-------------------------------------------------|:------|:---------------------------|:-----|:-----------|:--------|-----------------:|
|Artcraft Display Graphics Inc                     |Issued |Contractor - Special Trades |1533  |Broadway St |BC       |                12|
|Britco LP                                         |Issued |Contractor                  |20091 |91A Ave     |BC       |               330|
|Cobra Interiors Ltd                               |Issued |Contractor                  |2401  |United Blvd |BC       |                20|
|Jamieson's Pet Food Distributors Ltd              |Issued |Wholesale  Dealer           |7471  |Vantage Way |BC       |                15|
|Trotter and Morton Electrical Constructors BC Ltd |Issued |Electrical Contractor       |5151  |Canada Way  |BC       |                65|
|Canem Systems Ltd                                 |Issued |Electrical Contractor       |1600  |VALMONT WAY |BC       |                80|

***

### Check to see if these businesses have a valid coordinate that we can use with Mapbox GL JS


```r
vanbiz_latlongcheck <- vanbiz_valid %>% 
  mutate(hascoord = (!is.na(Latitude) & !is.na(Longitude))) %>% 
  mutate(hasaddress = (!is.na(House) & !is.na(Street)))

n_biz_wcoord <- vanbiz_latlongcheck %>% filter(hascoord) %>% nrow() # 5426 biz have coords -> READY
n_biz_nocoord <- vanbiz_latlongcheck %>% filter(!hascoord) %>% nrow() # 705 biz have no coords
n_biz_waddress <- vanbiz_latlongcheck %>% filter(!hascoord) %>% 
  filter(hasaddress) %>% nrow()                       # 663 biz have address -> GEOCODE
n_biz_noaddress <- vanbiz_latlongcheck %>% filter(!hascoord) %>% 
  filter(!hasaddress) %>% nrow()                       # 42 biz have no address -> Google API 
```

1. 5426 businesses have geo coordinates. These are ready to be converted into geojson.  
2. 705 businesses do not have geo coordinates, they will have to be geocoded.  
    + 663 of these have addresses, they will be ggmap-geocoded  
    + 42 of these do not have addresses, they will be GooglePlaces-geocoded  

ggmap-geocoding via google's api takes in a query as a full address instead of split into Unit, House number, street number, etc. My early attempts with geocoding failed when zipcodes were included as zipcodes often change or are inaccurate. In the following code chunk, I added one column to house the address query and another column to track if each business was successfully geocoded.


```r
# add full address and geocode columns. Needed later
vanbiz_pregeocoded <- vanbiz_latlongcheck %>% 
  mutate(full_address = "not set",
         geocode = "not run")

# write_rds(vanbiz_pregeocoded,"~/Developer/project_vanbiz/r/vanbiz_pregeocoded.rds")
```


### ggmap-geocoding: Creating a function for geocoding businesses with addresses

While creating a function for geocoding, I created a simple helper function below that could detect whether the geocode was sucessful.


```r
vanbiz_pregeocoded <- read_rds("~/Developer/project_vanbiz/r/vanbiz_pregeocoded.rds")

#helper function
isGeocoded <- function(geocode_list) {
  temp_vec <- vector()
  for (i in 1:length(geocode_list)) {
    temp_vec[i] <- !is.na(geocode_list[[i]]$lon)
  }
  temp_vec
}
```

The ggmap-geocoding function itself is rather long and has 6 main parts:  

1. The first part filters out businesses that were previously geocoded  
2. The second splits the businesses dataframe into businesses that don't need geocoding (has coord), businesses that have an address and business that have neither.  
3. Because ggmap runs on google's api, there is a limit of 2500 queries per user per day and thus, we have a check for the number of queries remaining.  
4. The fourth is the actual geocoding section and here parts of an address are pasted together to form a full address for querying.  
5. After the "has address" list is geocoded, we check for businesses that failed to geocode.
6. Successful geocoded business enter the "vanbizhasaddress_success" dataframe while businesses that failed to geocode based on address


```r
geocode_biz <- function(vanbiz_df) {
  
  # 1. filter for rows that need geocoding work
  vanbiz_todo <- vanbiz_df %>% 
    filter(geocode == "not run")
  
  # 2. filter df for geocoding and google search api
  vanbiz_hascoord <- vanbiz_todo %>% 
    filter(hascoord) %>% 
    mutate(geocode = "has coord")
  vanbiz_hasaddress <- vanbiz_todo %>% 
    filter(!hascoord) %>% 
    filter(hasaddress)
  vanbiz_noaddress <- vanbiz_todo %>% 
    filter(!hascoord) %>% 
    filter(!hasaddress)
  
  # 3. break the function if there are not enough geocoding queries remaining
  if ((nrow(vanbiz_hasaddress) + nrow(vanbiz_noaddress)) > 
      as.integer(str_extract(geocodeQueryCheck(),"[0-9]+"))) {
    print("2500 geocode limit reached")
    break
  }
  
  # 4. geocode vanbiz_hasaddress
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
  
  # 5. check for failed geocoding
  vanbiz_googlesearch <- vanbiz_noaddress %>% 
    rbind(vanbiz_hasaddress_failed)
  
  #display remaining query
  print(geocodeQueryCheck())
  # 6. final output
  list(vanbiz_hascoord = vanbiz_hascoord,
       vanbiz_hasaddress_success = vanbiz_hasaddress_success,
       vanbiz_googlesearch = vanbiz_googlesearch)
}

# vanbiz_geocoded <- geocode_biz(vanbiz_pregeocoded)
# write_rds(vanbiz_geocoded,"~/Developer/project_vanbiz/r/vanbiz_geocoded.rds")
```
  
Installing a [nominatim server/local instance that runs on OpenStreetMap](https://wiki.openstreetmap.org/wiki/Nominatim/Installation) is an alternative to using the `geocode` package or other hosted geocoding API.  

Update: BC now has a publicly available geocoder to try out!  

Google and other hosted APIs (i.e. mapquest) limit searches, geocoding and the use of the data on their servers and thus, is not feasable beyond this demo. However, google also has the Places Library that lets us find places via bizname when the address is not available. For now, we will use a mix of Google geocoding(for address geocoding) and Google Places (for business name geocoding)

### GoogleMapsApi-geocoding: Creating a function for geocoding businesses using business names

Businesses that do not have addresses that could be geocoded require querying via business name. For that, google's API appears to have the highest coverage via their Google Places system that helps businesses register themselves.  

The function below uses the `nearbysearch` API of google. Query requests of this type uses a simple url `"https://maps.googleapis.com/maps/api/place/nearbysearch/json?"` followed by fields such as location, radius, name as well as a mandatory User API key to limit queries. Besides generating a url query, my  function has a randomizer that ensures our requests occur at random intervals. This randomization serves no purpose at the moment as google only requires an API user to limit their rate of query. However, the intention was for it to simulate a human user. The last purpose of this function is to parse the query result and add the desired fields into our original dataframe.    


```r
vanbiz_geocoded <- read_rds("~/Developer/project_vanbiz/r/vanbiz_geocoded.rds")

View(vanbiz_geocoded$vanbiz_hascoord) # 5426 biz have valid coordinates
View(vanbiz_geocoded$vanbiz_hasaddress_success) # 659 biz have addresses that were successfully geocoded
View(vanbiz_geocoded$vanbiz_googlesearch) # 46 have no addresses or have addresses that were NOT sucessfully geocoded

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


# vanbiz_googlesearched <- google_maps_search(query_df=vanbiz_geocoded$vanbiz_googlesearch)
# write_rds(vanbiz_googlesearched,
          # path = "~/Developer/project_vanbiz/r/vanbiz_googlesearched.rds")
```

### Manual inspection of GoogleMapsAPI query results  

More often than not, there are disrepancies between the query result and the actual business information. Through this, I learned that even google is not perfect when it comes to data collection. Thus, it is always important to look at subsamples of the data before producing a final output.  

Below I have included a simple function to query a business name or string if I spot any suspicious google results. Preliminary view of the data shows that some businesses were sufficiently accurate but for simplicity and to save time, I have chosen to abandon the 46 googlesearched results. This leaves
me with the vanbiz_hascoord and vanbiz_hasaddress_success datasets.  


```r
# Look for any disrepancies between our business name and the google maps search
vanbiz_googlesearched <- read_rds("~/Developer/project_vanbiz/r/vanbiz_googlesearched.rds")
vanbiz_view <- vanbiz_googlesearched %>% select(BusinessName,goo_name,everything())
```




```r
kable(head(vanbiz_view))
```



|BusinessName                         |goo_name                      | index|LicenceRSN |LicenceNumber |LicenceRevisionNumber |BusinessTradeName                    |Status |IssuedDate          |ExpiredDate         |BusinessType      |BusinessSubType  |Unit |UnitType |House |Street |City |Province |Country |PostalCode |LocalArea   | NumberOfEmployees| Latitude| Longitude| FeePaid|ExtractDate |hascoord |hasaddress |full_address |geocode |  goo_lat|   goo_lng|goo_address                              |
|:------------------------------------|:-----------------------------|-----:|:----------|:-------------|:---------------------|:------------------------------------|:------|:-------------------|:-------------------|:-----------------|:----------------|:----|:--------|:-----|:------|:----|:--------|:-------|:----------|:-----------|-----------------:|--------:|---------:|-------:|:-----------|:--------|:----------|:------------|:-------|--------:|---------:|:----------------------------------------|
|Viking Security Services Inc         |Viking Fire Protection Inc    |  1390|2608450    |16-149608     |00                    |NA                                   |Issued |2016-03-27 22:57:56 |2016-12-31 00:00:00 |Security Services |Security Patrol  |NA   |NA       |NA    |NA     |NA   |NA       |NA      |NA         |NA          |                10|       NA|        NA|     176|2017-07-28  |FALSE    |FALSE      |not set      |not run | 49.18872| -122.9867|7885 North Fraser Way, Unit 140, Burnaby |
|Falcon Security Inc                  |Falcon Security Inc.          |  1440|2608408    |16-149566     |00                    |NA                                   |Issued |2015-11-13 12:04:17 |2016-12-31 00:00:00 |Security Services |Security Guard   |NA   |NA       |NA    |NA     |NA   |NA       |NA      |NA         |22-Marpole  |                15|       NA|        NA|     136|2017-07-28  |FALSE    |FALSE      |not set      |not run | 49.20403| -123.1341|1200 West 73rd Avenue, Vancouver         |
|Action Lock & Safe Ltd               |Action Lock & Security        |  1441|2608407    |16-149565     |00                    |Action Integrated Security Solutions |Issued |2015-12-23 14:24:50 |2016-12-31 00:00:00 |Security Services |Retail Alarms    |NA   |NA       |NA    |NA     |NA   |NA       |NA      |NA         |22-Marpole  |                24|       NA|        NA|     136|2017-07-28  |FALSE    |FALSE      |not set      |not run | 49.20486| -123.1344|8866 Hudson Street, Vancouver            |
|Source Security & Investigations Inc |not found                     |  3313|2608461    |16-149619     |00                    |NA                                   |Issued |2016-02-04 08:59:40 |2016-12-31 00:00:00 |Security Services |Other            |NA   |NA       |NA    |NA     |NA   |NA       |NA      |NA         |NA          |                45|       NA|        NA|     176|2017-07-28  |FALSE    |FALSE      |not set      |not run |  0.00000|    0.0000|not found                                |
|(Sean O'Leary)                       |Sean O George Law Corporation |  3334|2608448    |16-149606     |00                    |Safetech Alarm Systems               |Issued |2015-11-16 08:04:52 |2016-12-31 00:00:00 |Security Services |Alarm Monitoring |NA   |NA       |NA    |NA     |NA   |NA       |NA      |NA         |NA          |                26|       NA|        NA|     136|2017-07-28  |FALSE    |FALSE      |not set      |not run | 49.28499| -123.1108|375 Water Street, Vancouver              |
|Premier Security Inc                 |Premier Security Inc          |  3359|2608418    |16-149576     |00                    |NA                                   |Issued |2015-11-12 16:46:46 |2016-12-31 00:00:00 |Security Services |Security Patrol  |NA   |NA       |NA    |NA     |NA   |NA       |NA      |NA         |08-Fairview |               140|       NA|        NA|     136|2017-07-28  |FALSE    |FALSE      |not set      |not run | 49.26369| -123.1278|1055 West Broadway, Vancouver            |

### Producing the final dataframe  

Now that we have done our best to geocode the business entries, it is time to recombine the 2 remaining datasets (the 3rd dataset, vanbiz_googlesearched, was abandoned as it had 48 entries only which were questionable google results). To do that, we have to make sure all our fields/columns match between dataframes. Below, we transfer the google-geocoded latitudes and longitudes into the proper Latitude and Longitude columns.

*Fill in the missing Latitude and Longitude columns of vanbiz_googlesearched with goo_lat and goo_lng. Then, remove the goo_ columns*  


```r
vanbiz_geocoded$vanbiz_googlesearched <- vanbiz_googlesearched %>% 
  mutate(Latitude = goo_lat,
         Longitude = goo_lng,
         geocode = "googlesearched") 

# Keep only vanbiz_hascoord and vanbiz_hasaddress_success
# vanbiz_googlesearch & vanbiz_googlesearched (the same 48 hits). 
# Not all addresses resolved nicely.
# combine vanbiz_hascoord and vanbiz_hasaddress_success
```

Then, we combine the 2 datasets via `rbind()`. We also remove the geocode column as it is a `list`, not as `vector` and that would break the conversion from dataframe to json in the next step.  


```r
vanbiz_kept <- rbind(vanbiz_geocoded$vanbiz_hascoord,
                     vanbiz_geocoded$vanbiz_hasaddress_success)

nrow(vanbiz_kept) # 6085 businesses
```

[1] 6085

```r
# remove the geocode column which is a list. This would throw errors during geojson_write()
vanbiz_vancouver <- vanbiz_kept %>% 
  select(-geocode)
kable(head(vanbiz_vancouver))
```



| index|LicenceRSN |LicenceNumber |LicenceRevisionNumber |BusinessName                      |BusinessTradeName             |Status  |IssuedDate          |ExpiredDate         |BusinessType                   |BusinessSubType     |Unit      |UnitType |House |Street           |City      |Province |Country |PostalCode |LocalArea                    | NumberOfEmployees| Latitude| Longitude| FeePaid|ExtractDate |hascoord |hasaddress |full_address |
|-----:|:----------|:-------------|:---------------------|:---------------------------------|:-----------------------------|:-------|:-------------------|:-------------------|:------------------------------|:-------------------|:---------|:--------|:-----|:----------------|:---------|:--------|:-------|:----------|:----------------------------|-----------------:|--------:|---------:|-------:|:-----------|:--------|:----------|:------------|
| 15638|2841809    |16-310787     |00                    |Powell Street Holdings Ltd        |Incendio                      |Pending |NA                  |NA                  |Restaurant Class 1             |No Liquor Service   |NA        |NA       |103   |Columbia St      |Vancouver |BC       |Canada  |V6A 2R4    |02-Central Business/Downtown |                10| 49.28346| -123.1025|     136|2017-07-28  |TRUE     |TRUE       |not set      |
| 15696|2842606    |16-311311     |00                    |Oneplus International Corp        |NA                            |Issued  |2016-11-24 16:02:37 |2016-12-31 00:00:00 |Office                         |Advertising Agent   |2400      |Unit     |1055  |W GEORGIA ST     |Vancouver |BC       |Canada  |NA         |02-Central Business/Downtown |                20| 49.28542| -123.1219|      65|2017-07-28  |TRUE     |TRUE       |not set      |
| 15720|2750486    |16-125276     |01                    |The Secret Garden Tea Company Inc |NA                            |Issued  |2016-09-08 12:16:23 |2016-12-31 00:00:00 |Restaurant Class 1             |No Liquor Service   |NA        |NA       |2138  |W 40TH AV        |Vancouver |BC       |Canada  |V6M 1W5    |11-Arbutus Ridge             |                20| 49.23525| -123.1560|      32|2017-07-28  |TRUE     |TRUE       |not set      |
| 15752|2770672    |16-294785     |00                    |Body Energy Club Ltd              |NA                            |Pending |NA                  |NA                  |Ltd Service Food Establishment |NA                  |1st Floor |Unit     |1131  |W GEORGIA ST     |Vancouver |BC       |Canada  |V6E 3G4    |02-Central Business/Downtown |                12| 49.28632| -123.1234|      53|2017-07-28  |TRUE     |TRUE       |not set      |
| 15756|2753017    |16-277177     |00                    |Angelic Productions Ltd           |Angel In Training             |Issued  |2016-08-30 11:40:10 |2016-12-31 00:00:00 |Production Company             |Film Production     |NA        |NA       |1950  |FRANKLIN ST      |Vancouver |BC       |Canada  |V5L 1R2    |04-Grandview-Woodland        |                40| 49.28178| -123.0640|     110|2017-07-28  |TRUE     |TRUE       |not set      |
| 15764|2753071    |16-277231     |00                    |1085706 BC Ltd                    |Sunrise Pizza and Steak House |Issued  |2016-09-20 15:31:48 |2016-12-31 00:00:00 |Restaurant Class 1             |With Liquor Service |NA        |NA       |949   |COMMERCIAL DRIVE |Vancouver |BC       |Canada  |V5L 3W8    |04-Grandview-Woodland        |                15| 49.27592| -123.0699|     136|2017-07-28  |TRUE     |TRUE       |not set      |

```r
# write_rds(vanbiz_vancouver,
#           "~/Developer/project_vanbiz/r/vanbiz_vancouver.rds")
# vanbiz_vancouver <- read_rds("~/Developer/project_vanbiz/r/vanbiz_vancouver.rds")
```

### Finaly geojson conversion  
I used the tutorial below to convert the dataframe into a functioning geojson file which can be used directly by Mapbox GL JS in a web application. Here is the [link to the tutorial to convert r dataframe to geojson](https://ropensci.org/tutorials/geojsonio_tutorial.html). 


```r
suppressPackageStartupMessages(library(rgdal))
suppressPackageStartupMessages(library(geojsonio))

geojson_json(vanbiz_kept[1:2,], lat = 'Latitude', lon = 'Longitude')
```

```
## {"type":"FeatureCollection","features":[{"type":"Feature","geometry":{"type":"Point","coordinates":[-123.1024973,49.2834646]},"properties":{"index":15638,"LicenceRSN":"2841809","LicenceNumber":"16-310787","LicenceRevisionNumber":"00","BusinessName":"Powell Street Holdings Ltd ","BusinessTradeName":"Incendio ","Status":"Pending","IssuedDate":null,"ExpiredDate":null,"BusinessType":"Restaurant Class 1","BusinessSubType":"No Liquor Service","Unit":null,"UnitType":null,"House":"103","Street":"Columbia St","City":"Vancouver","Province":"BC","Country":"Canada","PostalCode":"V6A 2R4","LocalArea":"02-Central Business/Downtown","NumberOfEmployees":10,"FeePaid":136,"ExtractDate":"2017-07-28","hascoord":true,"hasaddress":true,"full_address":"not set","geocode":"has coord"}},{"type":"Feature","geometry":{"type":"Point","coordinates":[-123.121943,49.2854191]},"properties":{"index":15696,"LicenceRSN":"2842606","LicenceNumber":"16-311311","LicenceRevisionNumber":"00","BusinessName":"Oneplus International Corp ","BusinessTradeName":null,"Status":"Issued","IssuedDate":"2016-11-24 16:02:37","ExpiredDate":"2016-12-31 00:00:00","BusinessType":"Office","BusinessSubType":"Advertising Agent","Unit":"2400","UnitType":"Unit","House":"1055","Street":"W GEORGIA ST","City":"Vancouver","Province":"BC","Country":"Canada","PostalCode":null,"LocalArea":"02-Central Business/Downtown","NumberOfEmployees":20,"FeePaid":65,"ExtractDate":"2017-07-28","hascoord":true,"hasaddress":true,"full_address":"not set","geocode":"has coord"}}]}
```

```r
geojson_write(vanbiz_vancouver, lat = 'Latitude', lon = 'Longitude',
              file = "~/Developer/project_vanbiz/r/vanbiz_vancouver.geojson",
              overwrite = T)
```

```
## Success! File is at /Users/davidchoy/Developer/project_vanbiz/r/vanbiz_vancouver.geojson
```

```
## <geojson>
##   Path:       ~/Developer/project_vanbiz/r/vanbiz_vancouver.geojson
##   From class: data.frame
```

### Final result  

See my [mapbox visualization of Vancouver business licenses](https://hochoy.github.io/project_vanbiz/mapbox.html) using cleaned, filtered and geocoded data.  

***  





