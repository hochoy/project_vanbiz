# Parsing Vancouver Business Licenses
David Choy  
2/26/2017  

![](cityofvan_opendata.png)  

***  

[See repo for a better markdown render of this page](https://github.com/hochoy/project_portfolio2/)  
  

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
source("~/Developer/project_portfolio2/custom_func.R") # I wrote a set of utility functions for quickly reading/writing tsv files and to play a sound when super-slow functions finish running.
```
  
### Read in the business license file (json) and having a quick look at the data  

Then, I downloaded the vancouver business licenses from the [City of Vancouver Open Data portal](http://data.vancouver.ca/datacatalogue/businessLicence.htm). I've chosen to use the json format of the file here but csv is also available at the vancouver open data website. After reading the data, I usually add an index column to keep track of rows before doing any manipulation. Additionally, it is a good idea to check the size of a dataset. If there are a million rows in a dataset that should only have 10,000 entries, there is a high chance that the file was not read in correctly.  
  

```r
json_path <- "~/Developer/project_portfolio2/business_licences.json"
vanbiz_raw <- fromJSON(json_path)
vanbiz_df <- vanbiz_raw$features$properties %>%
  mutate(index = 1:nrow(.)) %>% 
  select(index,everything()) 
vanbiz_df %>% nrow() # 61391 companies registered with the City of Vancouver. That's a lot!
```

```
## [1] 61391
```

####Finding dirt in the data####  

Already, we have stumbled on our first clumps of data dirt. As we can see below:  

1. some businesses do not have a registered address or geocoordinate
2. NumberOfEmployees was read in as a character, not an integer  


```r
vanbiz_df %>% str()
```

```
## 'data.frame':	61391 obs. of  25 variables:
##  $ index                : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ LicenceRSN           : chr  "2558952" "2558953" "2558954" "2558955" ...
##  $ LicenceNumber        : chr  "16-100147" "16-100148" "16-100149" "16-100150" ...
##  $ LicenceRevisionNumber: chr  "00" "00" "00" "00" ...
##  $ BusinessName         : chr  "Beaver Pond Creative Productions Inc " "Aimee Claire Mergaert (Aimee Mergaert)" "Carol Elinor Sawyer (Carol Sawyer)" "Third Dimension Studio Inc " ...
##  $ BusinessTradeName    : chr  NA "Aimee Mergaert Designs " NA NA ...
##  $ Status               : chr  "Issued" "Issued" "Issued" "Gone Out of Business" ...
##  $ IssuedDate           : chr  "2015-12-04 15:01:24" "2015-12-30 16:56:31" "2015-11-05 15:14:51" NA ...
##  $ ExpiredDate          : chr  "2016-12-31 00:00:00" "2016-12-31 00:00:00" "2016-12-31 00:00:00" NA ...
##  $ BusinessType         : chr  "Artist" "Artist" "Artist" "Artist" ...
##  $ BusinessSubType      : chr  NA NA NA NA ...
##  $ Unit                 : chr  NA NA NA NA ...
##  $ UnitType             : chr  NA NA NA NA ...
##  $ House                : chr  NA NA NA NA ...
##  $ Street               : chr  NA NA NA NA ...
##  $ City                 : chr  NA NA NA NA ...
##  $ Province             : chr  NA NA NA NA ...
##  $ Country              : chr  NA NA NA NA ...
##  $ PostalCode           : chr  NA NA NA NA ...
##  $ LocalArea            : chr  "08-Fairview" "15-Kensington-Cedar Cottage" "03-Strathcona" "15-Kensington-Cedar Cottage" ...
##  $ NumberOfEmployees    : chr  "000" "0" "1" "0" ...
##  $ FeePaid              : num  136 136 136 NA NA 136 136 136 NA NA ...
##  $ ExtractDate          : chr  "2016-12-23" "2016-12-23" "2016-12-23" "2016-12-23" ...
##  $ Latitude             : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ Longitude            : num  NA NA NA NA NA NA NA NA NA NA ...
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



After filtering, what we end up with is a list of 6149 companies with registered BC addresses and more than 10 employees (qualifying it as a Small-to-Medium Enterprise). These are the companies that we will focus on.  


```r
vanbiz_valid <- vanbiz_df %>% 
  filter(Province == "BC" | is.na(Province)) %>% 
  filter(NumberOfEmployees >= 10) %>% 
  filter(Status %in% c("Issued","Pending")) %>% 
  filter(!is.na(BusinessName))
nrow(vanbiz_valid) 
```

```
## [1] 6149
```


|BusinessName                        |Status |BusinessType      |House |Street       |Province | NumberOfEmployees|
|:-----------------------------------|:------|:-----------------|:-----|:------------|:--------|-----------------:|
|CHR Holdings Inc                    |Issued |Animal Services   |5980  |Miller Rd    |BC       |                12|
|Love's Auctioneers & Appraisers Ltd |Issued |Auctioneer        |2720  |No 5 Road    |BC       |                10|
|Tivoli Moving Picture Caterers Inc  |Issued |Caterer           |2190  |Badger Rd    |BC       |                10|
|Westcana Services Inc               |Issued |Personal Services |5550  |152nd Street |BC       |                62|
|FDM Software Ltd                    |Issued |Computer Services |949   |W 3rd St     |BC       |                30|
|Teraspan Networks Inc               |Issued |Computer Services |6870  |Palm Avenue  |BC       |                13|

***

### Check to see if these businesses have a valid coordinate that we can use with Mapbox GL JS


```r
vanbiz_latlongcheck <- vanbiz_valid %>% 
  mutate(hascoord = (!is.na(Latitude) & !is.na(Longitude))) %>% 
  mutate(hasaddress = (!is.na(House) & !is.na(Street)))

n_biz_wcoord <- vanbiz_latlongcheck %>% filter(hascoord) %>% nrow() # 5442 biz have coords -> READY
n_biz_nocoord <- vanbiz_latlongcheck %>% filter(!hascoord) %>% nrow() # 707 biz have no coords
n_biz_waddress <- vanbiz_latlongcheck %>% filter(!hascoord) %>% 
  filter(hasaddress) %>% nrow()                       # 665 biz have address -> GEOCODE
n_biz_noaddress <- vanbiz_latlongcheck %>% filter(!hascoord) %>% 
  filter(!hasaddress) %>% nrow()                       # 42 biz have no address -> Google API 
```

1. 5442 businesses have geo coordinates. These are ready to be converted into geojson.  
2. 707 businesses do not have geo coordinates, they will have to be geocoded.  
    + 665 of these have addresses, they will be ggmap-geocoded  
    + 42 of these do not have addresses, they will be GooglePlaces-geocoded  

ggmap-geocoding via google's api takes in a query as a full address instead of split into Unit, House number, street number, etc. My early attempts with geocoding failed when zipcodes were included as zipcodes often change or are inaccurate. In the following code chunk, I added one column to house the address query and another column to track if each business was successfully geocoded.


```r
# add full address and geocode columns. Needed later
vanbiz_pregeocoded <- vanbiz_latlongcheck %>% 
  mutate(full_address = "not set",
         geocode = "not run")

# write_rds(vanbiz_pregeocoded,"~/Developer/project_portfolio2/vanbiz_pregeocoded.rds")
```


### ggmap-geocoding: Creating a function for geocoding businesses with addresses

While creating a function for geocoding, I created a simple helper function below that could detect whether the geocode was sucessful.


```r
vanbiz_pregeocoded <- read_rds("~/Developer/project_portfolio2/vanbiz_pregeocoded.rds")

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
# write_rds(vanbiz_geocoded,"~/Developer/project_portfolio2/vanbiz_geocoded.rds")
```
  
Installing a [nominatim server/local instance that runs on OpenStreetMap](https://wiki.openstreetmap.org/wiki/Nominatim/Installation) is an alternative to using the `geocode` package or other hosted geocoding API.  

Update: BC now has a publicly available geocoder to try out!  

Google and other hosted APIs (i.e. mapquest) limit searches, geocoding and the use of the data on their servers and thus, is not feasable beyond this demo. However, google also has the Places Library that lets us find places via bizname when the address is not available. For now, we will use a mix of Google geocoding(for address geocoding) and Google Places (for business name geocoding)

### GoogleMapsApi-geocoding: Creating a function for geocoding businesses using business names

Businesses that do not have addresses that could be geocoded require querying via business name. For that, google's API appears to have the highest coverage via their Google Places system that helps businesses register themselves.  

The function below uses the `nearbysearch` API of google. Query requests of this type uses a simple url `"https://maps.googleapis.com/maps/api/place/nearbysearch/json?"` followed by fields such as location, radius, name as well as a mandatory User API key to limit queries. Besides generating a url query, my  function has a randomizer that ensures our requests occur at random intervals. This randomization serves no purpose at the moment as google only requires an API user to limit their rate of query. However, the intention was for it to simulate a human user. The last purpose of this function is to parse the query result and add the desired fields into our original dataframe.    


```r
vanbiz_geocoded <- read_rds("~/Developer/project_portfolio2/vanbiz_geocoded.rds")

View(vanbiz_geocoded$vanbiz_hascoord) # 5442 biz have valid coordinates
View(vanbiz_geocoded$vanbiz_hasaddress_success) # 659 biz have addresses that were successfully geocoded
View(vanbiz_geocoded$vanbiz_googlesearch) # 48 have no addresses or have addresses that were NOT sucessfully geocoded

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
#           path = "~/Developer/project_portfolio2/vanbiz_googlesearched.rds")
```

### Manual inspection of GoogleMapsAPI query results  

More often than not, there are disrepancies between the query result and the actual business information. Through this, I learned that even google is not perfect when it comes to data collection. Thus, it is always important to look at subsamples of the data before producing a final output.  

Below I have included a simple function to query a business name or string if I spot any suspicious google results. Preliminary view of the data shows that some businesses were sufficiently accurate but for simplicity and to save time, I have chosen to abandon the 48 googlesearched results. This leaves
me with the vanbiz_hascoord and vanbiz_hasaddress_success datasets.  


```r
# Look for any disrepancies between our business name and the google maps search
vanbiz_googlesearched <- read_rds("~/Developer/project_portfolio2/vanbiz_googlesearched.rds")
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
# abc <- google_query("Karoleena Homes")
kable(head(vanbiz_view))
```



|BusinessName                             |goo_name                                | index|LicenceRSN |LicenceNumber |LicenceRevisionNumber |BusinessTradeName |Status |IssuedDate          |ExpiredDate         |BusinessType      |BusinessSubType      |Unit |UnitType |House |Street |City   |Province |Country |PostalCode |LocalArea                    | NumberOfEmployees| FeePaid|ExtractDate | Latitude| Longitude|hascoord |hasaddress |full_address |geocode |  goo_lat|   goo_lng|goo_address                          |
|:----------------------------------------|:---------------------------------------|-----:|:----------|:-------------|:---------------------|:-----------------|:------|:-------------------|:-------------------|:-----------------|:--------------------|:----|:--------|:-----|:------|:------|:--------|:-------|:----------|:----------------------------|-----------------:|-------:|:-----------|--------:|---------:|:--------|:----------|:------------|:-------|--------:|---------:|:------------------------------------|
|HR Pacific Construction Management Ltd   |not found                               |  1047|2566726    |16-107916     |00                    |NA                |Issued |2016-02-09 14:35:31 |2016-12-31 00:00:00 |Contractor        |Building             |NA   |NA       |NA    |NA     |Golden |BC       |CAN     |V0A 1H0    |NA                           |                15|     205|2016-12-23  |       NA|        NA|FALSE    |FALSE      |not set      |not run |  0.00000|    0.0000|not found                            |
|BCAA Holdings Ltd                        |BCAA                                    |  9429|2587855    |16-129034     |00                    |NA                |Issued |2015-12-01 12:27:21 |2016-12-31 00:00:00 |Office            |Insurance Agent      |NA   |NA       |NA    |NA     |NA     |NA       |CAN     |V5G 4T1    |11-Arbutus Ridge             |                15|     136|2016-12-23  |       NA|        NA|FALSE    |FALSE      |not set      |not run | 49.25518| -122.9989|4567 Canada Way, Burnaby             |
|Martin Glotman Engineering Ltd           |Glotman Simpson Consulting Engineers    |  9442|2588054    |16-129233     |00                    |NA                |Issued |2015-12-07 11:21:09 |2016-12-31 00:00:00 |Office            |Engineer             |NA   |NA       |NA    |NA     |NA     |NA       |CAN     |V6J 1N5    |08-Fairview                  |                26|     136|2016-12-23  |       NA|        NA|FALSE    |FALSE      |not set      |not run | 49.26734| -123.1423|1661 West 5th Avenue, Vancouver      |
|Golden Properties Ltd                    |Golden Properties Ltd                   |  9443|2588057    |16-129236     |00                    |NA                |Issued |2015-12-29 09:54:40 |2016-12-31 00:00:00 |Office            |Property Management  |NA   |NA       |NA    |NA     |NA     |NA       |CAN     |V6E 2K3    |02-Central Business/Downtown |                12|     136|2016-12-23  |       NA|        NA|FALSE    |FALSE      |not set      |not run | 49.28874| -123.1217|1177 West Hastings Street, Vancouver |
|Sing Tao(canada) Ltd                     |Sing Tao Daily                          |  9455|2588364    |16-129543     |00                    |NA                |Issued |2016-01-04 09:55:09 |2016-12-31 00:00:00 |Office            |Publisher            |NA   |NA       |NA    |NA     |NA     |NA       |CAN     |V6P 3M2    |22-Marpole                   |               122|     153|2016-12-23  |       NA|        NA|FALSE    |FALSE      |not set      |not run | 49.27933| -123.1015|128 Keefer Street, Vancouver         |
|Xpera Risk Mitigation & Investigation LP |Xpera Risk Mitigation and Investigation | 13203|2608365    |16-149523     |00                    |NA                |Issued |2015-12-29 16:37:30 |2016-12-31 00:00:00 |Security Services |Security Consultants |NA   |NA       |NA    |NA     |NA     |NA       |NA      |NA         |NA                           |                50|     136|2016-12-23  |       NA|        NA|FALSE    |FALSE      |not set      |not run | 49.22284| -122.8331|17 Fawcett Road #225, Coquitlam      |

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

nrow(vanbiz_kept) # 6101 businesses
```

[1] 6101

```r
# remove the geocode column which is a list. This would throw errors during geojson_write()
vanbiz_vancouver <- vanbiz_kept %>% 
  select(-geocode)
kable(head(vanbiz_vancouver))
```



| index|LicenceRSN |LicenceNumber |LicenceRevisionNumber |BusinessName                 |BusinessTradeName            |Status |IssuedDate          |ExpiredDate         |BusinessType             |BusinessSubType |Unit |UnitType |House |Street     |City      |Province |Country |PostalCode |LocalArea    | NumberOfEmployees| FeePaid|ExtractDate | Latitude| Longitude|hascoord |hasaddress |full_address |
|-----:|:----------|:-------------|:---------------------|:----------------------------|:----------------------------|:------|:-------------------|:-------------------|:------------------------|:---------------|:----|:--------|:-----|:----------|:---------|:--------|:-------|:----------|:------------|-----------------:|-------:|:-----------|--------:|---------:|:--------|:----------|:------------|
| 15798|2559157    |16-100350     |00                    |Advanced Parking Systems Ltd |NA                           |Issued |2016-01-12 12:07:05 |2016-12-31 00:00:00 |Auto Parking Lot/Parkade |NA              |NA   |NA       |1019  |NELSON ST  |Vancouver |BC       |CAN     |V6E1J1     |01-West End  |               150|     136|2016-12-23  | 49.28191| -123.1266|TRUE     |TRUE       |not set      |
| 15832|2559191    |16-100384     |00                    |Diamond Parking Ltd          |DPS Diamond Parking Services |Issued |2016-02-19 14:29:53 |2016-12-31 00:00:00 |Auto Parking Lot/Parkade |NA              |NA   |NA       |865   |W Broadway |Vancouver |BC       |CAN     |V5Z 1J9    |08-Fairview  |                70|     136|2016-12-23  | 49.26365| -123.1235|TRUE     |TRUE       |not set      |
| 15833|2559192    |16-100385     |00                    |Diamond Parking Ltd          |DPS Diamond Parking Services |Issued |2016-02-16 10:45:20 |2016-12-31 00:00:00 |Auto Parking Lot/Parkade |NA              |2075 |Unit     |2085  |Yew St     |Vancouver |BC       |CAN     |           |07-Kitsilano |                70|     136|2016-12-23  | 49.26768| -123.1554|TRUE     |TRUE       |not set      |
| 15834|2559193    |16-100386     |00                    |Diamond Parking Ltd          |DPS Diamond Parking Services |Issued |2016-02-16 10:48:56 |2016-12-31 00:00:00 |Auto Parking Lot/Parkade |NA              |NA   |NA       |2412  |Laurel St  |Vancouver |BC       |CAN     |           |08-Fairview  |                70|     136|2016-12-23  | 49.26402| -123.1237|TRUE     |TRUE       |not set      |
| 15846|2559205    |16-100398     |00                    |Diamond Parking Ltd          |Diamond Parking Services     |Issued |2016-02-16 11:00:50 |2016-12-31 00:00:00 |Auto Parking Lot/Parkade |NA              |NA   |NA       |1022  |NELSON ST  |Vancouver |BC       |CAN     |V6E 4S7    |01-West End  |                70|     136|2016-12-23  | 49.28166| -123.1273|TRUE     |TRUE       |not set      |
| 15850|2559209    |16-100402     |00                    |Diamond Parking Ltd          |NA                           |Issued |2016-02-16 10:57:34 |2016-12-31 00:00:00 |Auto Parking Lot/Parkade |NA              |NA   |NA       |3588  |KINGSWAY   |Vancouver |BC       |CAN     |V5R 5L7    |21-Killarney |                10|     136|2016-12-23  | 49.23187| -123.0269|TRUE     |TRUE       |not set      |

```r
# write_rds(vanbiz_vancouver,
#           "~/Developer/project_portfolio2/vanbiz_vancouver.rds")
# vanbiz_vancouver <- read_rds("~/Developer/project_portfolio2/vanbiz_vancouver.rds")
```

### Finaly geojson conversion  
I used the tutorial below to convert the dataframe into a functioning geojson file which can be used directly by Mapbox GL JS in a web application. Here is the [link to the tutorial to convert r dataframe to geojson](https://ropensci.org/tutorials/geojsonio_tutorial.html). 


```r
suppressPackageStartupMessages(library(rgdal))
suppressPackageStartupMessages(library(geojsonio))

# geojson_json(vanbiz_kept[1:2,], lat = 'Latitude', lon = 'Longitude')
# geojson_write(vanbiz_vancouver, lat = 'Latitude', lon = 'Longitude',
#               file = "~/Developer/project_portfolio2/vanbiz_vancouver.geojson",
#               overwrite = T)
```

### Final result  

See my [mapbox visualization of Vancouver business licenses](https://hochoy.github.io/project_portfolio2/mapbox.html) using cleaned, filtered and geocoded data.  

***  





