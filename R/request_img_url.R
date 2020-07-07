create_search_filter <- function(idstring = "20171002_155842_0f32"){
  searchlist <- list(name = "PSScene4Band",
                     item_types = list("PSScene4Band"),
                     filter = list(
                       type = "StringInFilter",
                       field_name = "id",
                       config = list(idstring)
                     ))
  jsonlite::toJSON(searchlist)
  searchlist
}

request_img_url <- function(id_to_use){
  
  data_dir <- "/nfs/khondula-data/planetmethane/"
  itemtype <- "analytic_sr"
  
  message(glue("Trying to get {id_to_use}"))
  # if(!id_to_use %in% all_img_ids){
    
    search1 <- create_search_filter(idstring = id_to_use)
    r <- POST(planet_quicksearch_api,
              authenticate(api_key, password = ""),
              body = search1,
              encode = "json")
    
    # save returned content in an object called `r`.
    
    r_content <- content(r)
    feature_ids <- sapply(r_content$features, function(x) x[["id"]])
    feature_ids
    # activate
    i = 1
    assets_url <- r_content$features[[i]]$`_links`$assets
    r2 <- GET(url = assets_url, config = authenticate(api_key, password = ""))
    assets <- content(r2)
    
    if(!is.null(assets[[itemtype]])){
      activation_url <- assets[[itemtype]]$`_links`$activate
      activation_request <- GET(activation_url, authenticate(api_key, password = ""))
      http_status(activation_request)  
    } else {
      message(glue::glue("No Analytic SR item for {id_to_use}"))
    }
    
    # Once an asset is active, the response will contain a location. Use the location to download the asset.Re-do the GET request to the asset url, which now has a location
    
    assets_url <- r_content$features[[i]]$`_links`$assets
    r2 <- GET(url = assets_url, config = authenticate(api_key, password = ""))
    location_url <- content(r2)[[itemtype]]$location
    location_url
}

request_img_url2 <- function(id_to_use){
  
  data_dir <- "/nfs/khondula-data/planetmethane/"
  itemtype <- "analytic_sr"
  message(glue("Trying to get {id_to_use}"))

    search1 <- create_search_filter(idstring = id_to_use)
    r <- POST(planet_quicksearch_api,
              authenticate(api_key, password = ""),
              body = search1,
              encode = "json")
    
    # save returned content in an object called `r`.
    
    r_content <- content(r)
    feature_ids <- sapply(r_content$features, function(x) x[["id"]])
    feature_ids
    # activate
    i = 1
    assets_url <- r_content$features[[i]]$`_links`$assets
    r2 <- GET(url = assets_url, config = authenticate(api_key, password = ""))
    assets <- content(r2)
    
    if(!is.null(assets[[itemtype]])){
      activation_url <- assets[[itemtype]]$`_links`$activate
      activation_request <- GET(activation_url, authenticate(api_key, password = ""))
      http_status(activation_request)  
    } else {
      message(glue::glue("No Analytic SR item for {id_to_use}"))
    }
    
    # Once an asset is active, the response will contain a location. Use the location to download the asset.Re-do the GET request to the asset url, which now has a location
    
    assets_url <- r_content$features[[i]]$`_links`$assets
    r2 <- GET(url = assets_url, config = authenticate(api_key, password = ""))
    location_url <- content(r2)[[itemtype]]$location
    location_url
    
    message(glue("{location_url}"))
    filename <- glue("{data_dir}download_{itemtype}/{id_to_use}_{itemtype}.tiff")
    download.file(location_url, filename)
    message(glue::glue("{itemtype} downloaded for {id_to_use}"))
    
}