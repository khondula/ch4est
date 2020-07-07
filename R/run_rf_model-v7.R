
library(rsample)
library(stars)
library(glue)
library(fs)
library(velox)
library(caret)
library(raster)
library(dplyr)
library(sf)
library(readr)
library(ranger)
library(h2o)
# imgs_file <- nomodels_imgs_files[1]
# imgs_file <- grep("20180714_151447_1051", nomodels_imgs_files, value = TRUE)
run_rf_model <- function(imgs_file){
  # set up
  thisimgID <- imgs_file %>% basename() %>% tools::file_path_sans_ext()
  data_dir <- "/nfs/khondula-data/planetmethane"
  img_extract_dir <- glue("{data_dir}/stars/traindata-v4")
  planet_crs <- "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  band_names <- c("b1_blue", "b2_green", "b3_red", "b4_NIR",
                  "NDVI", "NDWI", "HUE", "SAT", "VAL", "LUM", "CHROMA", "HUE2")
  gb_ws_file <- glue("{data_dir}/polygons/Greensboro_watershed.shp")
  training_v4_file <- glue("{data_dir}/polygons/training_v4.shp")
  
  # test for overlap of image and watershed
  myimg_extent <- raster::stack(imgs_file) %>% 
    raster::extent() %>% 
    as("SpatialPolygons") %>% st_as_sf()
  st_crs(myimg_extent) <- proj4string(raster::stack(imgs_file))
  greensboro <- st_read(gb_ws_file) %>% st_transform(planet_crs)
  
  mat1 <- st_intersects(myimg_extent, greensboro, sparse = FALSE)
  gbs_in_extent <- which(apply(mat1, 1, any))
  gb_extent <- myimg_extent[gbs_in_extent,]
  message(glue("Overlap of {thisimgID} is {nrow(gb_extent)>0}"))
  
  if(nrow(gb_extent)>0){
    
  # get mvdeps raster anyway
  mvdeps_gb_buffer20m_filename <- glue("{data_dir}/polygons/mvdeps_v3.shp")
  mvdeps_gb_buffer20m <- st_read(mvdeps_gb_buffer20m_filename)
  mvdeps_sp <- sf::as_Spatial(st_geometry(mvdeps_gb_buffer20m),
                              IDs = as.character(mvdeps_gb_buffer20m[["dep_id"]])) 
  # read in THIS image
  myimg_prox <- stars::read_stars(imgs_file, proxy = TRUE)
  # crop to watershed
  myimg_greens_prox <- st_crop(myimg_prox, greensboro)
  # plot(myimg_greens_prox)
  message(glue("converting stars proxy to stars img {thisimgID}"))
  myimg_greens <- st_as_stars(myimg_greens_prox)
  
  # extract values for training polygons
  # subset stars objects using sf objects!
  myimg_mvdeps <- myimg_greens[mvdeps_gb_buffer20m]
  # then convert to rasters and label bands
  myimg_mvdeps_r <- as(myimg_mvdeps, "Raster")
  names(myimg_mvdeps_r) <- band_names
  
  # check for extract data in directory
  extract2_filename <- glue("{img_extract_dir}/{thisimgID}.csv")
  extracted_already <- file.exists(extract2_filename)
  
  if(!extracted_already){
  # read in spatial data  
  # read in training data for upland and water classes polygons
  train_polygons <- st_read(training_v4_file)
  train_water <- dplyr::filter(train_polygons, 
                               trainclass == "water")
  train_upland <- dplyr::filter(train_polygons, 
                                trainclass == "upland")
  # convert sf objects to sp for polygon IDs
  train_water_sp <- sf::as_Spatial(st_geometry(train_water),
            IDs = as.character(train_water[["trainid"]])) 
  train_upland_sp <- sf::as_Spatial(st_geometry(train_upland),
              IDs = as.character(train_upland[["trainid"]])) 

  message(glue("subset stars and convert to raster {thisimgID}"))
  # extract values for training polygons
  # subset stars objects using sf objects!
  # myimg_greens_r <- as(myimg_greens, "Raster")
  myimg_water <- myimg_greens[train_water]
  myimg_upland <- myimg_greens[train_upland]
  # then convert to rasters and label bands
  myimg_water_r <- as(myimg_water, "Raster")
  myimg_upland_r <- as(myimg_upland, "Raster")
  names(myimg_water_r) <- band_names
  names(myimg_upland_r) <- band_names
  
  message(glue("extracting with velox for image {thisimgID}"))
  # do value extract with velox
  # water
  img_vxW <- velox::velox(myimg_water_r)
  img_extractW <- img_vxW$extract(train_water_sp, df = TRUE)
  if(nrow(img_extractW)==0){
    message(glue("NO WATER OVERLAP IN {thisimgID}"))
  }
  if(nrow(img_extractW)>0){
  names(img_extractW)[2:13] <- band_names
  img_extract_narm_water <- img_extractW %>% 
    dplyr::filter(!is.na(NDWI)) %>% 
    mutate(imgID = thisimgID)
  img_extract_narm_water <- img_extract_narm_water %>% 
    dplyr::mutate(trainclass = "water")
  
  # upland
  img_vxU <- velox::velox(myimg_upland_r)
  img_extractU <- img_vxU$extract(train_upland_sp, df = TRUE)
  names(img_extractU)[2:13] <- band_names
  img_extract_narm_upland <- img_extractU %>% 
    dplyr::filter(!is.na(NDWI)) %>% 
    mutate(imgID = thisimgID)
  img_extract_narm_upland <- img_extract_narm_upland %>% 
    dplyr::mutate(trainclass = "upland")
  
  img_extract_narm <- rbind(img_extract_narm_water,
                            img_extract_narm_upland)
  # save extracted data
  readr::write_csv(img_extract_narm, extract2_filename)
  }
  message(glue("data extracted for {thisimgID}"))
  }
  if(file.exists(extract2_filename)){
    
  img_extract_narm <- readr::read_csv(extract2_filename)
  # img_extract_narm <- vroom::vroom(extract2_filename)
  img_extract_narm_water <- img_extract_narm %>%
    dplyr::filter(trainclass == "water")
  img_extract_narm_upland <- img_extract_narm %>%
    dplyr::filter(trainclass == "upland")
  rm(img_extract_narm)
  # prep rf model training data
  # number of water pixels 
  npixW <- img_extract_narm_water %>% nrow()
  # sample same number from upland
  upland_subset <- img_extract_narm_upland %>%
    dplyr::sample_n(size = npixW)
  img_extract_narm_subset <- img_extract_narm_water %>%
    rbind(upland_subset)
  img_extract_rf_data <- img_extract_narm_subset %>%
    dplyr::select(-ID_sp, -imgID, -HUE, -HUE2, -VAL) %>% 
    na.omit() %>% 
    mutate(trainclass = factor(trainclass, levels = c("water", "upland")))
  
  myvals_split <- initial_split(img_extract_rf_data, 
                                prop = .7, strata = trainclass)
  my_train <- training(myvals_split)
  my_test  <- testing(myvals_split)
  
  my_model_id <- Sys.time() %>% gsub("[[:punct:]]", "", .) %>% gsub(" ", "_", .)
  my_model_id <- glue("{thisimgID}_at{my_model_id}")
  message(glue("starting to train model for {thisimgID}"))
  # H2O model
  # localH2O = h2o.init(min_mem_size = "5G")
  h2o.init(max_mem_size = "25g")
  train.h2o <- as.h2o(my_train)
  
  y <- "trainclass"
  x <- setdiff(names(my_train), y)
  
  hyper_grid.h20 <- list(
    ntrees = seq(200, 500, by = 100),
    mtries = c(3,5,7),
    # max_depth = seq(10, 30, by = 10),
    min_rows = c(1, 3, 5),
    sample_rate = c(0.70, 0.75, 0.80)
  )
  search_criteria <- list(
    strategy = "RandomDiscrete",
    stopping_metric = "logloss",
    stopping_tolerance = 0.005,
    stopping_rounds = 10,
    max_runtime_secs = 60*60
  )

  this_grid_id <- glue("rf_grid_{my_model_id}")
  grid <- h2o.grid(algorithm = "randomForest",
                   grid_id = this_grid_id,
                   x = x,
                   y = y,
                   training_frame = train.h2o,
                   hyper_params = hyper_grid.h20,
                   search_criteria = search_criteria)
  
  this_grid_perf <- h2o.getGrid(
    grid_id = this_grid_id, 
    sort_by = "aucpr", 
    decreasing = TRUE
  )
  # this_grid_perf
  # Grab the model_id for the top model, chosen by validation error
  best_model_id <- this_grid_perf@model_ids[[1]]
  best_model <- h2o.getModel(best_model_id)
  # h2o.confusionMatrix(best_model)
  # h2o.auc(best_model)
  # SAVE MODEL
  model_files_dir <- glue("{data_dir}/stars/h2o-v7-rf/01-rfmodels")
  model_filename <- glue("{model_files_dir}/{thisimgID}")
  if(dir.exists(model_filename)){fs::dir_delete(model_filename)}
  h2o.saveModel(best_model, model_filename, force = TRUE)
  message(glue("model saved for {thisimgID}"))

  # Now let’s evaluate the model performance on a test set
  my_test.h2o <- as.h2o(my_test)
  # best_model_perf <- h2o.performance(model = best_model, newdata = my_test.h2o)
  # save test predictions
  pred_h20 <- predict(best_model, my_test.h2o) %>% as.data.frame()
  my_test$predict <- pred_h20$predict
  my_test$upland <- pred_h20$upland
  my_test$water <- pred_h20$water
 
  test_files_dir <- glue("{data_dir}/stars/h2o-v7-rf/02-testdata")
  if(!dir.exists(test_files_dir)){fs::dir_create(test_files_dir)}
  test_filename <- glue("{test_files_dir}/{thisimgID}.rds")
  my_test %>% saveRDS(test_filename)
  message(glue("test predictions saved for {thisimgID}"))
  
  # classify image
  # add cell ids to raster
  cellxys <- raster::coordinates(myimg_mvdeps_r)
  myimg_mvdeps_r$cellids <- raster::cellFromXY(myimg_mvdeps_r, cellxys)
  # extract including cellid
  img_vx_mvdeps <- velox::velox(myimg_mvdeps_r)
  img_extract_mvdeps <- img_vx_mvdeps$extract(mvdeps_sp, df = TRUE)
  names(img_extract_mvdeps)[2:14] <- c(band_names, "cellid")
  img_extract_mvdeps_narm <- img_extract_mvdeps %>% 
    dplyr::filter(!is.na(NDWI))
  
  mvdeps.h2o <- as.h2o(img_extract_mvdeps_narm)
  pred_h2o_myimg <- predict(best_model, mvdeps.h2o) %>% as.data.frame()
  img_extract_mvdeps_narm$predict <- pred_h2o_myimg$predict
  img_extract_mvdeps_narm$upland <- pred_h2o_myimg$upland
  img_extract_mvdeps_narm$water <- pred_h2o_myimg$water
  # then add layers to raster with predict, upland, water
  class_ints <- data.frame(predict = c("water", "upland"), class = 1:2)
  pred_myimg_df <- img_extract_mvdeps_narm %>% 
    left_join(class_ints, by = "predict")
  # save predictions
  pred_files_dir <- glue("{data_dir}/stars/h2o-v7-rf/03-pred_myimg_df")
  if(!dir.exists(pred_files_dir)){fs::dir_create(pred_files_dir)}
  pred_my_img_filename <- glue::glue("{pred_files_dir}/{thisimgID}.rds")
  pred_myimg_df %>% saveRDS(pred_my_img_filename)
  message(glue("model predictions saved for {thisimgID}"))
  
  # remove cellid duplicates
  pred_myimg_df_nodups <- pred_myimg_df %>% 
    dplyr::distinct_at(vars(cellid), .keep_all = TRUE)
  mvdeps_cellnos <- pred_myimg_df_nodups$cellid
  r_classified <- myimg_mvdeps_r$cellids
  r_classified$predict <- NA
  r_classified$upland <- NA
  r_classified$water <- NA
  r_classified$mvdeps_id <- NA
  r_classified$predict[mvdeps_cellnos] <- pred_myimg_df_nodups$class
  r_classified$upland[mvdeps_cellnos] <- pred_myimg_df_nodups$upland
  r_classified$water[mvdeps_cellnos] <- pred_myimg_df_nodups$water
  r_classified$mvdeps_id[mvdeps_cellnos] <- as.character(pred_myimg_df_nodups$ID_sp) %>% 
    as.numeric()
  # save classified raster with cell ids, 3 model layers
  classimg_files_dir <- glue("{data_dir}/stars/h2o-v7-rf/04-rf-classified-imgs")
  if(!dir.exists(classimg_files_dir)){fs::dir_create(classimg_files_dir)}
  classifiedPath <- glue("{classimg_files_dir}/{thisimgID}.tif")
  writeRaster(r_classified, classifiedPath, overwrite = TRUE)
  message(glue("classified raster saved for {thisimgID}"))
  
  # extract values for each mvdeps polygon
  myimg_classified <- raster::stack(classifiedPath)
  img_vx <- velox::velox(myimg_classified)
  img_extract <- img_vx$extract(mvdeps_sp, df = TRUE)
  # save with polygon id and image id 
  img_extract_narm <- img_extract %>% 
    dplyr::select(ID_sp, cellid = X1, class = X2, upland = X3, 
                  water = X4, int_ID_sp = X5) %>% 
    left_join(class_ints, by = "class") %>%
    dplyr::mutate(imgID = thisimgID)
  
  mvdepsclass_files_dir <- glue("{data_dir}/stars/h2o-v7-rf/05-mvdeps-classified")
  if(!dir.exists(mvdepsclass_files_dir)){fs::dir_create(mvdepsclass_files_dir)}
  mvdeps_class_filename <- glue("{mvdepsclass_files_dir}/{thisimgID}.rds")
  img_extract_narm %>% saveRDS(mvdeps_class_filename)
  message(glue("extracted predictions saved for {thisimgID}"))
  
  h2o.rm(ids = this_grid_id, cascade = TRUE)
  # h2o.removeAll()
  }
  if(!file.exists(extract2_filename)){
    message(glue("no extracted training data for {thisimgID}"))
  }
  }
  }
