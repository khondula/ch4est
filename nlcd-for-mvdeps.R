# mvdeps included should be...
# within NLCD forest or woody wetlands

# get NLCD class of each mvdeps
mvdeps_orig_file <- glue("{data_dir}/polygons/mvdepressions.shp")
mvdeps <- st_read(mvdeps_orig_file) %>% st_transform(planet_crs)
mat1 <- mvdeps %>% 
  sf::st_intersects(greensboro, sparse = FALSE)
mvdeps_in_gb <- which(apply(mat1, 1, any))
mvdeps_gb <- mvdeps[mvdeps_in_gb,]
mvdeps_gb <- mvdeps_gb %>% mutate(dep_id_chr = glue("dep_{dep_id}"))
mvdeps_gb_prj <- st_transform(mvdeps_gb, proj4string(nlcd_mask_greens))
# convert sf objects to sp for polygon IDs
mvdeps_gb_sp <- sf::as_Spatial(st_geometry(mvdeps_gb_prj),
                  IDs = as.character(mvdeps_gb_prj[["dep_id_chr"]])) 
# extract values for training polygons
# subset stars objects using sf objects!

nlcd_mask_greens # from greensboro-nlcd.R
# nlcd_greensboro
nlcd_stars <- st_as_stars(nlcd_mask_greens)
# nlcd_stars_mvdeps <- nlcd_mask_greens_stars[mvdeps_gb_prj]

# then convert to rasters and label bands
nlcd_r <- as(nlcd_stars, "Raster")
# do value extract with velox
nlcd_vx <- velox::velox(nlcd_r)
nlcd_extract <- nlcd_vx$extract(mvdeps_gb_sp, df = TRUE, small = TRUE)

# individual cells, sum to get unique classes
nlcd_extract_df <- nlcd_extract %>%
  dplyr::rename(nlcd_class = 2) %>%
  dplyr::group_by(ID_sp, nlcd_class) %>%
  dplyr::summarise(sum_nlcd = n())

head(nlcd_extract_df, 10)
# is there more than one class for that dep
# test - dep 00004 is 8 cells 82, 1 of 21
nlcd_extract_mixed <- nlcd_extract_df %>%
  dplyr::arrange(-sum_nlcd) %>% 
  summarise(num_classes = n(),
            top_class = first(nlcd_class)) %>% 
  dplyr::mutate(multiclass = num_classes > 1) %>% 
  left_join(nlcd_codes, by = c("top_class" = "Class")) %>%
  dplyr::select(-description) %>%
  dplyr::mutate(dep_id = substr(ID_sp, 5, 9))

nlcd_extract_summary <- nlcd_extract_mixed$class_name %>% 
  table() %>% 
  as.data.frame() %>%
  dplyr::rename(nlcd_class = 1, num_mvdeps = Freq) %>%
  arrange(-num_mvdeps)

# nlcd_extract_summary$num_mvdeps %>% sum()
# 11166 + 23
# nlcd_extract_mixed %>%
  # write_csv("/nfs/khondula-data/planetmethane/stars/ref/nlcd_x_mvdeps.csv")
# woody wetland, deciduous forest, 
# mixed forest, evergreen forest

read_csv("/nfs/khondula-data/planetmethane/stars/h2o-v2-rf/ref/mvdeps_meta_df.csv") %>% 
  dplyr::left_join(nlcd_extract_mixed, by = "dep_id") %>% 
  dplyr::select(-ID_sp, -num_classes) %>% 
  write_csv("/nfs/khondula-data/planetmethane/stars/ref/mvdeps_meta.csv")

mvdeps_meta <- read_csv("/nfs/khondula-data/planetmethane/stars/ref/mvdeps_meta.csv")
my_nlcd_classes <- c(90, 41, 42, 43)
mvdeps_mynlcd <- dplyr::filter(mvdeps_meta, top_class %in% my_nlcd_classes)
# cultivated crops is 2nd highest
# also lots of developed open space
