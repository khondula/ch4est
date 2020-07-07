library(glue)
library(readr)
library(dplyr)
library(purrr)

data_dir <- "/nfs/khondula-data/planetmethane"


## Version 4 - 05 output now rds files
# mvdeps_class_file <- new_mvdeps_class_files[7]
mvdeps_summarize_v4 <- function(mvdeps_class_file, models_id_dir){
  thisimgID <- mvdeps_class_file %>% 
    basename() %>% tools::file_path_sans_ext()
  
  mvdeps_class <- mvdeps_class_file %>% readRDS()
    # read_csv(col_types = cols_only(ID_sp = "c",
    #                                cellid = "i",
    #                                class = "i",
    #                                upland = "d",
    #                                water = "d",
    #                                int_ID_sp = "i",
    #                                imgID = "c"))
  mvdeps_class_narm <- mvdeps_class %>% 
    dplyr::filter(!is.na(class)) 
  if(nrow(mvdeps_class_narm)==0){
    message(glue("No predicted water in {thisimgID}"))
  }
  if(nrow(mvdeps_class_narm)>0){
    class_ints <- data.frame(predict = c("water", "upland"), 
                             class = 1:2)
    
    mvdeps_df2 <- mvdeps_class_narm %>%
      mutate(predict = as.character(predict),
             ID_sp = as.character(ID_sp)) %>%
      # dplyr::left_join(class_ints, by = "class") %>%
      dplyr::group_by(ID_sp, predict) %>%
      summarise(n_cells = n()) %>% 
      tidyr::pivot_wider(names_from = "predict", 
                         values_from = "n_cells", 
                         values_fill = list(n_cells = 0))
    if(!"upland" %in% names(mvdeps_df2)){
      mvdeps_df2 <- mvdeps_df2 %>% mutate(upland = 0)
    }
    if(!"water" %in% names(mvdeps_df2)){
      mvdeps_df2 <- mvdeps_df2 %>% mutate(water = 0)
    }
    mvdeps_df2 <- mvdeps_df2 %>%
      dplyr::mutate(n_cells = upland + water,
                    water_m2 = water*9,
                    img_id = thisimgID,
                    img_date = as.Date(substr(img_id, 1,8), "%Y%m%d"))
    mvdeps_df_filename <- glue("{data_dir}/stars/{models_id_dir}/06-mvdeps-predicts/{thisimgID}.csv")
    mvdeps_df2 %>% readr::write_csv(mvdeps_df_filename)  
    message(glue("mvdeps predicts {thisimgID} summarized!"))
  }
  
}

update_mvdeps_predicts_v4 <- function(models_id_dir){
  mvdeps_class_dir <- glue("{data_dir}/stars/{models_id_dir}/05-mvdeps-classified")
  mvdeps_class_files <- list.files(mvdeps_class_dir, full.names = TRUE)
  mvdeps_df_dir <- glue("{data_dir}/stars/{models_id_dir}/06-mvdeps-predicts")
  if(!dir.exists(mvdeps_df_dir)){dir.create(mvdeps_df_dir)}
  mvdeps_dir_files <- list.files(mvdeps_df_dir)
  
  mvdeps_class_ids <- mvdeps_class_files %>% 
    basename() %>% tools::file_path_sans_ext()
  mvdeps_df_ids <- mvdeps_dir_files %>% 
    basename() %>% tools::file_path_sans_ext()
  new_mvdeps_class_files <- mvdeps_class_files[!mvdeps_class_ids %in% mvdeps_df_ids]
  purrr::walk(new_mvdeps_class_files, ~mvdeps_summarize_v4(.x, models_id_dir))
  message(glue("{length(new_mvdeps_class_files)} new files summarized!"))
}


