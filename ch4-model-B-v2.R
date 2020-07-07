
# my_dep_id <- "10097"

library(dplyr)
library(vroom)
library(LakeMetabolizer)

run_ch4_modelB <- function(my_dep_id, mov_avg_ts_filename, model_runs_id){
# set up for all deps
  # future::plan(multiprocess)
  
  data_dir <- "/nfs/khondula-data/planetmethane"
  # SET UP
  wateryear_days <- seq.Date(from = as.Date("2017-10-01"), 
                             to = as.Date("2018-09-30"), by = 1)
  mvdeps_meta_file <- glue("{data_dir}/stars/ref/mvdeps_meta_df.rds")
  mol_g_conversion = 16.04246
  # read in data for ch4 distributions
  hr_t1 <- readRDS(glue("{data_dir}/stars/ref/hr_table1_mod.rds"))
  # get latest time series from models directory
  gsod_data <- readRDS(glue("{data_dir}/stars/ref/gsod-data_sub.rds"))

  # ppmv source CH4 1.85 ppmv
  calc_sat_concCH4 <- function(waterTemp, baro){
    ckHCH4 <- 0.000014 #mol m-3 Pa, range: 0.0000096 - 0.000092
    cdHdTCH4 <- 1900 #K, range: 1400-2400
    cKelvin <- 273.15 #Conversion factor from Kelvin to Celsius
    cT0 <- 298.15#Henry's law constant T0
    cPresConv <- 0.000001 # Constant to convert mixing ratio from umol/mol (ppmv) to mol/mol. 
    satConcCH4 <-  (ckHCH4 * exp(cdHdTCH4*(1/(waterTemp + cKelvin) - 1/cT0))) * 
      1.85 * baro * cPresConv  
    sat_conc_ch4_umol <- satConcCH4*1e6
    return(sat_conc_ch4_umol)
  }
# for just 1 depression
  # read in and filter to just 1 depression
  my_ts <- readRDS(mov_avg_ts_filename) %>%
    dplyr::filter(dep_id == my_dep_id) %>%
    dplyr::select(dep_id, img_date, water_ma) %>%
    dplyr::filter(img_date %in% wateryear_days)
  
  # get metadata about my dep from mvdeps area data frame
  mvdeps_meta_my_dep <- readRDS(mvdeps_meta_file) %>%
    dplyr::filter(dep_id == my_dep_id)

  # STATIC WATER AREAS FOR THIS DEP
  mvdeps_area_my_dep <- mvdeps_meta_my_dep[["area_m2"]]
  my_ts_max_area <- max(my_ts[["water_ma"]], na.rm = TRUE)
  my_ts_median_area <- median(my_ts[["water_ma"]], na.rm = TRUE)
  my_ts_mean_area <- mean(my_ts[["water_ma"]], na.rm = TRUE)
  my_ts_q1_area <- quantile(my_ts[["water_ma"]], 
                            0.25, na.rm = TRUE) %>% as.numeric()
  my_ts_q3_area <- quantile(my_ts[["water_ma"]],
                            0.75, na.rm = TRUE) %>% as.numeric()
  
  # add static areas to time series df
  my_ts_wAreas <- my_ts %>%
    dplyr::mutate(mvdeps_area = mvdeps_area_my_dep,
                  maxpred_area = my_ts_max_area, 
                  medpred_area = my_ts_median_area,
                  meanpred_area = my_ts_mean_area,
                  q1_area = my_ts_q1_area,
                  q3_area = my_ts_q3_area) %>%
    left_join(gsod_data, by = c("img_date" = "YEARMODA"))

  ts_days = nrow(my_ts_wAreas) # nrow in time series

  # SIZE CLASS AREA CONCENTRATION MODEL
  # use temp and pressure to calculate flux rate from concentration
  # returns long format data frame

  # get ch4 distribution for this deps size class
  # size class m2 in table is MAX area in size class
  my_sizeclass = mvdeps_meta_my_dep[["area_size_class"]]
  my_sizeclass_ch4 <- dplyr::filter(hr_t1, sizeclass_m2 == my_sizeclass)
  # # moment matching
  # mym <- my_sizeclass_ch4[["ch4_flux_est"]]
  # mysd <- my_sizeclass_ch4[["ch4_flux_sd"]]
  # mylocation <- log(mym^2 / sqrt(mysd^2 + mym^2))
  # myshape <- sqrt(log(1 + (mysd^2 / mym^2)))
  mylocation <- my_sizeclass_ch4[["ch4_umol_est_loc"]]
  myshape <- my_sizeclass_ch4[["ch4_umol_est_shape"]]
  my_k600 <- my_sizeclass_ch4[["k600"]]
  
  # RANDOM DRAWS FOR EACH DAY OF TIME SERIES
calc_ch4_modelB <- function(my_ts_wAreas){  
    
  my_ch4_ts <- rlnorm(n = ts_days, 
                      meanlog = mylocation,
                      sdlog = myshape)
  
  # add my_ch4_ts to inundation ts data frame
  # multiply flux rate by day area for dynamic ts ch4
  # multiply flux rate by my_area for static ts ch4
  ts_ch4 <- c()
  
  ts_ch4 <- my_ts_wAreas %>% 
    dplyr::mutate(ch4_umolL = my_ch4_ts,
                  k600 = my_k600,
                  kgas = k600.2.kGAS.base(k600 = my_k600, temperature = water_temp_C, gas = "CH4"),
                  satCH4_umol = calc_sat_concCH4(waterTemp = water_temp_C, baro = stp_kpa_na),
                  ch4_mmolm2day = kgas*(ch4_umolL - satCH4_umol),
           ch4_dyn = water_ma*ch4_mmolm2day,
           ch4_mvdeps = mvdeps_area*ch4_mmolm2day,
           ch4_75deps = (mvdeps_area*(3/4))*ch4_mmolm2day,
           ch4_maxpred = maxpred_area*ch4_mmolm2day,
           ch4_medpred = medpred_area*ch4_mmolm2day,
           ch4_meanpred = meanpred_area*ch4_mmolm2day,
           ch4_q1area = q1_area*ch4_mmolm2day,
           ch4_q3area = q3_area*ch4_mmolm2day) %>% 
    dplyr::select(-water_ma, -mvdeps_area,
                  -maxpred_area, -medpred_area,
                  -meanpred_area, -q1_area, -q3_area) %>%
    tidyr::pivot_longer(cols = c("ch4_dyn", "ch4_mvdeps", 
                                 "ch4_75deps", "ch4_maxpred",
                               "ch4_medpred", "ch4_meanpred", 
                               "ch4_q1area", "ch4_q3area"),
                        names_to = "area_model_type", 
                        values_to = "ch4_mmol") 
  return(ts_ch4)
}
# run 1000 times for each dpression time series
message("running model 1000x")
my_runs_list <- rep(list(my_ts_wAreas), 1000)
names(my_runs_list) <- glue("model_{1:1000}")
# my_runs <- purrr::map(my_runs_list, ~calc_ch4_modelA(.x))
my_runs <- furrr::future_map(my_runs_list, ~calc_ch4_modelB(.x))
message("did future map")
ts_ch4_bind <- my_runs %>% 
  bind_rows(.id = "model_id")

# SAVE CH4 TIME SERIES WITH ALL DEPS
ch4_ts_dir <- glue("{data_dir}/stars/{model_runs_id}/08-ch4-models/outtype_B")
if(!dir.exists(ch4_ts_dir)){fs::dir_create(ch4_ts_dir)}
ts_ch4_bind_filename <- glue("{ch4_ts_dir}/dep_{my_dep_id}.rds")
ts_ch4_bind %>% saveRDS(file = ts_ch4_bind_filename)

message(glue("emissions model results saved for {my_dep_id}"))
}

# Then, filter to just woody wetlands and sum up.
