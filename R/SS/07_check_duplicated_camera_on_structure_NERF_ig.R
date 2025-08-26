#PACOTES NECESSARIOS
source("R/FUNCTIONS.R")
library(lubridate)
use("lubridate", c("%within%", "%--%"))

#We load the camera trap setup data (ct) and look for duplicated camera IDs
# Read camera trap setup data
ct <- read_sheet(sheet = "Camera_trap", na = c("NA", "na"))

ct_nerf <- ct$NERF_test

# Cross-checking with Species Records
rec <- read_sheet(sheet = "Species_records_camera", na = c("NA", "na")) |> 
  purrr::map(\(x) x |> 
               dttm_update(
                 date_col = "Record_date",
                 time_col = "Record_time"
               ) |> 
               dplyr::select(-Record_time)
  ) 

rec_nerf <- rec$NERF_test

cam <- ct_nerf |> 
  dplyr::mutate(code = stringr::str_glue("S{Structure_id}-C{Camera_id}"),
                dplyr::across(dplyr::ends_with("_time"), ~ stringr::str_sub(., start = -8, end = -4)),
                datetime_start = lubridate::ymd_hm(paste(as.Date(Start_date), tidyr::replace_na(Start_time, "00:00"))),
                datetime_end = lubridate::ymd_hm(paste(as.Date(End_date), tidyr::replace_na(End_time, "00:00")))) |> 
  dplyr::rename(Camera_id_ct = Camera_id) |> 
  dplyr::select(code, Structure_id, Camera_id_ct, datetime_start, datetime_end)
  

reg <- rec_nerf |> 
  tibble::rowid_to_column("id")


all_matches <- reg |> 
    dplyr::full_join(cam, by = "Structure_id", relationship = "many-to-many")


bla <- all_matches |> 
  dplyr::mutate(belongs_to = dplyr::if_else(
    condition = Record_date %within% c(datetime_start %--% datetime_end),
    Camera_id_ct,
    "nope"))

bla2 <- bla |> 
  filter(belongs_to != "nope")

writexl::write_xlsx(bla2, path = "Output/Check_cameras_NERF_ig.xlsx")
