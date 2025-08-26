#PACOTES NECESSARIOS
source("R/FUNCTIONS.R")

use("lubridate", c("%within%", "%--%"))

#We load the camera trap setup data (ct) and look for duplicated camera IDs
# Read camera trap setup data
ct <- read_sheet(sheet = "Camera_trap", na = c("NA", "na"))

ct |>
  head(2)

#Identifying Duplicated Cameras - quais dataset, quais estruturas e quais cameras estao duplicadas, informando quantas vezes
duplicated_cameras <- ct |> 
  purrr::map(~ .x |> 
               dplyr::count(Structure_id, Camera_id) |> 
               dplyr::filter(n > 1)) |> 
  purrr::discard(~ nrow(.x) == 0) |> 
  dplyr::bind_rows(.id = "Dataset")

duplicated_cameras |>
  head()
#  Extract the datasets with duplicated cameras and generate a column with new unique camera IDs
# pega o nome dos datasets
dataset_dup_cameras <- duplicated_cameras |> 
  dplyr::distinct(Dataset) |> 
  dplyr::pull(Dataset)

#separa cada dataset em uma lista com suas informacoes das cameras duplicadas
ct_with_dupes <- ct[names(ct) %in% dataset_dup_cameras]

# ja cria uma coluna nova com o novo id das cameras duplicadas, agora todas sao unicas - foi acrescentado uma letra ao final do nome
ct_uniq <- ct_with_dupes |> 
  purrr::map(~ unique_id(.x))

writexl::write_xlsx(ct_uniq, path = "Output/Check_duplicated_cameras_renameCAMERAS.xlsx")

# para visualizar
ct_uniq |>
  head(2)

# Check if any dataset still has more than one camera ID - precisa retornar zero, ou seja, nao tem mais duplicadas, caso contrario rever
ct_uniq |> 
  dplyr::bind_rows(.id = "Dataset") |> 
  dplyr::count(Dataset, Structure_id, Camera_id) |> 
  dplyr::filter(n > 1) |>
  head(2)

# Cross-checking with Species Records
rec <- read_sheet(sheet = "Species_records_camera", na = c("NA", "na")) |> 
  purrr::map(\(x) x |> 
               dttm_update(
                 date_col = "Record_date",
                 time_col = "Record_time"
               ) |> 
               dplyr::select(-Record_time)
  ) 

# pega os registros das cameras duplicadas que foram identificadas antes
rec_with_dupes <- rec[names(rec) %in% dataset_dup_cameras]

rec_with_dupes |> 
  head(2)

# Output Generation
rows_with_errors <- list()

for (dataset in dataset_dup_cameras) {
  cam <- ct_uniq[[dataset]] |> 
    dplyr::mutate(code = stringr::str_glue("S{Structure_id}-C{Camera_id_orig}"))
  
  reg <- rec_with_dupes[[dataset]] |> 
    tibble::rowid_to_column("id") |> 
    dplyr::mutate(code = stringr::str_glue("S{Structure_id}-C{Camera_id}"))
  
  intermediate_result <- reg |> 
    dplyr::full_join(cam, by = "code", suffix = c("_rec", ""), relationship = "many-to-many") |> 
    dplyr::mutate(
      dplyr::across(dplyr::ends_with("_time"), ~ stringr::str_sub(., start = -8, end = -4)),
      datetime_record = Record_date,
      datetime_start = lubridate::ymd_hm(paste(
        as.character(Start_date),
        tidyr::replace_na(Start_time, "00:00")
      )),
      datetime_end = lubridate::ymd_hm(paste(
        as.character(End_date),
        tidyr::replace_na(End_time, "00:00")
      )),
      belongs_to = dplyr::if_else(
        condition = datetime_record %within% c(datetime_start %--% datetime_end),
        Camera_id,
        "nope"
      )
    )
  
  rows_with_errors[[dataset]] <- intermediate_result |> 
    dplyr::distinct(id, belongs_to, .keep_all = TRUE) |> 
    dplyr::filter(!(dplyr::n() > 1 & belongs_to == "nope"), .by = "id") |> 
    dplyr::filter(!is.na(id))
  
   #intermediate_result |> 
    #dplyr::filter(all(belongs_to == "nope"), .by = "id")
}

#rows_with_errors |> 
 # dplyr::bind_rows(.id = "dataset")

writexl::write_xlsx(rows_with_errors, path = "Output/Check_duplicated_cameras_renameRECORDS.xlsx")
