#PACOTES NECESSARIOS
library(hms)
library(tidyverse)

source("R/FUNCTIONS.R")

# Lê a planilha de ct
ct <- read_sheet(sheet = "Camera_trap", na = c("NA", "na"))

# Checa consistência de valores de data e hora com retirada antes da instalação
check_date_before <- map(.x = ct, function(dataset) {
  dataset |>
    dttm_update(date_col = "Start_date", time_col = "Start_time") |>
    dttm_update(date_col = "End_date", time_col = "End_time") |>
    select(-ends_with("_time")) |>
    mutate(
      duration = as.duration(Start_date %--% End_date)
    ) |>
    filter(duration < 86400)
}) |>
  bind_rows(.id = "dataset") |>
  select(dataset, Camera_id, Start_date:duration)

check_date_before

# Checa consistência de valores de data e hora para durações muito extensas
check_date_after <- map(.x = ct, function(dataset) {
  dataset |>
    dttm_update(date_col = "Start_date", time_col = "Start_time") |>
    dttm_update(date_col = "End_date", time_col = "End_time") |>
    select(-ends_with("_time")) |>
    mutate(
      duration = as.duration(Start_date %--% End_date)
    ) |>
    filter(duration > 7776000)
}) |>
  bind_rows(.id = "dataset") |>
  select(dataset, Camera_id, Start_date:duration)

check_date_after

# Checa consistência de valores de datas para o futuro
check_date_future <- map(.x = ct, function(dataset) {
  data_thresh <- "2025-04-30"
  dataset |>
    mutate(
      date_start = ymd(as.character(Start_date)),
      date_end = ymd(as.character(End_date))
    ) |>
    filter(if_any(starts_with("date_"), ~ .x > data_thresh))
}) |>
  bind_rows(.id = "dataset") |>
  select(dataset, Camera_id, date_start:date_end)

check_date_future

# Checa se o tempo amostrado condiz com a quantidade de registros
rec <- read_sheet(sheet = "Species_records_camera", na = c("NA", "na"))

datasets <- names(rec)
# Faz junção da planilha de armadilhas fotográficas com a planilha de espécies
species_records_within_ct_date <- list()
error_log <- tibble(dataset = character(), error_message = character())

for (dataset in datasets) {
  message(str_glue("Starting dataset {dataset}\n"))
  tryCatch(
    {
      camera <- ct[[dataset]] |>
        select(Structure_id, Camera_id, Start_date, End_date)
      
      species_records_within_ct_date[[dataset]] <- rec[[dataset]] |>
        inner_join(camera, by = c("Camera_id", "Structure_id")) |>
        mutate(
          across(ends_with("date"), as_datetime),
          row = row_number(),
          check = case_when(
            Record_date %within% c(Start_date %--% End_date) ~
              "YES",
            TRUE ~ "NO"
          )
        ) |>
        filter(check == "NO") |>
        select(row, Species, Camera_id, ends_with("date"), check)
      message(str_glue("Finalizing dataset {dataset}\n"))
    },
    error = function(e) {
      msg <- as.character(e$message)
      error_log <<- bind_rows(
        error_log,
        tibble(dataset = dataset, error_message = msg)
      )
      message(str_glue("Error in dataset {dataset}: {msg}\n"))
      return(NULL)
    }
  )
}

error_log

species_records_within_ct_date |> 
  discard(~ nrow(.x) == 0) |> #descarta os arquivos sem erros
  bind_rows(.id = "dataset") |>  #junta todos os dados em um único data frame
  count(dataset, sort = TRUE) |> 
  print(n = Inf)

species_records_within_ct_date |>
  openxlsx::write.xlsx(
    "Output/REGISTROS_SP_FORA_DA_DATA.xlsx", #salva o arquivo
    asTable = TRUE, #como tabela
    colWidths = "auto" #deixa a largura das colunas de acordo com o tamanho de texto de cada uma delas
  )
