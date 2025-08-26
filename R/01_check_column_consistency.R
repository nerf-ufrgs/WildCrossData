# PACOTES NECESSARIOS
library(readxl)
library(tidyverse)
library(writexl)
library(here)

source("R/FUNCTIONS.R")

#Lê as planilhas para checagem de consistência e empilhamento dos dados ----
#Underpasses ----
under_q <- quietly(function(dataset) {
  sheet <- "Underpasses"

  column_types <- set_column_types(sheet = sheet)

  read_excel(
    dataset,
    col_types = column_types,
    sheet = sheet,
    na = c("NA", "na"), # define as possibilidades de valores vazios que se tornarão NA no novo data frame
    col_names = TRUE
  ) |>
    janitor::clean_names() |>
    janitor::remove_empty("rows") #remove as linhas vazias (em que os elementos de todas as colunas são NA)
})

under_all_outputs <- read_sheet(results = FALSE) |>
  map(under_q)

under_warns <- under_all_outputs |>
  map(\(x) pluck(x, "warnings")) |>
  compact()

under_results <- under_all_outputs |>
  map(\(x) pluck(x, "result")) |>
  bind_rows(.id = "Dataset")

#Overpasses ----
over_q <- quietly(function(dataset) {
  sheet <- "Overpasses"

  column_types <- set_column_types(sheet = sheet)

  read_excel(
    dataset,
    col_types = column_types,
    sheet = sheet,
    na = c("NA", "na"), # define as possibilidades de valores vazios que se tornarão NA no novo data frame
    col_names = TRUE
  ) |>
    janitor::clean_names() |>
    janitor::remove_empty("rows") #remove as linhas vazias (em que os elementos de todas as colunas são NA)
})

over_all_outputs <- read_sheet(results = FALSE) |>
  map(over_q)

over_warns <- over_all_outputs |>
  map(\(x) pluck(x, "warnings")) |>
  compact()

over_results <- over_all_outputs |>
  map(\(x) pluck(x, "result")) |>
  bind_rows(.id = "Dataset")

#Fencing ----
fence_q <- quietly(function(dataset) {
  sheet <- "Fencing"

  column_types <- set_column_types(sheet = sheet)

  read_excel(
    dataset,
    col_types = column_types,
    sheet = sheet,
    na = c("NA", "na"), # define as possibilidades de valores vazios que se tornarão NA no novo data frame
    col_names = TRUE
  ) |>
    janitor::clean_names() |>
    janitor::remove_empty("rows") #remove as linhas vazias (em que os elementos de todas as colunas são NA)
})

fence_all_outputs <- read_sheet(results = FALSE) |>
  map(fence_q)

fence_warns <- fence_all_outputs |>
  map(\(x) pluck(x, "warnings")) |>
  compact()

fence_results <- fence_all_outputs |>
  map(\(x) pluck(x, "result")) |>
  bind_rows(.id = "Dataset")

#Camera trap ----
camera_q <- quietly(function(dataset) {
  sheet <- "Camera_trap"

  column_types <- set_column_types(sheet = sheet)

  read_excel(
    dataset,
    col_types = column_types,
    sheet = sheet,
    na = c("NA", "na"), # define as possibilidades de valores vazios que se tornarão NA no novo data frame
    col_names = TRUE
  ) |>
    janitor::clean_names() |>
    janitor::remove_empty("rows") |> #remove as linhas vazias (em que os elementos de todas as colunas são NA)
    mutate(across(ends_with("_time"), hms::as_hms))
})

camera_all_outputs <- read_sheet(results = FALSE) |>
  map(camera_q)

camera_warns <- camera_all_outputs |>
  map(\(x) pluck(x, "warnings")) |>
  compact()

camera_results <- camera_all_outputs |>
  map(\(x) pluck(x, "result")) |>
  bind_rows(.id = "Dataset")

#Species ----
species_q <- quietly(function(dataset) {
  sheet <- "Species_records_camera"

  column_types <- set_column_types(sheet = sheet)

  read_excel(
    dataset,
    col_types = column_types,
    sheet = sheet,
    na = c("NA", "na"), # define as possibilidades de valores vazios que se tornarão NA no novo data frame
    col_names = TRUE
  ) |>
    janitor::clean_names() |>
    janitor::remove_empty("rows") |> #remove as linhas vazias (em que os elementos de todas as colunas são NA)
    mutate(across(ends_with("_time"), hms::as_hms))
})

species_all_outputs <- read_sheet(results = FALSE) |>
  map(species_q)

species_warns <- species_all_outputs |>
  map(\(x) pluck(x, "warnings")) |>
  compact()

species_results <- species_all_outputs |>
  map(\(x) pluck(x, "result")) |>
  bind_rows(.id = "Dataset")

#Junta todas listas

all_warns <- list (camera_warns, fence_warns, over_warns, species_warns, under_warns)


# Agrupa todos erros
error_types <- c("camera_warns", "fence_warns", "over_warns", "species_warns", "under_warns")

# Lista vazia pra receber os dfs
warns_df <- list()

# Loop por cada grupo de warn
for (i in seq_along(all_warns)) {
  group <- all_warns[[i]]
  error_type <- error_types[i]  # nome descritivo do tipo de warn
  
  # Para cada nome dentro do grupo
  for (name in names(group)) {
    errors <- group[[name]]
    
    # Cria um data.frame com as colunas desejadas
    temp_df <- data.frame(
      Name = rep(name, length(errors)),
      Error = errors,
      Group = rep(i, length(errors)),
      type_error = rep(error_type, length(errors)),
      stringsAsFactors = FALSE
    )
    
    warns_df[[length(warns_df) + 1]] <- temp_df
  }
}

# Junta tudo em um único dataframe
final_df <- do.call(rbind, warns_df)

#arrumando a ordem das colunas
final_df <- final_df[, c("Name", "type_error", "Error", "Group")]

#exportando
write_xlsx(final_df, here("Output", "warns_column_consistency.xlsx"))
