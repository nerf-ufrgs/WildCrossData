#PACOTES NECESSARIOS PARA RODAR ESSE SCRIPT
library(janitor)
library(readxl)
library(httr)
library(jsonlite)
library(tidyverse)

source("R/FUNCTIONS.R")

spreadsheets <- read_sheet(path = "Excel", results = FALSE)

sp_full <- map(.x = spreadsheets, function(arquivo){
  read_excel(
    arquivo, 
    sheet = 6, 
    na = c("NA", "na"),
    col_types = c("guess", "guess", "guess", "date", "guess", "guess", "guess"),
    col_names = TRUE
  )
})

# Primeira etapa: criar lista completa de nomes científicos plenos para checagem na IUCN
species_all_check <- sp_full |> 
  map(function(x) {
    x |> 
      distinct(Species) |> 
      mutate(Species = str_squish(Species)) |> 
      filter(
        str_count(Species, " ") == 1,
        !str_detect(word(Species, 2, 2), "\\."),
        !str_detect(word(Species, 2, 2), "^sp$"),
        !str_detect(word(Species, 2, 2), "^sp(?=\\.)"),
        !str_detect(word(Species, 2, 2), "^spp(?=\\.)"),
        !str_detect(word(Species, 2, 2), "\\("),
        !str_detect(word(Species, 2, 2), "^ni$"),
        !str_detect(word(Species, 2, 2), "^NI$"),
        !str_detect(word(Species, 2, 2), "^NID$"),
        !str_detect(Species, "\\/")
      ) |> 
      arrange(Species) |> 
      pull()
  })

# Segunda etapa: verificar nomes de espécies que estão duplicadas para arrumar questões de espaço dentro de cada Excel.
species_all_check |> 
  map(function(x) {
    table <- table(x)
    
    table[table > 1]
  }) |> 
  keep(~ any(.x > 1))

# Outra opção: fazendo pelo Globalnames usando ITIS (data source = 3)
list_check_globalnames <- list()

for (dataset in names(species_all_check)) {
  
  species <- species_all_check[[dataset]]
  
  message(str_glue("Starting dataset {dataset}"))
  
  for (sp in species) {
    sp_ <- sp |> 
      str_remove_all("[[:punct:]]") |> 
      str_replace_all(pattern = " ", replacement = "_")
    
    result <- GET(str_glue("https://verifier.globalnames.org/api/v1/verifications/{sp_}?data_sources=3"))
    
    list_check_globalnames[[dataset]][[sp_]] <- fromJSON(rawToChar(result$content))[["names"]] 
  }
  list_check_globalnames[[dataset]][["all_results"]] <- list_check_globalnames[[dataset]] |> 
    bind_rows() |> 
    unnest(cols = c(bestResult), names_repair = "unique") |> 
    unnest(cols = c(scoreDetails), names_repair = "unique") |> 
    as_tibble()
}

list_sp <- list_check_globalnames |> 
  map("all_results") |> 
  bind_rows(.id = "dataset") |> 
  clean_names() |> 
  mutate(query = str_replace(name, "_", " "), .after = name) 

sp_with_errors <- list_sp |> 
  filter(match_type_4 != "Exact")

sp_whitelist <- list_sp |> 
  filter(match_type_4 == "Exact") |> 
  pull(query) |> 
  append(c("Guerlinguetus brasiliensis", "Guerlinguetus ingrami", "Marmosa demerarae",
           "Pauxi tuberosa", "Mazama jucunda", "Marmosa paraguayana", "Hadrosciurus spadiceus",
           "Philander quica", "Mazama rufa", "Alouatta puruensis", "Dicotyles tajacu",
           "Sylvilagus paraguensis")) |> # Inserir manualmente espécies que sabemos que estão certas na entrada, mas que ele corrige erroneamente em "matched_name"
  unique() |> 
  sort()

# Aqui finaliza a checagem de "espécies" ========

# Buscando agora pelas não espécies (todo o resto)
non_species_all_check <- map2(sp_full, species_all_check, function(x, y){
  x |> 
    distinct(Species) |> 
    mutate(Species = str_squish(Species)) |> 
    pull(Species) |> 
    setdiff(y)
}) |> 
  compact()

# List
list_non_sp_with_errors <- list()

for (dataset in names(non_species_all_check)) {
  
  species <- non_species_all_check[[dataset]]
  
  message(str_glue("Starting dataset {dataset}"))
  
  for (sp in species) {
    sp_ <- sp |> 
      str_remove_all("[[:punct:]]") |> 
      str_replace_all(pattern = " ", replacement = "_")
    
    result <- GET(str_glue("https://verifier.globalnames.org/api/v1/verifications/{sp_}?data_sources=3")) # the link for the API check
    
    list_non_sp_with_errors[[dataset]][[sp_]] <- fromJSON(rawToChar(result$content))[["names"]] # save the part that interests us on a list composed by the dataset and the species name
  }
  # bind the species list on a single data frame unnesting the columns that are a data frame
  list_non_sp_with_errors[[dataset]][["all_results"]] <- list_non_sp_with_errors[[dataset]] |> 
    bind_rows() |> 
    as_tibble()
}

non_sp_with_errors <- list_non_sp_with_errors |> 
  map("all_results") |> 
  bind_rows(.id = "dataset") |> 
  unnest(cols = c(bestResult), names_repair = "unique") |> 
  unnest(cols = c(scoreDetails), names_repair = "unique") |> 
  clean_names() |> 
  mutate(
    query = str_replace_all(name, "_", " "),
    result_with_problem = case_when(
      str_count(query, " ") >= 2 ~ "YES",
      word(query, 2, 2) == "spp" ~ "YES",
      word(query, 2, 2) == "NI" ~ "YES",
      word(query, 2, 2) == "ni" ~ "YES",
      match_type_5 != "Exact" ~ "YES",
      TRUE ~ "NO"
    )
  ) |> 
  filter(result_with_problem == "YES") |> 
  select(dataset, query, matched_canonical_simple, match_type = match_type_5)

sp_with_errors |> 
  select(dataset, query, matched_canonical_simple, match_type = match_type_4) |> 
  filter(!query %in% sp_whitelist) |> 
  bind_rows(non_sp_with_errors) |> 
  arrange(dataset) |> 
  openxlsx2::write_xlsx(
    sprintf("Output/PLAN_ERROS_CHECA_ESPECIES_%s.xlsx", today()), asTable = TRUE
  )

message("Script finalizado")
