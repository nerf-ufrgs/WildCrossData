########## ITEM 1 DE REVISAO ############

#VERIFICAR A EXISTENCIA DE CELULAS VAZIAS NAS PLANILHAS PARA PODER PREENCHER COM "NA"

#PACOTES NECESSARIOS PARA RODAR ESSE SCRIPT
library(readxl)
library(tidyverse)
library(writexl)
library(here)

source("R/FUNCTIONS.R")

list_sheet_dataset <- list()

sheets <- c(
  "Underpasses",
  "Overpasses",
  "Fencing",
  "Camera_trap",
  "Species_records_camera",
  "Author_data"
)

#Verifica nos arquivos excel em todas as abas se ha celulas em branco e em quais linhas estao
for (sheet in sheets) {
  
  message(str_glue("\nStarting sheet {sheet}\n"))
  
  list_sheet_dataset[[sheet]] <- read_sheet(
    sheet = sheet
  )
  
  message(str_glue("\nFinalizing sheet {sheet}\n"))
}

# Confere se todos os datasets possuem o mesmo número - número total - de datasets.
# Número total de datasets
number_of_dataset <- list.files("Excel") |> 
  length()

list_sheet_dataset |> 
  map(~ length(.x) == number_of_dataset)

# Checando se existe uma planilha completamente vazia
list_sheet_dataset |> 
  list_flatten() |> 
  map(~ rowid_to_column(.x, "id") |> #cria um id para facilitar a checagem no arquivo excel
        add_row(id = 0) |> 
        mutate(row = id + 1, #altera a coluna do passo anterior para que o resultado seja igual ao da linha no arquivo excel
               in_or_out = if_else(sum(id) == 0, "Uh oh!", "OK")) |>  
        filter(in_or_out == "Uh oh!") |> 
        select(row) #seleciona apenas o índice das linhas para checagem no excel
  ) |> 
  keep(~ nrow(.x) >= 1) |>  #mantem apenas os arquivos que possuem células vazias para serem checadas (número de linhas > 1) - opção sem fórmula: keep(function(w) nrow(w) >= 1)
  names()


list_sheet_dataset |> 
  list_flatten() |> 
  map(~ rowid_to_column(., "id") |>
        add_row(id = 0) |>
        mutate(row = id + 1, 
               in_or_out = if_else(sum(id) == 0, "Uh oh!", "OK")) |> 
        filter(in_or_out != "Uh oh!") |>
        filter_all(any_vars(is.na(.))) |>
        filter(id != 0) |> 
        select(row)
  ) |>
  keep(~ nrow(.x) >= 1)

df <- list_sheet_dataset |>
  purrr::list_flatten() |>
  purrr::map(~ tibble::rowid_to_column(., "id") |>
               dplyr::add_row(id = 0) |>
               dplyr::mutate(row = id + 1,
                             in_or_out = dplyr::if_else(sum(id) == 0, "Uh oh!", "OK")) |>
               dplyr::filter(in_or_out != "Uh oh!") |>
               dplyr::filter_all(dplyr::any_vars(is.na(.))) |>
               dplyr::filter(id != 0) |>
               dplyr::select(row)
  ) |>
  purrr::keep(~ nrow(.x) >= 1)

# prepara a lista para exportar


# Exporta para o output
write_xlsx(list("Erros" = lista_longa), here("Output", "02_check_blank_output.xlsx"))
