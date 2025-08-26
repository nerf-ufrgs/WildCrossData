library(readxl)
library(tidyverse)
library(writexl)
library(here)

source("R/FUNCTIONS.R")

read_sheet <- function(path = "Excel", sheet = NULL, na = "", results = TRUE) {
  excel <- list.files(
    path = path,
    pattern = "^\\w.+xlsx$",
    full.names = TRUE,
    recursive = TRUE
  )
  
  names <- excel |>
    stringr::str_split("/|\\.") |>
    purrr::map_vec(2)
  
  load <- excel |>
    purrr::set_names(names)
  
  if (!results) {
    return(load)
  }
  
  column_types <- set_column_types(sheet = sheet)
  
  result <- load |>
    purrr::map(function(file) {
      df <- readxl::read_excel(
        path = file,
        sheet = sheet,
        na = na,
        col_names = TRUE,
        col_types = NULL
      ) #|>
      #   janitor::remove_empty("rows")
      
      names(df) <- df |>
        janitor::clean_names() |>
        colnames() |>
        stringr::str_to_sentence()
      
      return(df)
    })
  
  return(result)
}

list_sheet_dataset <- list()

spreadsheets <- c(
  "Underpasses",
  "Overpasses",
  "Fencing",
  "Camera_trap",
  "Species_records_camera",
  "Author_data"
)

# we read every sheet for every dataset
for (spreadsheet in spreadsheets) {
  
  message(stringr::str_glue("\nStarting sheet {spreadsheet}\n"))
  list_sheet_dataset[[spreadsheet]] <- read_sheet(
    path = "Excel",
    sheet = spreadsheet
  )
  
  message(stringr::str_glue("\nFinalizing sheet {spreadsheet}\n"))
}

number_of_dataset <- list.files(path = "Excel") |>
  length()

list_sheet_dataset |>
  purrr::map(~ length(.x) == number_of_dataset)

result_df <- purrr::imap_dfr(list_sheet_dataset, function(df_list, nome_grupo) {
  purrr::imap_dfr(df_list, function(df, nome_planilha) {
    if (!is.data.frame(df)) return(NULL)
    
    df_with_id <- tibble::rowid_to_column(df, "id")
    
    # matriz lógica TRUE/FALSE indicando células NA
    na_matrix <- is.na(df_with_id)
    na_matrix[, "id"] <- FALSE  # ignorar coluna id
    
    # pegar linhas e colunas onde tem NA
    positions <- which(na_matrix, arr.ind = TRUE)
    
    if (nrow(positions) == 0) return(NULL)
    
    # criar data.frame com as posições reais
    tibble::tibble(
      grupo = nome_grupo,
      planilha = nome_planilha,
      linha = df_with_id$id[positions[, "row"]],
      coluna = colnames(df_with_id)[positions[, "col"]]
    )
  })
})

result_df |> 
  arrange() |> 
  openxlsx2::write_xlsx(
    sprintf("Output/result_df_blank_cells_%s.xlsx", today()), asTable = TRUE)

