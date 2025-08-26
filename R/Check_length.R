library(tidyverse)

#Create an object with full path to .xlsx tables
nomes_completos <- list.files(path = "Excel", 
                     pattern = "\\.xlsx$",
                     full.names = TRUE)

# Divide the full path to get individual names of each table
nomes <- stringr::str_split(nomes_completos, pattern = "(?<=/)|(?=\\.xlsx$)") |> 
  sapply(function(z) z[2])

#Join every table into a single list
tabelas_com_abas <- purrr::map(nomes_completos, function(path) {
  abas <- readxl::excel_sheets(path)
  purrr::map(abas, ~readxl::read_excel(path, sheet = .x)) |>
    setNames(abas)
}) |>
  setNames(nomes)

tabelas_corrigidas <- purrr::map_depth(tabelas_com_abas, .depth = 2, function(df) {
  names(df) <- stringr::str_replace_all(names(df), "lenght", "length")
  return(df)
})

#abaixo foi feito por IA

for (caminho_do_arquivo in nomes_completos) {
  
  message("Processando: ", basename(caminho_do_arquivo))
  
  wb <- openxlsx::loadWorkbook(caminho_do_arquivo)
  abas <- names(wb)
  
  for (aba_atual in abas) {
    
    nomes_atuais <- names(openxlsx::read.xlsx(wb, sheet = aba_atual))
    nomes_corrigidos <- stringr::str_replace_all(nomes_atuais, "lenght", "length")
    
    if (!identical(nomes_atuais, nomes_corrigidos)) {
      openxlsx::writeData(wb, 
                          sheet = aba_atual, 
                          x = t(nomes_corrigidos),
                          startCol = 1, 
                          startRow = 1, 
                          colNames = FALSE)
    }
  }
  
  openxlsx::saveWorkbook(wb, caminho_do_arquivo, overwrite = TRUE)
}
