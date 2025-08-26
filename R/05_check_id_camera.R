#VERIFICA OS IDS DAS CAMERAS - SE OS IDS INFORMADOS NOS REGISTROS ESTAO NA ABA DAS ARMADILHAS

#PACOTES NECESSARIOS
library(tidyverse)

source("R/FUNCTIONS.R")

#Lê a planilha de ct
ct <- read_sheet(sheet = "Camera_trap")

#Lê a planilha de registros
rec <- read_sheet(sheet = "Species_records_camera")

#VERIFICA SE OS IDS CAMERAS NOS REGISTROS SAO CORRESPONDENTES AO IDS DAS ARMADILHAS
record_to_ct <- map2(ct, rec, function(c, r) {
  setdiff(r$Camera_id, c$Camera_id) |>
    enframe(name = "Error", value = "Camera_id") |>
    mutate(note = "Record without camera on sheet")
}) |> #MOSTRA APENAS OS IDS ERRADOS QUE NAO ESTAO NA ABA DAS ARMADILHAS
  discard(~ nrow(.x) == 0) |> #descarta os arquivos sem erros
  bind_rows(.id = "Dataset")

ct_to_record <- map2(ct, rec, function(c, r) {
  setdiff(c$Camera_id, r$Camera_id) |>
    enframe(name = "Error", value = "Camera_id") |>
    mutate(note = "Camera without record on sheet")
}) |> #MOSTRA APENAS OS IDS ERRADOS QUE NAO ESTAO NA ABA DAS ARMADILHAS
  discard(~ nrow(.x) == 0) |> #descarta os arquivos sem erros
  bind_rows(.id = "Dataset")

record_to_ct |>
  bind_rows(ct_to_record) |>
  openxlsx::write.xlsx("Output/CHECA_ID_errors.xlsx", asTable = TRUE)
