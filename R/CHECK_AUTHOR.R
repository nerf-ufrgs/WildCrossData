library(janitor)
library(readxl)
library(taxize)
library(httr)
library(jsonlite)
library(tidyverse)

excel <- list.files(
  path = "Excel", #fuça a pasta "EXCEL_MOD"
  pattern = "^\\w.+xlsx$", #busca um padrão de arquivo que termine com "xlsx"
  full.names = TRUE, #retorna o caminho completo do arquivo
  recursive = TRUE
) #fuça nas subpastas do caminho escolhido

names <- excel %>% 
  str_split(., "/|\\.") %>%  #fatia o endereço completo dos arquivos baseado em / e . Aqui pode ter diferença para o Windows em função do uso de \ nos caminhos.
  map_chr(pluck(2)) #transforma em data frame tipo tibble

load <- excel %>% 
  set_names(., names) #atribui à lista de arquivos (excel) os nomes do vetor extraído no passo anterior

author <- map(.x = load, function(arquivo){
  read_excel(
    arquivo, #lê o arquivo excel que está no endereço em load
    sheet = 7, #lê a segunda planilha do arquivo excel (dados câmeras)
    #skip = 1, #pula a primeira linha do arquivo (por causa da mesclagem)
    na = c("NA", "na"),
    # col_types = c("guess", "guess", "guess", "date", "guess", "guess", "guess"),
    # col_names = TRUE
  )
})

map(author, pluck(10), .id = "author")
