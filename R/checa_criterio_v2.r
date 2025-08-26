library(magrittr)
library(janitor)
library(readxl)
library(hms)
library(lubridate)
library(tidyverse)

#Verficar a coluna Criterio - quais sao os criterios informados e se os registros obdecem esse criterio

options(scipen = 999)

excel <- list.files(
  path = "Dados", #fuça a pasta "EXCEL_MOD"
  pattern = "^\\w.+xlsx$", #busca um padrão de arquivo que termine com "xlsx"
  full.names = TRUE, #retorna o caminho completo do arquivo
  recursive = TRUE #fuça nas subpastas do caminho escolhido
)

nomes <- excel %>% 
  str_split(., "/|\\.", #fatia o endereço completo dos arquivos baseado em / e . Aqui pode ter diferença para o Windows em função do uso de \ nos caminhos.
            simplify = TRUE) %>% # transforma o resultado em data frame
  as_tibble() %>% #transforma em data frame tipo tibble
  pull(V3) #transforma a coluna V2 em vetor

load <- excel %>% 
  set_names(., nomes) #atribui à lista de arquivos (excel) os nomes do vetor extraído no passo anterior

#Lê a planilha de registros
# dup_data <- map(.x = load, function(arquivo){
#   read_excel(arquivo, #lê o arquivo excel que está no endereço em load
#              sheet = 6, #lê a segunda planilha do arquivo excel (dados câmeras)
#              #skip = 1, #pula a primeira linha do arquivo (por causa da mesclagem)
#              na = c("NA", "na"), #transforma valores na em NA verdadeiro
#              col_types = c("guess", "guess", "guess", "date", "guess", "guess", "guess"),
#              col_names = TRUE) %>% #transforma o nome das colunas de acordo com a lista criada na linha 10
#     filter(!is.na(Record_date)) %>%
#     mutate_all(as.character)
# }) %>%
#   discard(~ nrow(.x) == 0) %>%
#   plyr::ldply(., .fun = data.frame, .id = "Arquivo") %>% #transforma o resultado de listas para data frame
#   as_tibble() %T>% #transforma em tabela do tipo tibble
#   openxlsx::write.xlsx(x = .,
#                        file = "DATA_JANITOR/PLAN_CHECA_CRITERIO_DATA_DUP.xlsx", #salva o arquivo
#                        asTable = TRUE)


na_criterio <- map(.x = load, function(arquivo){
  read_excel(arquivo, #lê o arquivo excel que está no endereço em load
             sheet = 6, #lê a segunda planilha do arquivo excel (dados câmeras)
             #skip = 1, #pula a primeira linha do arquivo (por causa da mesclagem)
             na = c("NA", "na"),
             col_types = c("guess", "guess", "guess", "date", "guess", "guess", "guess"),
             col_names = TRUE) %>% 
    
    filter(is.na(Record_date) & !is.na(Criterio_Registro) | 
             is.na(Record_time) & !is.na(Criterio_Registro)) %>% 
    mutate_all(as.character)
    
}) %>% 
  discard(~ nrow(.x) == 0) %>% 
  plyr::ldply(., .fun = data.frame, .id = "Arquivo") %>% #transforma o resultado de listas para data frame
  as_tibble() %T>% #transforma em tabela do tipo tibble
  openxlsx::write.xlsx(x = .,
                       file = "Output/PLAN_CHECA_CRITERIO_DATAHORA_NA.xlsx", #salva o arquivo
                       asTable = TRUE)

#Checa quais sao os criterios informados
nao_num <- map(.x = load, function(arquivo){
  read_excel(arquivo, #lê o arquivo excel que está no endereço em load
             sheet = 6, #lê a segunda planilha do arquivo excel (dados câmeras)
             #skip = 1, #pula a primeira linha do arquivo (por causa da mesclagem)
             na = c("NA", "na"),
             col_types = c("guess", "guess", "guess", "date", "guess", "guess", "guess"),
             col_names = TRUE) %>% 
    filter(!is.na(Criterio_Registro))
}) %>% 
  discard(~ nrow(.x) == 0) %>% 
  plyr::ldply(., .fun = data.frame, .id = "Arquivo") %>% #transforma o resultado de listas para data frame
  as_tibble() %>% 
  distinct(Arquivo, Criterio_Registro) %>% 
  arrange(Criterio_Registro)


#Checa se ha mais de um criterio por camera
multiplos_criterios <- map(.x = load, function(arquivo){
  read_excel(arquivo, #lê o arquivo excel que está no endereço em load
             sheet = 6, #lê a segunda planilha do arquivo excel (dados câmeras)
             #skip = 1, #pula a primeira linha do arquivo (por causa da mesclagem)
             na = c("NA", "na"),
             col_types = c("guess", "guess", "guess", "date", "guess", "guess", "guess"),
             col_names = TRUE) %>% 
    filter(!is.na(Criterio_Registro)) %>% 
    distinct(Camera_ID, Criterio_Registro) %>% 
    count(Camera_ID) %>% 
    filter(n > 1)
}) %>% 
  discard(~ nrow(.x) == 0) %>% 
  plyr::ldply(., .fun = data.frame, .id = "Arquivo") %>% #transforma o resultado de listas para data frame
  as_tibble()

#Conferir se a hora dos registros preenchida em “Hora_Registro” obedece ao intervalo informado na coluna “Criterio_Registro”.

#Group by por espécie e câmera, calculando os intervalos entre cada registro. Para isso, precisa juntar data + hora. 
#Após a tabela gerada contendo coluna com o intervalo em minutos de cada um dos registros, filtrar pelo Criterio_Registro para ver se há algum intervalo menor do que aquele previsto.

leitura <- map(.x = load, function(arquivo){
  read_excel(
    arquivo, #lê o arquivo excel que está no endereço em load
    sheet = 6, #lê a segunda planilha do arquivo excel (dados câmeras)
    #skip = 1, #pula a primeira linha do arquivo (por causa da mesclagem)
    na = c("NA", "na"),
    col_names = TRUE
  ) %>% 
    filter(
      !str_detect(Criterio_Registro, pattern = "[:alpha:]"),
      !is.na(Criterio_Registro),
      !is.na(Record_time),
      !is.na(Record_date)
    ) %>% 
    mutate(Criterio_Registro = as.numeric(Criterio_Registro)) %>% 
    filter(Criterio_Registro <= 60) %>% 
    select(Species, Camera_ID, Record_date, Record_time, Criterio_Registro)
}) %>% 
  discard(~ nrow(.x) == 0)

PREV_CRITERIO <- leitura %>% 
  map(., function(a){
    tempoprim <- a %>% 
      mutate(DH_Registro = ymd(Record_date) + hms(as_hms(Record_time))) %>% 
      select(-Record_date, -Record_time) %>% 
      group_by(Species, Camera_ID) %>% 
      arrange(DH_Registro) %>% 
      mutate(Diftempoprim = round(as.numeric(DH_Registro[1] %--% DH_Registro)/60, 0),
             Diftempoant = round(as.numeric(lag(DH_Registro) %--% DH_Registro)/60, 0),
             OK_Diftempoprim = case_when(Diftempoprim == 0 & is.na(Diftempoant) ~ "OK",
                                         Diftempoprim == 0 & Diftempoant == 0 ~ "POSS_DUP",
                                         Diftempoprim < Criterio_Registro ~ "RATIÔ",
                                         TRUE ~ "OK")) %>% 
      arrange(Species, Camera_ID, DH_Registro) %>% 
      rowid_to_column("ID")
    
    tempoant <- tempoprim %>% 
      filter(OK_Diftempoprim == "OK") %>% 
      mutate(Diftempoant = round(as.numeric(lag(DH_Registro) %--% DH_Registro)/60, 2),
             OK_Diftempoant = case_when(Diftempoant < Criterio_Registro ~ "RATIÔ",
                                        TRUE ~ "OK"))
    
    full_join(tempoprim, tempoant) %>% 
      arrange(ID)
  }) %>% 
  plyr::ldply(., .fun = data.frame, .id = "Arquivo") %>% #transforma o resultado de listas para data frame
  as_tibble() %>% 
  filter(OK_Diftempoprim != "OK" |
           OK_Diftempoant == "RATIÔ") %>% 
  openxlsx::write.xlsx(x = .,
                       file = "Output/PLAN_CHECA_CRITERIO_ERRADO.xlsx", #salva o arquivo
                       asTable = TRUE)
