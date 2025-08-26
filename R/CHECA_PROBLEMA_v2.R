library(sf)
library(janitor)
library(magrittr)
library(parzer)
library(readxl)
library(lubridate)
library(hms)
library(tidyverse)

#Verifica se as datas problemas estao dentro do periodo de amostragem, mas nao dentro do periodo de funcionamento

excel <- list.files(path = "Dados", #fuça a pasta "EXCEL_MOD"
                    pattern = "^\\w.+xlsx$", #busca um padrão de arquivo que termine com "xlsx"
                    full.names = TRUE, #retorna o caminho completo do arquivo
                    recursive = TRUE) #fuça nas subpastas do caminho escolhido

nomes <- excel %>% 
  str_split(., "/|\\.", #fatia o endereço completo dos arquivos baseado em / e . Aqui pode ter diferença para o Windows em função do uso de \ nos caminhos.
            simplify = TRUE) %>% # transforma o resultado em data frame
  as_tibble() %>% #transforma em data frame tipo tibble
  pull(V3) #transforma a coluna V2 em vetor

load <- excel %>% 
  set_names(., nomes) #atribui à lista de arquivos (excel) os nomes do vetor extraído no passo anterior

pega_datas <- map(load, function(arquivo){
  read_excel(
    arquivo, #lê o arquivo excel que está no endereço em load
    sheet = 5, #lê a planilha do arquivo excel (dados câmeras)
    #range = cell_cols(1:10),
    #skip = 1, #pula a primeira linha do arquivo (por causa da mesclagem)
    na = c("NA", "na"), #transforma células com NA ou na em NA no data frame do R
    col_names = TRUE #assume as colunas com os nomes como eles vêm do Excel
  ) %>% 
    select(
      Camera_ID = 2, #seleciona a coluna 1 renomeando com ID_Local_Camera
      Start_date = 8, #seleciona a coluna 11 renomeando com Data_inicio
      End_date = 10, #seleciona a coluna 13 renomeando com Data_retirada
      starts_with("Problem") #seleciona as colunas iniciadas por "Problema"
    ) %>% #
    
    mutate(across(ends_with("date"), ymd), #transforma as colunas iniciadas por "Data" para ano-mês-dia
           across(starts_with("Problem"), ymd)) #transforma as colunas iniciadas por "Problema" para ano-mês-dia
}) %>%  
  plyr::ldply(., .fun = data.frame, .id = "Arquivo") %>% #transforma o resultado de listas para data frame
  as_tibble() %>% 
  mutate(
    TInstala1 = if_else(Problem1_from - Start_date < 0, "RATIÔ", "OK"), 
    TInstala2 = if_else(Problem2_from - Start_date < 0, "RATIÔ", "OK"),
    # TInstala3 = if_else(Problem3_from - Start_date < 0, "RATIÔ", "OK"),
    # TInstala4 = if_else(Problem4_from - Start_date < 0, "RATIÔ", "OK"),
    # TInstala5 = if_else(Problem5_from - Start_date < 0, "RATIÔ", "OK"),
    # TInstala6 = if_else(Problem6_from - Start_date < 0, "RATIÔ", "OK"),
    # TInstala7 = if_else(Problem7_from - Start_date < 0, "RATIÔ", "OK"),
    # TInstala8 = if_else(Problem8_from - Start_date < 0, "RATIÔ", "OK"),
    # TInstala9 = if_else(Problem9_from - Start_date < 0, "RATIÔ", "OK"),
    TRetira1 = if_else(Problem1_to - End_date > 0, "RATIÔ", "OK"),
    TRetira2 = if_else(Problem2_to - End_date > 0, "RATIÔ", "OK")
    # TRetira3 = if_else(Problem3_to - End_date > 0, "RATIÔ", "OK"),
    # TRetira4 = if_else(Problem4_to - End_date > 0, "RATIÔ", "OK"),
    # TRetira5 = if_else(Problem5_to - End_date > 0, "RATIÔ", "OK"),
    # TRetira6 = if_else(Problem6_to - End_date > 0, "RATIÔ", "OK"),
    # TRetira7 = if_else(Problem7_to - End_date > 0, "RATIÔ", "OK"),
    # TRetira8 = if_else(Problem8_to - End_date > 0, "RATIÔ", "OK"),
    # TRetira9 = if_else(Problem9_to - End_date > 0, "RATIÔ", "OK")
  ) %>% #para cada uma das linhas, se o início ou final do problema for anterior ou posterior à data de retirada, consiste em erro "RATIÔ", do contrário, fica como "OK"?
  #mutate(across(starts_with("T"), replace_na, 0)) %>% #altera as colunas iniciadas com T substituindo NA por 0
  rowid_to_column() #cria uma coluna de ID

Instala <- pega_datas %>% 
  filter_at(vars(starts_with("TInstala")), any_vars(. == "RATIÔ")) #filtra, nas variáveis começadas por TInstala, todas as linhas que tiverem "RATIÔ"
# filter(across(starts_with("TInstala"), ~ .x == "RATIÔ") %>%
#          reduce(`|`))

Retira <- pega_datas %>% 
  filter_at(vars(starts_with("TRetira")), any_vars(. == "RATIÔ")) #filtra, nas variáveis começadas por TRetira, todas as linhas que tiverem "RATIÔ"

bind_rows(Instala, Retira) %T>% #juntas as linhas com os erros em Instala e Retira
  openxlsx::write.xlsx(
    "Output/ERRO_PROBLEMA_INSTALA_RETIRA.xlsx", #salva o arquivo
    asTable = TRUE, #como tabela
    colWidths = "auto" #deixa a largura das colunas de acordo com o tamanho de texto de cada uma delas
  ) 
