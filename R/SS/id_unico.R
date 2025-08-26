source("SCRIPTS/function_idUnique.R")

library(readxl)
library(tidyverse)

#Lê os arquivos - planilha armadilhas fotográficas
excel <- list.files(path = "Excel", #fuça a pasta "EXCEL_MOD"
                    pattern = "^\\w.+xlsx$", #busca um padrão de arquivo que termine com "xlsx"
                    full.names = TRUE, #retorna o caminho completo do arquivo
                    recursive = FALSE) #fuça nas subpastas do caminho escolhido

nomes <- excel %>% 
  str_split(., "/|\\.") %>%  #fatia o endereço completo dos arquivos baseado em / e . Aqui pode ter diferença para o Windows em função do uso de \ nos caminhos.
  map_chr(pluck(2)) #transforma em data frame tipo tibble

load <- excel %>% 
  set_names(., nomes) #atribui à lista de arquivos (excel) os nomes do vetor extraído no passo anterior

planilha_remold <- map(load, function(arquivo){
  x <- arquivo %>%  #lê o arquivo excel que está no endereço em load
    read_excel(
      sheet = 5, #lê a quinta planilha do arquivo excel (dados câmeras)
      na = c("NA", "na"),
      col_names = TRUE
    ) %>% 
    mutate(Camera_ID_orig = Camera_ID, #criamos nova coluna idêntica a ID_Local_Camera
           across(where(is.character), str_squish))  #retiramos espaços extras das colunas character
  
  x %>% 
    id_unico(., sep = "_") %>% #utilizamos a função id_unico, que atribui letras a strings repetidos, usando underline como separador
    mutate(Camera_ID = if_else(double > 1, make.unique(Camera_ID, sep = "_"), Camera_ID)) %>%  #caso_persistam os repetidos (após acabar o alfabeto), cria um número após underline
    select(-double) %>% 
    mutate(checa_ID = Camera_ID_orig == Camera_ID) %>% #checa se o ID entre as colunas é o mesmo
    relocate(Camera_ID_orig, .before = 1) %>% #altera colunas de lugar
    relocate(checa_ID, .after = Camera_ID) #altera colunas de lugar
})

arquivo_para_mod <- planilha_remold %>% 
  plyr::ldply(., .fun = data.frame, .id = "Arquivo") %>% #junta as listas em data frame único
  filter(checa_ID == FALSE) %>%  #filtra apenas pela coluna checa_ID com ID diferente
  distinct(Arquivo) %>% #seleciona apenas os arquivos que possuem ID diferente
  droplevels() %>% #remove os níveis que não existem mais
  pull() #transforma em vetor

filter_plan <- planilha_remold %>% 
  keep(names(.) %in% arquivo_para_mod) #seleciona da lista apenas os arquivos que possuem ID diferente

#Cria planilhas com ID único atualizado para substituição das planilhas originais
walk(names(filter_plan), function(x){
  y <- filter_plan[[x]] #pega a instância atual da iteração
  
  y %>% 
    select(-checa_ID) %>% #remove a coluna checa_ID
    openxlsx::write.xlsx(., #cria os Excel
                         file = sprintf("Output/%s_remold.xlsx", x),
                         asTable = TRUE,
                         sheetName = "Plan_AF") 
  
})

#Criando a nova planilha de ID único para registros

#Carregando dados de registro
sp <- map(.x = load, function(arquivo){
  read_excel(
    arquivo, #lê o arquivo excel que está no endereço em load
    sheet = 6, #lê a segunda planilha do arquivo excel (dados câmeras)
    #skip = 1, #pula a primeira linha do arquivo (por causa da mesclagem)
    na = c("NA", "na"), #transforma células com NA ou na em NA no data frame do R
    #col_types = c("guess", "guess", "date", "guess", "date", "guess", "guess"), #passa os tipos das colunas. Guess, por exemplo, indica para o R advinhar, enquanto date assume data. ATENÇÃO: eventualmente dá problema de data quando a coluna é de hora. Nesse caso, alterar date para guess e só depois aterar, via mutate, para hora.
    #col_names = nome_colunas_reg #renomeia as colunas conforme os nomes no objeto "nome_colunas_reg"
  ) %>% 
    mutate(
      across(starts_with("Data"), as.Date), #transforma as colunas iniciadas por "Data", como data
      across(starts_with("Data"), ymd), #transforma as colunas iniciadas por "Data", como ano-mês-dia
      #across(starts_with("Hora"), hms)
    )
}) %>% 
  keep(names(.) %in% arquivo_para_mod)

#Lê os arquivos - planilha armadilhas fotográficas
pega_datas <- map(load, function(arquivo){
  x <- read_excel(
    arquivo, #lê o arquivo excel que está no endereço em load
    sheet = 5, #lê a segunda planilha do arquivo excel (dados câmeras)
    na = c("NA", "na"),
    col_names = TRUE
  ) %>% 
    mutate(Camera_ID_orig = Camera_ID,
           across(where(is.character), str_squish)) %>% 
    id_unico(., sep = "_") %>% 
    mutate(Camera_ID = make.unique(Camera_ID, sep = "_")) %>% 
    mutate(across(ends_with("date"), as.Date),
           across(starts_with("Problem"), as.Date),
           across(ends_with("date"), ymd),
           across(starts_with("Problem"), ymd))
})

pega_datas_cont <- pega_datas %>% 
  plyr::ldply(., .fun = data.frame, .id = "Arquivo") %>% #transforma o resultado de listas para data frame
  as_tibble() %>% 
  mutate(P1 = Problem1_to - Problem1_from + 1,
         P2 = Problem2_to - Problem2_from + 1,
         P3 = Problem3_to - Problem3_from + 1,
         P4 = Problem4_to - Problem4_from + 1) %>% 
  # P5 = Problema5_Até - Problema5_De + 1,
  # P6 = Problema6_Até - Problema6_De + 1,
  # P7 = Problema7_Até - Problema7_De + 1,
  # P8 = Problema8_Até - Problema8_De + 1,
  # P9 = Problema9_Até - Problema9_De + 1) %>%
  select(-starts_with("Problem")) %>%
  mutate(P1 = as.numeric(P1),
         P2 = as.numeric(P2),
         P3 = as.numeric(P3),
         P4 = as.numeric(P4)) %>% 
  mutate_at(c(15:18), ~replace_na(.,0)) %>% 
  mutate(PSoma = P1 + P2 + P3 + P4,
         Duration = (End_date - Start_date) - PSoma,
         ID = sprintf("%s_%s", Arquivo, Camera_ID)
  ) %>%
  arrange(ID)

nest_af <- pega_datas_cont %>% 
  filter(Arquivo %in% arquivo_para_mod) %>% 
  group_by(Arquivo) %>% 
  nest()

NEST_AF <- set_names(x = nest_af$data, nest_af$Arquivo)

ID_unique <- map2(NEST_AF, sp, function(x, y){
  y %>% 
    left_join(., x,
              by = c("Camera_ID" = "Camera_ID_orig"),
              suffix = c("", "_uniq")) %>% 
    mutate(Amostragem = interval(Start_date, End_date),
           Data_no_ID = case_when(Record_date %within% Amostragem ~ "OK",
                                  TRUE ~ "RATIÔ")) %>%
    filter(Data_no_ID != "RATIÔ")
  
}) %>% 
  set_names(names(sp))


#Cria planilhas com ID único atualizado para substituição das planilhas originais
walk(names(ID_unique), function(x){
  y <- ID_unique[[x]] #pega a instância atual da iteração
  
  y %>% 
    openxlsx::write.xlsx(., #cria os Excel
                         file = sprintf("%s_remoldRecords.xlsx", x),
                         asTable = TRUE,
                         sheetName = "Plan_SP") 
  
})