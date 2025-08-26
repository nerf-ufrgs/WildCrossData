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
    na = c("NA", "na"), #transforma células com NA ou na em NA no data frame do R
    #col_types = c("guess", "guess", "date", "guess", "date", "guess", "guess"), #passa os tipos das colunas. Guess, por exemplo, indica para o R advinhar, enquanto date assume data. ATENÇÃO: eventualmente dá problema de data quando a coluna é de hora. Nesse caso, alterar date para guess e só depois aterar, via mutate, para hora.
  ) %>% 
    rename(Camera_ID_orig = Camera_ID)
    
}) %>% 
  keep(names(.) %in% arquivo_para_mod)


NEST_AF <- set_names(x = nest_af$data, nest_af$Arquivo)

filter_plan

ID_unique <- map2(filter_plan, sp, function(x, y){
    left_join(x,y,
              by = c("Structure_ID", "Camera_ID_orig"))
              #suffix = c("", "_uniq")) %>% 
    # mutate(Amostragem = interval(Start_date, End_date),
    #        Data_no_ID = case_when(Record_date %within% Amostragem ~ "OK",
    #                               TRUE ~ "RATIÔ")) %>%
    # filter(Data_no_ID != "RATIÔ")
  
}) %>% 
  set_names(names(sp))


x %>% 
  id_unico(., sep = "_") %>% #utilizamos a função id_unico, que atribui letras a strings repetidos, usando underline como separador
  mutate(Camera_ID = if_else(double > 1, make.unique(Camera_ID, sep = "_"), Camera_ID)) %>%  #caso_persistam os repetidos (após acabar o alfabeto), cria um número após underline
  select(-double) %>% 
  mutate(checa_ID = Camera_ID_orig == Camera_ID) %>% #checa se o ID entre as colunas é o mesmo
  relocate(Camera_ID_orig, .before = 1) %>% #altera colunas de lugar
  relocate(checa_ID, .after = Camera_ID) #altera colunas de lugar


#Cria planilhas com ID único atualizado para substituição das planilhas originais
walk(names(ID_unique), function(x){
  y <- ID_unique[[x]] #pega a instância atual da iteração
  
  y %>% 
    openxlsx::write.xlsx(., #cria os Excel
                         file = sprintf("%s_remoldRecords.xlsx", x),
                         asTable = TRUE,
                         sheetName = "Plan_SP") 
  
})