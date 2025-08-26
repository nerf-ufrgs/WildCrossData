library(sf)
library(janitor)
library(magrittr)
library(parzer)
library(readxl)
library(lubridate)
library(hms)
library(tidyverse)

# PROBLEMA: As datas e os horários dos registros das espécies devem estar dentro do período de amostragem (data e hora) da câmera correspondente, e fora dos períodos de problema, se estas colunas estiverem preenchida

#Trabalhando com registros fora da data para planilhas com ID_Local_Camera repetidos
## Verifica datas dos registros dentro do período de amostragem exclusivamente das planilhas cujo ID_Local_Camera se repete

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

af <- map(.x = load, function(arquivo){
  read_excel(arquivo, #lê o arquivo excel que está no endereço em load
             sheet = 5, #lê a segunda planilha do arquivo excel (dados câmeras)
             na = c("NA", "na"), #transforma células com NA ou na em NA no data frame do R
             col_names = TRUE) %>%
    
     select(Camera_ID = 2,
            Start_date = 8,
            End_date = 10,
            starts_with("Problem")) %>% 
 
    mutate(across(ends_with("date"), ymd), #transforma as colunas terminadas por "date" para ano-mês-dia
           across(starts_with("Problem"), ymd)) %>% #transforma as colunas iniciadas por "Problema" para ano-mês-dia
    select(where(~!all(is.na(.x)))) #Mantém as colunas que possuem algum dado, ou seja, elimina colunas em que todas as linhas são NA.
}) %>% 
  discard(~ ncol(.x) <= 3) #descarta os data frames que possuem 3 ou menos colunas - porque nao foi preenchido as colunas problema, entao esta ok


sp <- map(.x = load, function(arquivo){
  read_excel(
    arquivo, #lê o arquivo excel que está no endereço em load
    sheet = 6, #lê a segunda planilha do arquivo excel (dados câmeras)
    na = c("NA", "na"), #transforma células com NA ou na em NA no data frame do R
    col_types = c("guess", "guess", "guess", "date", "guess", "guess", "guess"), #passa os tipos das colunas. Guess, por exemplo, indica para o R advinhar, enquanto date assume data. ATENÇÃO: eventualmente dá problema de data quando a coluna é de hora. Nesse caso, alterar date para guess e só depois aterar, via mutate, para hora.
    #col_names = nome_colunas_reg #renomeia as colunas conforme os nomes no objeto "nome_colunas_reg"
  ) %>% 
    mutate(
      across(ends_with("date"), as.Date), #transforma as colunas iniciadas por "Data", como data
      across(ends_with("date"), ymd), #transforma as colunas iniciadas por "Data", como ano-mês-dia
      #across(starts_with("Hora"), hms)
    )
}) %>% 
  keep(names(.) %in% names(af)) #mantem apenas os data frames cujos nomes estão contidos nos nomes dos data frames presentes em af

#O objeto 'bla' seleciona as cameras dentro de todas as planilhas que contem problema
bla <- map2(set_names(names(af), names(af)), names(sp), function(x, y){
  colunas_af <- af$Planilha_dados_1_portugues_GARCIA_Francini[1,] #Pega os nomes das colunas totais baseado no df da Cecília Cronemberger (que possui todas as colunas - até Problema9)
  
  colunas_af[1,] <- NA #Transformei o dado da primeira linha da X como NA para não ter resquício dos dados dela em outros data frames.
  
  a <- af[[x]] #Pega os nomes individuais das listas para iteração
  b <- sp[[y]] #Pega os nomes individuais das listas para iteração
  
  #b representa as planilhas sp.
  b_nest <- b %>%
    arrange(Camera_ID) %>% #Organiza pelo Camera_ID
    group_by(Camera_ID) %>% #Agrupa pelo Camera_ID
    nest() #Aninha
  
  b_NEST <- set_names(b_nest$data, b_nest$Camera_ID) #retira a coluna aninhada de dentro do list data frame e atribui os nomes do Camera_ID a cada um deles.
  
  a_nest <- a %>%
    bind_rows(colunas_af) %>% #Junta com as colunas do af da X, que não tem nada além de nomes das colunas e NA
    filter(!is.na(Camera_ID)) %>% #Exclui as linhas com Camera_ID com NA
    arrange(Camera_ID) %>% #Organiza pelo Camera_ID
    group_by(Camera_ID) %>% #Agrupa pelo Camera_ID
    nest() #Aninha
  
  a_NEST <- set_names(a_nest$data, a_nest$Camera_ID) #Retira a coluna aninhada de dentro do list data frame e atribui os nomes do Camera_ID a cada um deles.
 
  nomes_lista <- intersect(names(a_NEST), names(b_NEST)) #Uma vez que algumas das armadilhas não geraram registros de espécies, é preciso retirar as listas referentes a essas câmeras. A função intersect mantém apenas as câmeras que possuem registro
  
  multipluck <- function(x, ...) {
    `[`(x, ...)
  } #Função criada para extrair múltiplos nomes de listas, similar à função purrr::pluck que só permite extração de uma lista.
  
  a_NEST %<>% #atualiza o objeto a_NEST
    #multipluck(nomes) #mantém apenas as listas de AF que estão dentro de SP, ou seja, apenas as câmeras que possuem registros
    keep(names(.) %in% nomes_lista) #outra forma de fazer a mesma coisa acima
  #list(af = a_NEST, sp = b_NEST)
  
  pre <- map2(names(a_NEST), names(b_NEST), function(x, y){
    af <- a_NEST[[x]] #Pega os nomes individuais das listas para iteração
    sp <- b_NEST[[y]] #Pega os nomes individuais das listas para iteração
    
    af %<>% #Pega o data frame das armadilhas
      filter_at(vars(starts_with("Problem")), any_vars(!is.na(.))) %>% #filtra as variáveis iniciadas por "Problem", mantendo as linhas que estão preenchidas (diferentes de NA)
      remove_empty("cols") #remove colunas que possuem apenas NA
  }) %>% 
    set_names(., nm = names(a_NEST)) %>% #dá o nome aos af de acordo com a_NEST
    discard( ~ nrow(.x) == 0) %>% #descarta os data frame sem linhas
    plyr::ldply(., .fun = data.frame, .id = "Camera_ID")  #junta todos os dados em um único data frame
}) %>% 
  plyr::ldply(., .fun = data.frame, .id = "Arquivo") %>%  #junta todos os dados em um único data frame
  as_tibble()

af_p1 <- bla %>% 
  mutate(across(starts_with("Problem"), ~ replace_na(., replace = today())), #Substitui os NA, nas colunas iniciadas por "Problem", pela data de hoje
         Problema1_Datas = map2(Problem1_from, Problem1_to, seq, "day")) %>%  #Cria uma coluna contendo uma sequência com todas as datas que existem entre Problema1_De e Problema1_Até
  unnest(Problema1_Datas) %>% #Desaninha todas as datas em Problema1 geradas no passo anterior
  filter(Problema1_Datas != today()) %>% #Filtra todas as datas diferentes do dia de hoje
  mutate(Codigo = str_c(Arquivo, Camera_ID, Problema1_Datas, sep = "_")) %>% #Cria um código concatenando Arquivo, Camera_ID e Problema1_Datas, separando-as por underline (_)
  relocate(Codigo, .after = Camera_ID) #Altera de posição a coluna código, passando-a para antes de Camera_ID
  # group_by(Codigo) %>% 
  # nest()

sp_p1 <- sp %>% 
  plyr::ldply(., .fun = data.frame, .id = "Arquivo") %>%  #junta todos os dados em um único data frame
  as_tibble() %>% 
  mutate(Data = case_when(!is.na(Record_date) ~ Record_date,
                          TRUE ~ NA_real_), #Cria uma coluna data assumindo uma data única, que deve ser a Data_Foto
         Codigo = str_c(Arquivo, Camera_ID, Data, sep = "_"), #Cria um código concatenando Arquivo, Camera_ID e Data, separando-as por underline (_)
         Problema = "Problema 1") %>% #Cria uma coluna Problema, com o nome/número do problema ("Problema 1")
  filter(Codigo %in% af_p1$Codigo) %>% #Seleciona apenas os Códigos que estão presentes em af_p1
  distinct(Arquivo, Camera_ID, Problema, Codigo, Data) #Seleciona instâncias únicas de Arquivo, Camera_ID, Problema, Codigo, Data, ou seja, se houver linhas repetidas nessas colunas, ele vai apresentar uma vez só
# group_by(Codigo) %>% 
  # nest()

af_p2 <- bla %>% 
  mutate(across(starts_with("Problem"), ~ replace_na(., replace = today())),
         Problema2_Datas = map2(Problem2_from, Problem2_to, seq, "day")) %>%  #Cria uma coluna contendo uma sequência com todas as datas que existem entre Problema2_De e Problema2_Até
  unnest(Problema2_Datas) %>% 
  filter(Problema2_Datas != today()) %>% 
  mutate(Codigo = str_c(Arquivo, Camera_ID, Problema2_Datas, sep = "_")) %>% 
  relocate(Codigo, .after = Camera_ID) 
# group_by(Codigo) %>% 
# nest()

sp_p2 <- sp %>% 
  plyr::ldply(., .fun = data.frame, .id = "Arquivo") %>%  #junta todos os dados em um único data frame
  as_tibble() %>% 
  mutate(Data = case_when(!is.na(Record_date) ~ Record_date,
                          TRUE ~ NA_real_),
         Codigo = str_c(Arquivo, Camera_ID, Data, sep = "_"),
         Problema = "Problema 2") %>% 
  filter(Codigo %in% af_p2$Codigo) %>% 
  distinct(Arquivo, Camera_ID, Problema, Codigo, Data)
# group_by(Codigo) %>% 
# nest()

af_p3 <- bla %>% 
  mutate(across(starts_with("Problem"), ~ replace_na(., replace = today())),
         Problema3_Datas = map2(Problem3_from, Problem3_to, seq, "day")) %>%  #Cria uma coluna contendo uma sequência com todas as datas que existem entre Problema3_De e Problema3_Até
  unnest(Problema3_Datas) %>% 
  filter(Problema3_Datas != today()) %>% 
  mutate(Codigo = str_c(Arquivo, Camera_ID, Problema3_Datas, sep = "_")) %>% 
  relocate(Codigo, .after = Camera_ID) 
# group_by(Codigo) %>% 
# nest()

sp_p3 <- sp %>% 
  plyr::ldply(., .fun = data.frame, .id = "Arquivo") %>%  #junta todos os dados em um único data frame
  as_tibble() %>% 
  mutate(Data = case_when(!is.na(Record_date) ~ Record_date,
                          TRUE ~ NA_real_),
         Codigo = str_c(Arquivo, Camera_ID, Data, sep = "_"),
         Problema = "Problema 3") %>% 
  filter(Codigo %in% af_p3$Codigo) %>% 
  distinct(Arquivo, Camera_ID, Problema, Codigo, Data)

af_p4 <- bla %>% 
  mutate(across(starts_with("Problem"), ~ replace_na(., replace = today())),
         Problema4_Datas = map2(Problem4_from, Problem4_to, seq, "day")) %>%  #Cria uma coluna contendo uma sequência com todas as datas que existem entre Problema4_De e Problema4_Até
  unnest(Problema4_Datas) %>% 
  filter(Problema4_Datas != today()) %>% 
  mutate(Codigo = str_c(Arquivo, Camera_ID, Problema4_Datas, sep = "_")) %>% 
  relocate(Codigo, .after = Camera_ID) 
# group_by(Codigo) %>% 
# nest()

sp_p4 <- sp %>% 
  plyr::ldply(., .fun = data.frame, .id = "Arquivo") %>%  #junta todos os dados em um único data frame
  as_tibble() %>% 
  mutate(Data = case_when(!is.na(Record_date) ~ Record_date,
                          TRUE ~ NA_real_),
         Codigo = str_c(Arquivo, Camera_ID, Data, sep = "_"),
         Problema = "Problema 4") %>% 
  filter(Codigo %in% af_p4$Codigo) %>% 
  distinct(Arquivo, Camera_ID, Problema, Codigo, Data)

af_p5 <- bla %>% 
  mutate(across(starts_with("Problem"), ~ replace_na(., replace = today())),
         Problema5_Datas = map2(Problem5_from, Problem5_to, seq, "day")) %>%  #Cria uma coluna contendo uma sequência com todas as datas que existem entre Problema5_De e Problema5_Até
  unnest(Problema5_Datas) %>% 
  filter(Problema5_Datas != today()) %>% 
  mutate(Codigo = str_c(Arquivo, Camera_ID, Problema5_Datas, sep = "_")) %>% 
  relocate(Codigo, .after = Camera_ID) 
# group_by(Codigo) %>% 
# nest()

sp_p5 <- sp %>% 
  plyr::ldply(., .fun = data.frame, .id = "Arquivo") %>%  #junta todos os dados em um único data frame
  as_tibble() %>% 
  mutate(Data = case_when(!is.na(Record_date) ~ Record_date,
                          TRUE ~ NA_real_),
         Codigo = str_c(Arquivo, Camera_ID, Data, sep = "_"),
         Problema = "Problema 5") %>% 
  filter(Codigo %in% af_p5$Codigo) %>% 
  distinct(Arquivo, Camera_ID, Problema, Codigo, Data)

##############################################################################################
af_p6 <- bla %>% 
  mutate(across(starts_with("Problem"), ~ replace_na(., replace = today())),
         Problema6_Datas = map2(Problema6_De, Problema6_Até, seq, "day")) %>%  #Cria uma coluna contendo uma sequência com todas as datas que existem entre Problema6_De e Problema6_Até
  unnest(Problema6_Datas) %>% 
  filter(Problema6_Datas != today()) %>% 
  mutate(Codigo = str_c(Arquivo, ID_Local_Camera, Problema6_Datas, sep = "_")) %>% 
  relocate(Codigo, .after = ID_Local_Camera) 
# group_by(Codigo) %>% 
# nest()

sp_p6 <- sp %>% 
  plyr::ldply(., .fun = data.frame, .id = "Arquivo") %>%  #junta todos os dados em um único data frame
  as_tibble() %>% 
  mutate(Data = case_when(!is.na(Data_Foto) ~ Data_Foto,
                          TRUE ~ Data_Registro),
         Codigo = str_c(Arquivo, ID_Local_Camera, Data, sep = "_"),
         Problema = "Problema 6") %>% 
  filter(Codigo %in% af_p6$Codigo) %>% 
  distinct(Arquivo, ID_Local_Camera, Problema, Codigo, Data)

af_p7 <- bla %>% 
  mutate(across(starts_with("Problem"), ~ replace_na(., replace = today())),
         Problema7_Datas = map2(Problema7_De, Problema7_Até, seq, "day")) %>%  #Cria uma coluna contendo uma sequência com todas as datas que existem entre Problema7_De e Problema7_Até
  unnest(Problema7_Datas) %>% 
  filter(Problema7_Datas != today()) %>% 
  mutate(Codigo = str_c(Arquivo, ID_Local_Camera, Problema7_Datas, sep = "_")) %>% 
  relocate(Codigo, .after = ID_Local_Camera) 
# group_by(Codigo) %>% 
# nest()

sp_p7 <- sp %>% 
  plyr::ldply(., .fun = data.frame, .id = "Arquivo") %>%  #junta todos os dados em um único data frame
  as_tibble() %>% 
  mutate(Data = case_when(!is.na(Data_Foto) ~ Data_Foto,
                          TRUE ~ Data_Registro),
         Codigo = str_c(Arquivo, ID_Local_Camera, Data, sep = "_"),
         Problema = "Problema 7") %>%  
  filter(Codigo %in% af_p7$Codigo) %>% 
  distinct(Arquivo, ID_Local_Camera, Problema, Codigo, Data)

af_p8 <- bla %>% 
  mutate(across(starts_with("Problem"), ~ replace_na(., replace = today())),
         Problema8_Datas = map2(Problema8_De, Problema8_Até, seq, "day")) %>%  #Cria uma coluna contendo uma sequência com todas as datas que existem entre Problema8_De e Problema8_Até
  unnest(Problema8_Datas) %>% 
  filter(Problema8_Datas != today()) %>% 
  mutate(Codigo = str_c(Arquivo, ID_Local_Camera, Problema8_Datas, sep = "_")) %>% 
  relocate(Codigo, .after = ID_Local_Camera) 
# group_by(Codigo) %>% 
# nest()

sp_p8 <- sp %>% 
  plyr::ldply(., .fun = data.frame, .id = "Arquivo") %>%  #junta todos os dados em um único data frame
  as_tibble() %>% 
  mutate(Data = case_when(!is.na(Data_Foto) ~ Data_Foto,
                          TRUE ~ Data_Registro),
         Codigo = str_c(Arquivo, ID_Local_Camera, Data, sep = "_"),
         Problema = "Problema 8") %>% 
  filter(Codigo %in% af_p8$Codigo) %>% 
  distinct(Arquivo, ID_Local_Camera, Problema, Codigo, Data)

af_p9 <- bla %>% 
  mutate(across(starts_with("Problem"), ~ replace_na(., replace = today())),
         Problema9_Datas = map2(Problema9_De, Problema9_Até, seq, "day")) %>%  #Cria uma coluna contendo uma sequência com todas as datas que existem entre Problema9_De e Problema9_Até
  unnest(Problema9_Datas) %>% 
  filter(Problema9_Datas != today()) %>% 
  mutate(Codigo = str_c(Arquivo, ID_Local_Camera, Problema9_Datas, sep = "_")) %>% 
  relocate(Codigo, .after = ID_Local_Camera) 
# group_by(Codigo) %>% 
# nest()

sp_p9 <- sp %>% 
  plyr::ldply(., .fun = data.frame, .id = "Arquivo") %>%  #junta todos os dados em um único data frame
  as_tibble() %>% 
  mutate(Data = case_when(!is.na(Data_Foto) ~ Data_Foto,
                          TRUE ~ Data_Registro),
         Codigo = str_c(Arquivo, ID_Local_Camera, Data, sep = "_"),
         Problema = "Problema 9") %>% 
  filter(Codigo %in% af_p9$Codigo) %>% 
  distinct(Arquivo, ID_Local_Camera, Problema, Codigo, Data)

mget(ls(pattern = "^sp_")) %>% #Pega do ambiente de objetos (Global Environment), pela função mget, todos os objetos que se iniciem (^) com "sp_"
  plyr::ldply(., .fun = data.frame, .id = "Arquivo") %>% #junta todos os dados em um único data frame
  as_tibble() %>% 
  arrange(Arquivo) %T>% #Organiza pelo nome do arquivo
  openxlsx::write.xlsx("DATA_JANITOR/REGISTROS_NOS_PROBLEMAS.xlsx",
                       asTable = TRUE)


mget(ls(pattern = "sp_")) %>% 
  plyr::ldply(., .fun = data.frame, .id = "Arquivo") %>% 
  as_tibble() %>% 
  arrange(Arquivo) %>% 
  count(Arquivo, sort = TRUE)

