#VERIFICA SE TODOS INFORMARAM OS ORCIDS E SE OS MESMOS EXISTEM (ESTAO CORRETOS)

install.packages("rorcid")
install.packages("httpuv")
library(rorcid)
library(httpuv)

rorcid::orcid_auth() #como não vai encontrar um token no seu computador, vai necessitar fazer login e autorizar via navegador. Um token vai ser gerado e mostrado no console. Copiar apenas o código alfanumérico excluindo o termo "Bearer "

usethis::edit_r_environ() #Após copiar o token, rodar esse comando e transcrever o que está abaixo dentro do .Renviron. 
#ATENÇÃO! O token deve ser inserido entre as aspas e a segunda linha deve ter o seu # retirado.
#Orcid Token
ORCID_TOKEN='7ac6ec7e-b16a-4523-85fe-f9e02f6c1f1b'

#Após esse procedimento dar um Session -> Restart R.

library(rorcid)
library(magrittr)
library(readxl)
library(tidyverse)

options(scipen = 999) #Evita apresentação de números em formato científico (exponencial)

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


#Checa consistência de ORCID - existência e leitura na base.
checa_orcid <- map(load, function(arquivo){
  nest <- read_excel(arquivo, #lê o arquivo excel que está no endereço em load
                     sheet = 7, #lê a quarta planilha do arquivo excel (dados câmeras)
                     #range = cell_cols(1:10),
                     #skip = 1, #pula a primeira linha do arquivo (por causa da mesclagem)
                     na = c("NA", "na"),
                     col_names = TRUE) %>% 
    janitor::clean_names() %>% 
    rename(nome = 1) %>% 
    mutate(ORCID = str_sub(orcid, start = -19)) %>% 
    #filter(is.na(ORCID))
    pull(ORCID)
    #select(nome, ORCID)
  
  as.orcid(nest)
}) 
  #plyr::ldply(., .fun = data.frame, .id = "Arquivo") %>% 
  #arrange(ORCID)

nomes_all <- excel %>% 
  str_split(., "/|\\.", #fatia o endereço completo dos arquivos baseado em / e . Aqui pode ter diferença para o Windows em função do uso de \ nos caminhos.
            simplify = TRUE) %>% # transforma o resultado em data frame
  as_tibble() %>% #transforma em data frame tipo tibble
  pull(V3) #transforma a coluna V2 em vetor

load_all <- excel %>% 
  set_names(., nomes_all) #atribui à lista de arquivos (excel) os nomes do vetor extraído no passo anterior

##########################################################################
#verificando a ausencia de informacao na aba dos autores - presenca de NA
checa_na_autores <- map(load_all, function(arquivo){
  nest <- read_excel(arquivo, #lê o arquivo excel que está no endereço em load
                     sheet = 7, #lê a quarta planilha do arquivo excel (dados câmeras)
                     #range = cell_cols(1:10),
                     #skip = 1, #pula a primeira linha do arquivo (por causa da mesclagem)
                     na = c("NA", "na"),
                     col_names = TRUE) %>% 
    janitor::clean_names() %>% 
    select(2:12) %>% 
    rowid_to_column(., "ID_EXCEL") %>% #cria um id para facilitar a checagem no arquivo excel
    #mutate(ID_EXCEL = ID_EXCEL + 1) %>% #altera a coluna do passo anterior para que o resultado seja igual ao da linha no arquivo excel
    filter_all(any_vars(is.na(.))) #filtra toda e qualquer linha, em qualque coluna, que possua célula em branco
}) %>% 
  keep(., ~ nrow(.x) >= 1) %>%  #mantem apenas os arquivos que possuem células vazias para serem checadas (número de linhas > 1) - opção sem fórmula: keep(function(w) nrow(w) >= 1)
  plyr::ldply(., .fun = data.frame, .id = "Arquivo") %>% 
  as_tibble()

