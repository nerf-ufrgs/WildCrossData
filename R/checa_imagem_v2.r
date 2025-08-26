#VERIFICANDO AS IMAGENS FORNECIDAS PELA GALERA - CAMERATRAP
##VERIFICANDO SE O NOME DOS ARQUIVOS CORRESPONDE AOS NOMES DOS ARQUIVOS NAS PLANILHAS

#PACOTE NECESSARIOS
library(magrittr)
library(janitor)
library(readxl)
library(hms)
library(lubridate)
library(tidyverse)

#LENDO A PASTA ONDE ESTAO AS PLANILHAS (APENAS AS PLANILHAS)
excel <- list.files(path = "Excel", #fuça a pasta "EXCEL_MOD"
                    pattern = "^\\w.+xlsx$", #busca um padrão de arquivo que termine com "xlsx"
                    full.names = TRUE, #retorna o caminho completo do arquivo
                    recursive = TRUE #fuça nas subpastas do caminho escolhido
)
#SEPARANDO APENAS O NOME DE CADA PLANILHA E RETIRANDO A EXTENSÃO DA LOCALIAÇÃO DA PLANILHA, APENAS PARA FACILITAR
nomes <- excel %>% 
  str_split(., "/|\\.", #fatia o endereço completo dos arquivos baseado em / e . Aqui pode ter diferença para o Windows em função do uso de \ nos caminhos.
            simplify = TRUE) %>% # transforma o resultado em data frame
  as_tibble() %>% #transforma em data frame tipo tibble
  pull(V2) #transforma a coluna V2 em vetor

load <- excel %>% 
  set_names(., nomes) #atribui à lista de arquivos (excel) os nomes do vetor extraído no passo anterior

#Lê a planilha de AF
af <- map(.x = load, function(arquivo){
  read_excel(arquivo, #lê o arquivo excel que está no endereço em load
             sheet = 5, #lê a segunda planilha do arquivo excel (dados câmeras)
             range = cell_cols(1:7), #define as colunas que devem ser lidas
             #skip = 3, #pula a primeira linha do arquivo (por causa da mesclagem)
             na = c("NA", "na"),
             col_names = TRUE) %>% 
    remove_empty("rows") %>% #remove as linhas vazias (em que os elementos de todas as colunas são NA)
    #set_names(nome_colunas_af) %>%  #renomeia as colunas conforme os nomes do vetor nome_colunas_af
    select(1, 7) %>% #seleciona apenas as colunas 1 (id cameras) e coluna 3 (das fotos)
    separate(
      col = "Camera_vision_photo",
      into = "Camera_vision_photo",
      #sep = "\\.\\w{2,}g$"
      sep = "\\.[[:lower:]]{2,}g$|\\.[[:upper:]]{2,}G$" #esquema necessario por causa dos formatos das imagens, jpeg, JPEG, png, PNG...
    )
})

#Lê as pastas com as imagens
imagens <- list.files(path = "Dados", #fuça a pasta "Dados" onde estao as fotos
                      pattern = "\\.\\w{2,}g$", #busca um padrão de arquivo que termine com ponto final (\\.), com pelo menos duas letras (\\w{2,}) e um "g"
                      full.names = TRUE, #retorna o caminho completo do arquivo
                      recursive = TRUE, #fuça nas subpastas do caminho escolhido
                      ignore.case = TRUE #ignora se o valor é maiúsculo ou minúsculo
)

#CRIA UMA TABELA COM A LISTA DOS NOMES DOS ARQUIVOS DENTRO DE CADA PASTA
nomes_img <- imagens %>% 
  str_split(., "/", #fatia o endereço completo dos arquivos baseado em / e . Aqui pode ter diferença para o Windows em função do uso de \ nos caminhos.
            simplify = TRUE) %>%  # transforma o resultado em data frame
  as_tibble() %>%  #transforma em data frame tipo tibble
  # pull(V3) #transforma a coluna V2 em vetor
  separate(col = V4,
           into = "nome_arq",
           sep = "\\.[[:lower:]]{2,}g$|\\.[[:upper:]]{2,}G$") %>% 
  group_by(V2) %>% 
  nest()

NEST <- nomes_img$data %>% 
  set_names(x = ., nm = nomes_img$V2) #separa as tabelas aninhadas e a cada uma delas dá o nome da sua respectiva planilha

#VERIFICA IMAGENS QUE ESTAO NA PLANILHA, MAS NAO ESTAO NA PASTA DAS IMAGENS
checa_foto_faltando <- map2(set_names(names(NEST), nomes_img$V2), names(af), function(x, y){ #map para iterar entre todas as tabelinhas
  a <- NEST[[x]] #para cada um dos nomes (tabelas) de x, cria o objeto a
  b <- af[[y]] #para cada um dos nomes (tabelas) de y, cria o objeto b
  
  c <- a %>% 
    pull(nome_arq)
  
  d <- b %>% 
    filter(!is.na(Camera_vision_photo)) %>% 
    pull(Camera_vision_photo)
  
  setdiff(d, c)
}) %>% 
  plyr::ldply(., .fun = data.frame, .id = "Pasta") %>% #transforma o resultado de listas para data frame
  as_tibble() %>% #transforma em tabela do tipo tibble
  rename(ERRO = 2) %>% #renomeia a coluna 2 para "ERRO"
  filter(!str_detect(string = ERRO, pattern = "character(0)")) #filtra os dados que não têm problema

openxlsx::write.xlsx(x = checa_foto_faltando,
                     file = "Output/PLAN_ERROS_CHECA_IMAGEM2.xlsx", #salva o arquivo
                     asTable = TRUE)

#########################
# RODAR AQUI PARA VER QUAIS PASTAS FALTAM ALGUM ARQUIVO DE IMAGEM E INSERIR O NADA.JPG - 
#ALGUMAS PLANILHAS NAO TEM FOTOS, EH NECESSARIO COLOCAR ISSO PARA NAO DAR ERRO
NEST_AF <- nomes_img$data %>% 
  lapply(., function(x) filter(x, V3 == "Cameratrap")) %>% 
  set_names(x = ., nm = nomes_img$V2) #separa as tabelas aninhadas e a cada uma delas dá o nome da sua respectiva planilha

nomes_img %>% 
  distinct(V2) %>% 
  print(n = Inf)

#VERIFICA QUAIS IMAGENS ESTAO NA PASTA E NAO ESTAO NA PLANILHA, OU SEJA, SAO FOTOS A MAIS
checa_foto_sobrando <- map2(set_names(names(NEST_AF), nomes_img$V2), names(af), function(x, y){ #map para iterar entre todas as tabelinhas
  a <- NEST_AF[[x]] #para cada um dos nomes (tabelas) de x, cria o objeto a
  b <- af[[y]] #para cada um dos nomes (tabelas) de y, cria o objeto b
  
  c <- a %>% 
    pull(nome_arq)
  
  d <- b %>% 
    filter(!is.na(Camera_vision_photo)) %>% 
    pull(Camera_vision_photo)
  
  setdiff(c, d)
}) %>% 
  plyr::ldply(., .fun = data.frame, .id = "Pasta") %>% #transforma o resultado de listas para data frame
  as_tibble() %>% #transforma em tabela do tipo tibble
  rename(ERRO = 2) %>% #renomeia a coluna 2 para "ERRO"
  filter(!str_detect(string = ERRO, pattern = "character(0)")) %T>% #filtra os dados que não têm problema
  openxlsx::write.xlsx(x = checa_foto_sobrando,
                       file = "Output/PLAN_ERROS_CHECA_IMAGEM_SOBRANDO.xlsx", #salva o arquivo
                       asTable = TRUE)
