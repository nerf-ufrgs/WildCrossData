library(rorcid)
library(httpuv)
library(readxl)
library(tidyverse)

# Authorizing through Orcid
rorcid::orcid_auth() #como não vai encontrar um token no seu computador, vai necessitar fazer login e autorizar via navegador. Um token vai ser gerado e mostrado no console. Copiar apenas o código alfanumérico excluindo o termo "Bearer "

usethis::edit_r_environ() #Após copiar o token, rodar esse comando e transcrever o que está abaixo dentro do .Renviron. 
#ATENÇÃO! O token deve ser inserido entre as aspas e a segunda linha deve ter o seu # retirado.


# Reading files
excel <- list.files(
  path = "Excel", #fuça a pasta "EXCEL_MOD"
  pattern = "^\\w.+xlsx$", #busca um padrão de arquivo que termine com "xlsx"
  full.names = TRUE, #retorna o caminho completo do arquivo
  recursive = TRUE
) #fuça nas subpastas do caminho escolhido

names <- excel |>  
  str_split("/|\\.") |>  #fatia o endereço completo dos arquivos baseado em / e . Aqui pode ter diferença para o Windows em função do uso de \ nos caminhos.
  map_chr(pluck(2)) #transforma em data frame tipo tibble

load <- excel |> 
  set_names(names) #atribui à lista de arquivos (excel) os nomes do vetor extraído no passo anterior

#Checa consistência de ORCID - existência e leitura na base.
checa_orcid <- map(load, function(arquivo){
  nest <- read_excel(arquivo,
                     sheet = 7, # Author data
                     na = c("NA", "na"),
                     col_names = TRUE) |> 
    janitor::clean_names() |> 
    rename(nome = 1) |> 
    mutate(orcid_num = str_sub(orcid, start = -19)) |> 
    pull(orcid_num)
})

a <- checa_orcid$Ana_Delciellos[1]

rorcid::as.orcid(a)

