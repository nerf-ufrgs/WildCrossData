library(janitor)
library(magrittr)

#Cria um ID único para colunas que possuam IDs duplicados
id_unico <- suppressMessages(
  function(x, sep = "_"){
    x_com_id <- x %>% 
      rowid_to_column()
    
    x_com_id %>% 
      dplyr::group_by(Structure_ID) %>% 
      dplyr::add_count(Structure_ID, Camera_ID, name = "double") %>% 
      dplyr::mutate(ID_dup = dplyr::row_number(Camera_ID),
                    nome_formulario_dup = dplyr::if_else(condition = double == 1,
                                                         true = Camera_ID,
                                                         false = stringr::str_c(Camera_ID, sep, LETTERS[ID_dup]))) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(rowid, nome_formulario_dup, double) %>% 
      dplyr::left_join(x_com_id, ., by = "rowid") %>% 
      dplyr::mutate(Camera_ID = dplyr::if_else(!is.na(nome_formulario_dup), nome_formulario_dup, Camera_ID)) %>%
      dplyr::select(-nome_formulario_dup, -rowid)
  }
)
