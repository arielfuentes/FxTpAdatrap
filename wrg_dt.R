vjs_fix <- function(df, per){
  #libraries
  library(tibble)
  library(dtplyr)
  library(dplyr)
  library(data.table)
  library(tidyr)
  library(readr)
  #####################
  #fix "servicio"
  v_data <- filter(.data = df, Dda != 0) %>% 
    mutate(Dda = case_when(tipodia == "LABORAL" ~ Dda/5,
                           T ~ Dda),
           serv_1era_etapa = case_when(tipotransporte_1era %in% c("BUS", "ZP") ~ serv_1era_etapa,
                                       tipotransporte_1era == "METROTREN" ~ tipotransporte_1era,
                                       T ~ linea_metro_subida_1
                                       ),
           serv_2da_etapa = case_when(tipotransporte_2da %in% c("BUS", "ZP") ~ serv_2da_etapa,
                                      tipotransporte_2da == "METROTREN" ~ tipotransporte_2da,
                                      T ~ linea_metro_subida_2
                                      ),
           serv_3era_etapa = case_when(tipotransporte_3era %in% c("BUS", "ZP") ~ serv_3era_etapa,
                                       tipotransporte_3era == "METROTREN" ~ tipotransporte_3era,
                                       T ~ linea_metro_subida_3
                                       ),
           serv_4ta_etapa = case_when(tipotransporte_4ta %in% c("BUS", "ZP") ~ serv_4ta_etapa,
                                      tipotransporte_4ta == "METROTREN" ~ tipotransporte_4ta,
                                      T ~ linea_metro_subida_4
                                      ),
           valid = case_when((netapa == 1 &
                                serv_1era_etapa != "-" & 
                                paraderosubida_1era != "-" & 
                                paraderobajada_1era != "-") |
                               (netapa == 2 & 
                                  serv_1era_etapa != "-" & 
                                  paraderosubida_1era != "-" & 
                                  paraderobajada_1era != "-" &
                                  serv_2da_etapa != "-" & 
                                  paraderosubida_2da != "-" & 
                                  paraderobajada_2da != "-") |
                               (netapa == 3 & 
                                  serv_1era_etapa != "-" & 
                                  paraderosubida_1era != "-" & 
                                  paraderobajada_1era != "-" &
                                  serv_2da_etapa != "-" & 
                                  paraderosubida_2da != "-" & 
                                  paraderobajada_2da != "-" &
                                  serv_3era_etapa != "-" & 
                                  paraderosubida_3era != "-" & 
                                  paraderobajada_3era != "-") |
                               (netapa == 4 & 
                                  serv_1era_etapa != "-" & 
                                  paraderosubida_1era != "-" & 
                                  paraderobajada_1era != "-" &
                                  serv_2da_etapa != "-" & 
                                  paraderosubida_2da != "-" & 
                                  paraderobajada_2da != "-" &
                                  serv_3era_etapa != "-" & 
                                  paraderosubida_3era != "-" & 
                                  paraderobajada_3era != "-" &
                                  serv_4ta_etapa != "-" & 
                                  paraderosubida_4ta != "-" & 
                                  paraderobajada_4ta != "-") ~ 1,  
                             T ~ 0
                             )
           ) %>%
    select(-linea_metro_subida_1,
           -linea_metro_subida_2,
           -linea_metro_subida_3,
           -linea_metro_subida_4,
           -linea_metro_bajada_1,
           -linea_metro_bajada_2,
           -linea_metro_bajada_3,
           -linea_metro_bajada_4
           ) %>%
    unite(paraderosubida, paraderobajada,
          col = "OD",
          sep = "*",
          remove = F)
  ####################
  #valid_status
  valid_stat <- group_by(.data = v_data,
                         paraderosubida,
                         paraderobajada,
                         valid) %>%
    summarise(DDA = sum(Dda)) %>%
    spread(key = valid,
           value = DDA) %>%
    mutate(`0`= replace_na(`0`, 0),
           `1`= replace_na(`1`, 0),
           f_valid = ((`1` + `0`)/`1`)#,
           #Nva_OD = f_valid*`1`
           ) %>%
    unite(paraderosubida, paraderobajada,
          col = "OD",
          sep = "*",
          remove = F
          ) %>%
    ungroup() %>%
    select(OD, f_valid) %>%
    filter(f_valid != Inf)
  v_data <- left_join(v_data, valid_stat, by = "OD") %>%
    mutate(DdaAjus = Dda*f_valid,
           interpar = case_when(paraderosubida == paraderobajada ~ 0,
                                T ~ 1)
           ) %>% #rm invalid stop_OD
    filter(valid == 1, interpar == 1) %>%
    select(-c("Dda", "valid", "f_valid", "interpar", "OD"))
  #adj demand
  dat_O <- sum(v_data$DdaAjus, na.rm = T)
  ####################
  #reduce data by PO status
  v_dataMTX <- left_join(v_data, distinct(select(.data = dicc_nodo,
                                     paraderosubida = CODTB9,
                                     IDSub = IDNODORED
                                     ))
                      ) %>%
    left_join(distinct(select(.data = dicc_nodo,
                     paraderobajada = CODTB9,
                     IDBaj = IDNODORED
                     ))
              ) %>%
    mutate(nodo = if_else(is.na(IDSub) | is.na(IDBaj),
                          true = 0,
                          false = 1,
                          missing = NA_real_
                          )
           ) %>%
    filter(nodo == 1) #only the existent nodes
  #total after remove inexistent nodes
  dat_d <- sum(v_data$DdaAjus, na.rm = T)
  ########################
  #create loss data report
  loss_rep <- tibble(Original = dat_O, Posterior = dat_d, 
                     Perdido = dat_O*(1-dat_d/dat_O),
                     PorcPerdido = paste0((1-dat_d/dat_O)*100, "%"), 
                     Periodo = per)
  write_delim(x = loss_rep, 
              path = paste0("output/loss_rep/loss_rep", per, ".csv"), 
              na = "NA", 
              delim = ";"
              )
  rm(dat_O, dat_d, loss_rep)
###########################################
####add submode
  
  v_data <- left_join(v_data, distinct(select(.data = dicc_submode,
                                     serv_1era_etapa = Servicio,
                                     submodo_1era = Categoría))
                      ) %>%
    left_join(distinct(select(.data = dicc_submode,
                     serv_2da_etapa = Servicio,
                     submodo_2da = Categoría
                     )
              )) %>%
    left_join(distinct(select(.data = dicc_submode,
                             serv_3era_etapa = Servicio,
                             submodo_3era = Categoría
                     )
    )) %>%
    left_join(distinct(select(.data = dicc_submode,
                     serv_4ta_etapa = Servicio,
                     submodo_4ta = Categoría
                     )
              ))
###############################
####add SIMT stops
  v_data <- left_join(v_data, distinct(select(.data = par,
                                     paraderosubida= `Código paradero TS`,
                                     parususubida = `Código  paradero Usuario`
                                     ))
                      ) %>%
    left_join(distinct(select(.data = par,
                     paraderobajada = `Código paradero TS`,
                     parusubajada = `Código  paradero Usuario`
                     ))
              ) %>%
    left_join(distinct(select(.data = par,
                     paraderosubida_1era = `Código paradero TS`,
                     parususubida_1era = `Código  paradero Usuario`
                     ))
              ) %>%
    left_join(distinct(select(.data = par,
                             paraderosubida_2da = `Código paradero TS`,
                             parususubida_2da = `Código  paradero Usuario`
                     ))
              ) %>%
    left_join(distinct(select(.data = par,
                     paraderosubida_3era = `Código paradero TS`,
                     parususubida_3era = `Código  paradero Usuario`
                     ))
              ) %>%
    left_join(distinct(select(.data = par,
                     paraderosubida_4ta = `Código paradero TS`,
                     parususubida_4ta = `Código  paradero Usuario`
                     ))
              ) %>%
    left_join(distinct(select(.data = par,
                     paraderobajada_1era = `Código paradero TS`,
                     parusubajada_1era = `Código  paradero Usuario`
                     ))
              ) %>%
    left_join(distinct(select(.data = par,
                     paraderobajada_2da = `Código paradero TS`,
                     parusubajada_2da = `Código  paradero Usuario`
                     ))
              ) %>%
    left_join(distinct(select(.data = par,
                     paraderobajada_3era = `Código paradero TS`,
                     parusubajada_3era = `Código  paradero Usuario`
                     ))
              ) %>%
    left_join(distinct(select(.data = par,
                     paraderobajada_4ta = `Código paradero TS`,
                     parusubajada_4ta = `Código  paradero Usuario`
                     ))
              )
  ########################
  #write mtx
  mtx <- select(.data = v_dataMTX, IDSub, IDBaj, DdaAjus)
  write_delim(x = mtx,
              path = paste0("output/mtx/mtx", per, ".csv"),
              na = "NA",
              delim = ";"
              )
  #write trip table
  v_data <- v_data %>%
    replace(is.na(.), "-") %>%
    select(-DdaAjus, everything(), DdaAjus)

  write_delim(x = v_data,
              path = paste0("output/trip/trip", per, ".csv"),
              na = "NA",
              delim = ";"
              )
  #return(v_data)
}
