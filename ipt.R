library(readr)
library(dplyr)
library(readxl)
library(stringr)
#####dictionaries
dicc_nodo <- read_csv2(file = file_nd, 
                       locale = locale(encoding = "latin1")
) %>%
  select(-c('IDNODORED_1', 'SIMT_1', 'GTFS'))

par <- read_xlsx(path = file_par) %>%
  select("Código paradero TS", "Código  paradero Usuario") %>%
  distinct() %>%
  bind_rows(select(filter(dicc_nodo, 
                          str_detect(dicc_nodo$SIMT, "^L")
                          ),
                   "Código paradero TS" = CODTB9, 
                   "Código  paradero Usuario" = SIMT
                   )
            )

per <- read.csv2(file = "data/periodos.csv")

dicc_submode <- read_csv2(file = file_submd, locale = locale(encoding = "latin1"), 
                          col_types = cols(Servicio = col_character(), Categoría = col_character(),
                                           `Categoría dos` = col_character()))
BBDD_vjs <- function(DDBB_v, per){
  library(DBI)
  library(dtplyr)
  library(dplyr)
  library(data.table)
  #connection
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver   = "SQL Server",
                        Server   = "@@@@@@@@@@@@@",
                        Database = "uchile",
                        UID      = rstudioapi::askForPassword("Database user"),
                        PWD      = rstudioapi::askForPassword("Database password"),
                        encoding = "latin1")
  #query
  sql_vjs <- paste0("SELECT SUM(CAST(factorexpansion AS FLOAT)) AS Dda, periodomediodeviaje, tipodia, 
paraderosubida, paraderobajada, netapa, tipotransporte_1era,	tipotransporte_2da, tipotransporte_3era,	
tipotransporte_4ta,	serv_1era_etapa,	serv_2da_etapa,	serv_3era_etapa, serv_4ta_etapa,	
linea_metro_subida_1,	linea_metro_subida_2,	linea_metro_subida_3,	linea_metro_subida_4,	
linea_metro_bajada_1,	linea_metro_bajada_2,	linea_metro_bajada_3,	linea_metro_bajada_4,	
paraderosubida_1era,	paraderosubida_2da,	paraderosubida_3era,	paraderosubida_4ta,	
paraderobajada_1era,	paraderobajada_2da,	paraderobajada_3era,	paraderobajada_4ta
FROM [uchile].[dbo].", DDBB_v,"
WHERE periodomediodeviaje = '", per, "' AND paraderosubida <> '-' AND 
paraderobajada <> '-' AND factorexpansion <> '-'
GROUP BY  tipodia, netapa, periodomediodeviaje, paraderosubida, paraderobajada, tipotransporte_1era,	
tipotransporte_2da, tipotransporte_3era,	tipotransporte_4ta,	serv_1era_etapa,	serv_2da_etapa,	
serv_3era_etapa, serv_4ta_etapa,	linea_metro_subida_1,	linea_metro_subida_2,	
linea_metro_subida_3,	linea_metro_subida_4,	linea_metro_bajada_1,	linea_metro_bajada_2,	
linea_metro_bajada_3,	linea_metro_bajada_4,	paraderosubida_1era,	paraderosubida_2da,	
paraderosubida_3era,	paraderosubida_4ta,	paraderobajada_1era,	paraderobajada_2da,	
paraderobajada_3era,	paraderobajada_4ta;")  
  
  #raw data
  raw_vjs <- lazy_dt(DBI::dbGetQuery(conn = con, 
                                    statement = sql_vjs)
  )
}
