#libraries
library(purrr)
library(readr)
library(dplyr)

#parameters
file_par <- "data/2019-07-06_consolidado_anexo4_(Circunvalación)_anual.xlsx"
file_nd <- "data/dicc_nodoV2_2.csv"
file_submd <- "data/dicc_submodo.csv"
source(file = "code/ipt.R", encoding = "utf-8")
source(file = "code/vjs_stpfix.R", encoding = "utf-8")
source(file = "code/wrg_dt.R", encoding = "utf-8")
DDBB_v <- "[viajes201908]"

lapply(X = per$periodomediodeviaje, 
       FUN = function(x) vjs_fix(vjs_stpfix(df = BBDD_vjs(DDBB_v, x)
                                           ),
                                 x)
       )

g <- list.files(path = "output/loss_rep/", full.names = T)

g %>%
  purrr::map(function(x) {
    read_delim(x, delim = ";")
  }) %>%
  reduce(bind_rows) %>%
  write_delim(path = "output/loss_rep/loss_rep_082019.csv", delim = ";")
