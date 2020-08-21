#parameters
file_par <- "data/2019-07-06_consolidado_anexo4_(Circunvalación)_anual.xlsx"
file_nd <- "data/dicc_nodoV2_2.csv"
file_submd <- "data/dicc_submodo.csv"
source(file = "code/ipt.R", encoding = "utf-8")
source(file = "code/vjs_stpfix.R", encoding = "utf-8")
source(file = "code/wrg_dt.R", encoding = "utf-8")
DDBB_v <- "[viajes201908]"
raw_vjs <- vjs_stpfix(DDBB_v, per)

lapply(X = per$periodomediodeviaje, FUN = function(x) vjs_fix(raw_vjs, x))
