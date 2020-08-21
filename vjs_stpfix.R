# source(file = "code/ipt.R", encoding = "utf-8")
# 
# #parameters
# DDBB_v <- "[viajes201908]"
# per <- "04 - PMA"
# raw_vjs <- vjs_stpfix()
#fixing metro stations names
vjs_stpfix <- function(df){
  library(tibble)
  library(dtplyr)
  library(dplyr)
  library(data.table)
  vjs <- df %>% ##BBDD_vjs
    as_tibble() %>%
    mutate(paraderosubida_1era = replace(paraderosubida_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_subida_1 %in% c("L1", "L1-L5") &
                                                 paraderosubida_1era == "BAQUEDANO"), 
                                         "BAQUEDANO L1"), 
           paraderosubida_1era = replace(paraderosubida_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_subida_1 %in% c("L1", "L1-L2") & 
                                                 paraderosubida_1era == "LOS HEROES"), 
                                         "LOS HEROES L1"),
           paraderosubida_1era = replace(paraderosubida_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_subida_1 %in% c("L1", "L1-L5") & 
                                                 paraderosubida_1era == "SAN PABLO"), 
                                         "SAN PABLO L1"),
           paraderosubida_1era = replace(paraderosubida_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_subida_1 %in% c("L1", "L1-L2") & 
                                                 paraderosubida_1era == "LOS LEONES"), 
                                         "LOS LEONES L1"),
           paraderosubida_1era = replace(paraderosubida_1era, 
                                         which(tipotransporte_1era == "METRO" &
                                                 linea_metro_subida_1 %in% c("L1", "L1-L4") & 
                                                 paraderosubida_1era == "TOBALABA"), 
                                         "TOBALABA L1"),
           paraderosubida_1era = replace(paraderosubida_1era, 
                                         which(tipotransporte_1era == "METRO" &
                                                 linea_metro_subida_1 %in% c("L1", "L1-L3") & 
                                                 paraderosubida_1era == "UNIVERSIDAD DE CHILE"), 
                                         "UNIVERSIDAD DE CHILE L1"), 
           paraderosubida_1era = replace(paraderosubida_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_subida_1 %in% c("L2", "L2-L3") &
                                                 paraderosubida_1era == "CAL Y CANTO"), 
                                         "CAL Y CANTO L2"), 
           paraderosubida_1era = replace(paraderosubida_1era,
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_subida_1 %in% c("L2", "L2-L6") &
                                                 paraderosubida_1era == "FRANKLIN"), 
                                         "FRANKLIN L2"), 
           paraderosubida_1era = replace(paraderosubida_1era,
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_subida_1 %in% c("L2", "L2-L4A") &
                                                 paraderosubida_1era == "LA CISTERNA"), 
                                         "LA CISTERNA L2"), 
           paraderosubida_1era = replace(paraderosubida_1era,
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_subida_1 %in% c("L2", "L2-L1") &
                                                 paraderosubida_1era == "LOS HEROES"), 
                                         "LOS HEROES L2"), 
           paraderosubida_1era = replace(paraderosubida_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_subida_1 %in% c("L2", "L2-L5") &
                                                 paraderosubida_1era == "SANTA ANA"), 
                                         "SANTA ANA L2"), 
           paraderosubida_1era = replace(paraderosubida_1era,
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_subida_1 %in% c("L4", "L4-L3") &
                                                 paraderosubida_1era == "PLAZA EGANA"), 
                                         "PLAZA EGANA L4"), 
           paraderosubida_1era = replace(paraderosubida_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_subida_1 %in% c("L4", "L4-L1") &
                                                 paraderosubida_1era == "TOBALABA"), "TOBALABA L4"), 
           paraderosubida_1era = replace(paraderosubida_1era,
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_subida_1 %in% c("L4", "L4-L5") &
                                                 paraderosubida_1era == "VICENTE VALDES"), 
                                         "VICENTE VALDES L4"), 
           paraderosubida_1era = replace(paraderosubida_1era,
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_subida_1 %in% c("L4", "L4-L4A") &
                                                 paraderosubida_1era == "VICUNA MACKENNA"), 
                                         "VICUNA MACKENNA L4"), 
           paraderosubida_1era = replace(paraderosubida_1era,
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_subida_1 %in% c("L5", "L5-L1") &
                                                 paraderosubida_1era == "BAQUEDANO"), 
                                         "BAQUEDANO L5"),
           paraderosubida_1era = replace(paraderosubida_1era,
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_subida_1 %in% c("L5", "L5-L1") &
                                                 paraderosubida_1era == "SAN PABLO"), 
                                         "SAN PABLO L5"),
           paraderosubida_1era = replace(paraderosubida_1era,
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_subida_1 %in% c("L5", "L5-L3") &
                                                 paraderosubida_1era == "IRARRAZAVAL"), 
                                         "IRARRAZAVAL L5"), 
           paraderosubida_1era = replace(paraderosubida_1era,
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_subida_1 %in% c("L5", "L5-L6") & 
                                                 paraderosubida_1era %in% c("NUBLE", "NUBLE EU")), 
                                         "NUBLE L5"),
           paraderosubida_1era = replace(paraderosubida_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_subida_1 %in% c("L5", "L5-L3") &
                                                 paraderosubida_1era == "PLAZA DE ARMAS"), 
                                         "PLAZA DE ARMAS L5"),
           paraderosubida_1era = replace(paraderosubida_1era,
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_subida_1 %in% c("L6", "L6-L2") &
                                                 paraderosubida_1era == "FRANKLIN"), 
                                         "FRANKLIN L6"), 
           paraderosubida_1era = replace(paraderosubida_1era,
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_subida_1 %in% c("L6", "L6-MT") &
                                                 paraderosubida_1era == "LO VALLEDOR"), 
                                         "LO VALLEDOR L6"), 
           paraderosubida_1era = replace(paraderosubida_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_subida_1 %in% c("L6", "L6-L5") &
                                                 paraderosubida_1era %in% c("NUBLE", "NUBLE EU")), 
                                         "NUBLE L6"),
           paraderosubida_1era = replace(paraderosubida_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_subida_1 %in% c("L6", "L6-L3") &
                                                 paraderosubida_1era == "NUNOA"), 
                                         "NUNOA L6"), 
           paraderosubida_1era = replace(paraderosubida_1era,
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_subida_1 %in% c("L3", "L3-L2") &
                                                 paraderosubida_1era == "CAL Y CANTO"), 
                                         "CAL Y CANTO L3"),
           paraderosubida_1era = replace(paraderosubida_1era,
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_subida_1 %in% c("L3", "L3-L5") & 
                                                 paraderosubida_1era == "PLAZA DE ARMAS"), 
                                         "PLAZA DE ARMAS L3"), 
           paraderosubida_1era = replace(paraderosubida_1era,
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_subida_1 %in% c("L3", "L3-L1") &
                                                 paraderosubida_1era == "UNIVERSIDAD DE CHILE"), 
                                         "UNIVERSIDAD DE CHILE L3"), 
           paraderosubida_1era = replace(paraderosubida_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_subida_1 %in% c("L3", "L3-L5") & 
                                                 paraderosubida_1era == "IRARRAZAVAL"), 
                                         "IRARRAZAVAL L3"), 
           paraderosubida_1era = replace(paraderosubida_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_subida_1 %in% c("L3", "L3-L6") & 
                                                 paraderosubida_1era == "NUNOA"), 
                                         "NUNOA L3"), 
           paraderosubida_1era = replace(paraderosubida_1era,
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_subida_1 %in% c("L3", "L3-L4") &
                                                 paraderosubida_1era == "PLAZA EGANA"), 
                                         "PLAZA EGANA L3"), 
           paraderosubida_2da = replace(paraderosubida_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_subida_2 %in% c("L1", "L1-L5") &
                                                paraderosubida_2da == "BAQUEDANO"), 
                                        "BAQUEDANO L1"), 
           paraderosubida_2da = replace(paraderosubida_2da, 
                                         which(tipotransporte_2da == "METRO" & 
                                                 linea_metro_subida_2 %in% c("L1", "L1-L5") & 
                                                 paraderosubida_2da == "SAN PABLO"), 
                                         "SAN PABLO L1"),
           paraderosubida_2da = replace(paraderosubida_2da, 
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_subida_2 %in% c("L1", "L1-L2") &
                                                paraderosubida_2da == "LOS HEROES"), 
                                        "LOS HEROES L1"), 
           paraderosubida_2da = replace(paraderosubida_2da, 
                                        which(tipotransporte_2da == "METRO" &
                                                linea_metro_subida_2 %in% c("L1", "L1-L2") & 
                                                paraderosubida_2da == "LOS LEONES"), 
                                        "LOS LEONES L1"),
           paraderosubida_2da = replace(paraderosubida_2da, 
                                        which(tipotransporte_2da == "METRO" &
                                                linea_metro_subida_2 %in% c("L1", "L1-L4") & 
                                                paraderosubida_2da == "TOBALABA"), 
                                        "TOBALABA L1"),
           paraderosubida_2da = replace(paraderosubida_2da, 
                                        which(tipotransporte_2da == "METRO" &
                                                linea_metro_subida_2 %in% c("L1", "L1-L3") & 
                                                paraderosubida_2da == "UNIVERSIDAD DE CHILE"), 
                                        "UNIVERSIDAD DE CHILE L1"), 
           paraderosubida_2da = replace(paraderosubida_2da, 
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_subida_2 %in% c("L2", "L2-L3") &
                                                paraderosubida_2da == "CAL Y CANTO"), 
                                        "CAL Y CANTO L2"), 
           paraderosubida_2da = replace(paraderosubida_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_subida_2 %in% c("L2", "L2-L6") & 
                                                paraderosubida_2da == "FRANKLIN"), 
                                        "FRANKLIN L2"), 
           paraderosubida_2da = replace(paraderosubida_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_subida_2 %in% c("L2", "L2-L4A") &
                                                paraderosubida_2da == "LA CISTERNA"), 
                                        "LA CISTERNA L2"), 
           paraderosubida_2da = replace(paraderosubida_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_subida_2 %in% c("L2", "L2-L1") &
                                                paraderosubida_2da == "LOS HEROES"), 
                                        "LOS HEROES L2"), 
           paraderosubida_2da = replace(paraderosubida_2da, 
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_subida_2 %in% c("L2", "L2-L5") &
                                                paraderosubida_2da == "SANTA ANA"), 
                                        "SANTA ANA L2"), 
           paraderosubida_2da = replace(paraderosubida_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_subida_2 %in% c("L4", "L4-L3") &
                                                paraderosubida_2da == "PLAZA EGANA"), 
                                        "PLAZA EGANA L4"), 
           paraderosubida_2da = replace(paraderosubida_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_subida_2 %in% c("L4", "L4-L1") &
                                                paraderosubida_2da == "TOBALABA"), 
                                        "TOBALABA L4"), 
           paraderosubida_2da = replace(paraderosubida_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_subida_2 %in% c("L4", "L4-L5") &
                                                paraderosubida_2da == "VICENTE VALDES"), 
                                        "VICENTE VALDES L4"), 
           paraderosubida_2da = replace(paraderosubida_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_subida_2 %in% c("L4", "L4-L4A") &
                                                paraderosubida_2da == "VICUNA MACKENNA"), 
                                        "VICUNA MACKENNA L4"), 
           paraderosubida_2da = replace(paraderosubida_2da, 
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_subida_2 %in% c("L5", "L5-L1") &
                                                paraderosubida_2da == "BAQUEDANO"), 
                                        "BAQUEDANO L5"),
           paraderosubida_2da = replace(paraderosubida_2da, 
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_subida_2 %in% c("L5", "L5-L1") &
                                                paraderosubida_2da == "SAN PABLO"), 
                                        "SAN PABLO L5"),
           paraderosubida_2da = replace(paraderosubida_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_subida_2 %in% c("L5", "L5-L3") &
                                                paraderosubida_2da == "IRARRAZAVAL"), 
                                        "IRARRAZAVAL L5"), 
           paraderosubida_2da = replace(paraderosubida_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_subida_2 %in% c("L5", "L5-L6") &
                                                paraderosubida_2da %in% c("NUBLE", "NUBLE EU")), 
                                        "NUBLE L5"), 
           paraderosubida_2da = replace(paraderosubida_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_subida_2 %in% c("L5", "L5-L3") &
                                                paraderosubida == "PLAZA DE ARMAS"), 
                                        "PLAZA DE ARMAS L5"),
           paraderosubida_2da = replace(paraderosubida_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_subida_2 %in% c("L6", "L6-L2") &
                                                paraderosubida_2da == "FRANKLIN"), 
                                        "FRANKLIN L6"), 
           paraderosubida_2da = replace(paraderosubida_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_subida_2 %in% c("L6", "L6-MT") &
                                                paraderosubida_2da == "LO VALLEDOR"), 
                                        "LO VALLEDOR L6"), 
           paraderosubida_2da = replace(paraderosubida_2da, 
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_subida_2 %in% c("L6", "L6-L5") &
                                                paraderosubida_2da %in% c("NUBLE", "NUBLE EU")), 
                                        "NUBLE L6"), 
           paraderosubida_2da = replace(paraderosubida_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_subida_2 %in% c("L6", "L6-L3") &
                                                paraderosubida_2da == "NUNOA"), 
                                        "NUNOA L6"), 
           paraderosubida_2da = replace(paraderosubida_2da, 
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_subida_2 %in% c("L3", "L3-L2") &
                                                paraderosubida_2da == "CAL Y CANTO"), 
                                        "CAL Y CANTO L3"),
           paraderosubida_2da = replace(paraderosubida_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_subida_2 %in% c("L3", "L3-L5") &
                                                paraderosubida_2da == "PLAZA DE ARMAS"), 
                                        "PLAZA DE ARMAS L3"), 
           paraderosubida_2da = replace(paraderosubida_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_subida_2 %in% c("L3", "L3-L1") &
                                                paraderosubida_2da == "UNIVERSIDAD DE CHILE"), 
                                        "UNIVERSIDAD DE CHILE L3"), 
           paraderosubida_2da = replace(paraderosubida_2da, 
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_subida_2 %in% c("L3", "L3-L5") & 
                                                paraderosubida_2da == "IRARRAZAVAL"), 
                                        "IRARRAZAVAL L3"), 
           paraderosubida_2da = replace(paraderosubida_2da, 
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_subida_2 %in% c("L3", "L3-L6") & 
                                                paraderosubida_2da == "NUNOA"), 
                                        "NUNOA L3"), 
           paraderosubida_2da = replace(paraderosubida_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_subida_2 %in% c("L3", "L3-L4") &
                                                paraderosubida_2da == "PLAZA EGANA"), 
                                        "PLAZA EGANA L3"),
           paraderosubida_3era = replace(paraderosubida_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_subida_3 %in% c("L1", "L1-L5") & 
                                                 paraderosubida_3era == "BAQUEDANO"), 
                                         "BAQUEDANO L1"), 
           paraderosubida_3era = replace(paraderosubida_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_subida_3 %in% c("L1", "L1-L5") & 
                                                 paraderosubida_3era == "SAN PABLO"), 
                                         "SAN PABLO L1"),
           paraderosubida_3era = replace(paraderosubida_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_subida_3 %in% c("L1", "L1-L2") &
                                                 paraderosubida_3era == "LOS HEROES"), 
                                         "LOS HEROES L1"), 
           paraderosubida_3era = replace(paraderosubida_3era, 
                                         which(tipotransporte_3era == "METRO" &
                                                 linea_metro_subida_3 %in% c("L1", "L1-L2") & 
                                                 paraderosubida_3era == "LOS LEONES"), 
                                         "LOS LEONES L1"),
           paraderosubida_3era = replace(paraderosubida_3era, 
                                         which(tipotransporte_3era == "METRO" &
                                                 linea_metro_subida_3 %in% c("L1", "L1-L4") & 
                                                 paraderosubida_3era == "TOBALABA"), 
                                         "TOBALABA L1"),
           paraderosubida_3era = replace(paraderosubida_3era, 
                                         which(tipotransporte_3era == "METRO" &
                                                 linea_metro_subida_3 %in% c("L1", "L1-L3") & 
                                                 paraderosubida_3era == "UNIVERSIDAD DE CHILE"),
                                         "UNIVERSIDAD DE CHILE L1"), 
           paraderosubida_3era = replace(paraderosubida_3era,
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_subida_3 %in% c("L2", "L2-L3") &
                                                 paraderosubida_3era == "CAL Y CANTO"), 
                                         "CAL Y CANTO L2"), 
           paraderosubida_3era = replace(paraderosubida_3era,
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_subida_3 %in% c("L2", "L2-L6") &
                                                 paraderosubida_3era == "FRANKLIN"), 
                                         "FRANKLIN L2"), 
           paraderosubida_3era = replace(paraderosubida_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_subida_3 %in% c("L2", "L2-L4A") &
                                                 paraderosubida_3era == "LA CISTERNA"), 
                                         "LA CISTERNA L2"), 
           paraderosubida_3era = replace(paraderosubida_3era,
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_subida_3 %in% c("L2", "L2-L1") &
                                                 paraderosubida_3era == "LOS HEROES"), 
                                         "LOS HEROES L2"), 
           paraderosubida_3era = replace(paraderosubida_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_subida_3 %in% c("L2", "L2-L5") & 
                                                 paraderosubida_3era == "SANTA ANA"), 
                                         "SANTA ANA L2"), 
           paraderosubida_3era = replace(paraderosubida_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_subida_3 %in% c("L4", "L4-L3") &
                                                 paraderosubida_3era == "PLAZA EGANA"), 
                                         "PLAZA EGANA L4"), 
           paraderosubida_3era = replace(paraderosubida_3era,
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_subida_3 %in% c("L4", "L4-L1") & 
                                                 paraderosubida_3era == "TOBALABA"), 
                                         "TOBALABA L4"), 
           paraderosubida_3era = replace(paraderosubida_3era,
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_subida_3 %in% c("L4", "L4-L5") &
                                                 paraderosubida_3era == "VICENTE VALDES"), 
                                         "VICENTE VALDES L4"), 
           paraderosubida_3era = replace(paraderosubida_3era,
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_subida_3 %in% c("L4", "L4-L4A") &
                                                 paraderosubida_3era == "VICUNA MACKENNA"), 
                                         "VICUNA MACKENNA L4"), 
           paraderosubida_3era = replace(paraderosubida_3era,
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_subida_3 %in% c("L5", "L5-L1") &
                                                 paraderosubida_3era == "BAQUEDANO"), 
                                         "BAQUEDANO L5"),
           paraderosubida_3era = replace(paraderosubida_3era,
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_subida_3 %in% c("L5", "L5-L1") &
                                                 paraderosubida_3era == "SAN PABLO"), 
                                         "SAN PABLO L5"),
           paraderosubida_3era = replace(paraderosubida_3era,
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_subida_3 %in% c("L5", "L5-L3") &
                                                 paraderosubida_3era == "IRARRAZAVAL"), 
                                         "IRARRAZAVAL L5"), 
           paraderosubida_3era = replace(paraderosubida_3era,
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_subida_3 %in% c("L5", "L5-L6") &
                                                 paraderosubida_3era %in% c("NUBLE", "NUBLE EU")), 
                                         "NUBLE L5"), 
           paraderosubida_3era = replace(paraderosubida_3era,
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_subida_3 %in% c("L6", "L6-L2") &
                                                 paraderosubida_3era == "FRANKLIN"), 
                                         "FRANKLIN L6"), 
           paraderosubida_3era = replace(paraderosubida_3era,
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_subida_3 %in% c("L6", "L6-MT") &
                                                 paraderosubida_3era == "LO VALLEDOR"), 
                                         "LO VALLEDOR L6"), 
           paraderosubida_3era = replace(paraderosubida_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_subida_3 %in% c("L6", "L6-L5") &
                                                 paraderosubida_3era %in% c("NUBLE", "NUBLE EU")), 
                                         "NUBLE L6"), 
           paraderosubida_3era = replace(paraderosubida_3era,
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_subida_3 %in% c("L5", "L5-L3") &
                                                 paraderosubida_3era == "PLAZA DE ARMAS"), 
                                         "PLAZA DE ARMAS L5"),
           paraderosubida_3era = replace(paraderosubida_3era,
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_subida_3 %in% c("L6", "L6-L3") &
                                                 paraderosubida_3era == "NUNOA"), 
                                         "NUNOA L6"), 
           paraderosubida_3era = replace(paraderosubida_3era,
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_subida_3 %in% c("L3", "L3-L2") &
                                                 paraderosubida_3era == "CAL Y CANTO"), 
                                         "CAL Y CANTO L3"),
           paraderosubida_3era = replace(paraderosubida_3era,
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_subida_3 %in% c("L3", "L3-L5") &
                                                 paraderosubida_3era == "PLAZA DE ARMAS"), 
                                         "PLAZA DE ARMAS L3"), 
           paraderosubida_3era = replace(paraderosubida_3era,
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_subida_3 %in% c("L3", "L3-L1") &
                                                 paraderosubida_3era == "UNIVERSIDAD DE CHILE"), 
                                         "UNIVERSIDAD DE CHILE L3"), 
           paraderosubida_3era = replace(paraderosubida_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_subida_3 %in% c("L3", "L3-L5") & 
                                                 paraderosubida_3era == "IRARRAZAVAL"), 
                                         "IRARRAZAVAL L3"), 
           paraderosubida_3era = replace(paraderosubida_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_subida_3 %in% c("L3", "L3-L6") & 
                                                 paraderosubida_3era == "NUNOA"), 
                                         "NUNOA L3"), 
           paraderosubida_3era = replace(paraderosubida_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_subida_3 %in% c("L3", "L3-L4") &
                                                 paraderosubida_3era == "PLAZA EGANA"), 
                                         "PLAZA EGANA L3"),
           paraderosubida_4ta = replace(paraderosubida_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_subida_4 %in% c("L1", "L1-L5") &
                                                paraderosubida_4ta == "BAQUEDANO"), 
                                        "BAQUEDANO L1"), 
           paraderosubida_4ta = replace(paraderosubida_4ta, 
                                         which(tipotransporte_4ta == "METRO" & 
                                                 linea_metro_subida_4 %in% c("L1", "L1-L5") & 
                                                 paraderosubida_4ta == "SAN PABLO"), 
                                         "SAN PABLO L1"),
           paraderosubida_4ta = replace(paraderosubida_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_subida_4 %in% c("L1", "L1-L2") &
                                                paraderosubida_4ta == "LOS HEROES"), 
                                        "LOS HEROES L1"), 
           paraderosubida_4ta = replace(paraderosubida_4ta, 
                                        which(tipotransporte_4ta == "METRO" &
                                                linea_metro_subida_4 %in% c("L1", "L1-L2") & 
                                                paraderosubida_4ta == "LOS LEONES"), 
                                        "LOS LEONES L1"),
           paraderosubida_4ta = replace(paraderosubida_4ta, 
                                        which(tipotransporte_4ta == "METRO" &
                                                linea_metro_subida_4 %in% c("L1", "L1-L4") & 
                                                paraderosubida_4ta == "TOBALABA"), 
                                        "TOBALABA L1"),
           paraderosubida_4ta = replace(paraderosubida_4ta, 
                                        which(tipotransporte_4ta == "METRO" &
                                                linea_metro_subida_4 %in% c("L1", "L1-L3") & 
                                                paraderosubida_4ta == "UNIVERSIDAD DE CHILE"), 
                                        "UNIVERSIDAD DE CHILE L1"), 
           paraderosubida_4ta = replace(paraderosubida_4ta,
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_subida_4 %in% c("L2", "L2-L3") &
                                                paraderosubida_4ta == "CAL Y CANTO"), 
                                        "CAL Y CANTO L2"), 
           paraderosubida_4ta = replace(paraderosubida_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_subida_4 %in% c("L2", "L2-L6") &
                                                paraderosubida_4ta == "FRANKLIN"), 
                                        "FRANKLIN L2"), 
           paraderosubida_4ta = replace(paraderosubida_4ta,
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_subida_4 %in% c("L2", "L2-L4A") &
                                                paraderosubida_4ta == "LA CISTERNA"), 
                                        "LA CISTERNA L2"), 
           paraderosubida_4ta = replace(paraderosubida_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_subida_4 %in% c("L2", "L2-L1") &
                                                paraderosubida_4ta == "LOS HEROES"), 
                                        "LOS HEROES L2"), 
           paraderosubida_4ta = replace(paraderosubida_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_subida_4 %in% c("L2", "L2-L5") &
                                                paraderosubida_4ta == "SANTA ANA"), 
                                        "SANTA ANA L2"), 
           paraderosubida_4ta = replace(paraderosubida_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_subida_4 %in% c("L4", "L4-L3") &
                                                paraderosubida_4ta == "PLAZA EGANA"), 
                                        "PLAZA EGANA L4"), 
           paraderosubida_4ta = replace(paraderosubida_4ta,
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_subida_4 %in% c("L4", "L4-L1") &
                                                paraderosubida_4ta == "TOBALABA"), 
                                        "TOBALABA L4"), 
           paraderosubida_4ta = replace(paraderosubida_4ta,
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_subida_4 %in% c("L4", "L4-L5") &
                                                paraderosubida_4ta == "VICENTE VALDES"), 
                                        "VICENTE VALDES L4"), 
           paraderosubida_4ta = replace(paraderosubida_4ta,
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_subida_4 %in% c("L4", "L4-L4A") &
                                                paraderosubida_4ta == "VICUNA MACKENNA"), 
                                        "VICUNA MACKENNA L4"), 
           paraderosubida_4ta = replace(paraderosubida_4ta,
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_subida_4 %in% c("L5", "L5-L1") &
                                                paraderosubida_4ta == "BAQUEDANO"), 
                                        "BAQUEDANO L5"), 
           paraderosubida_4ta = replace(paraderosubida_4ta,
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_subida_4 %in% c("L5", "L5-L1") &
                                                paraderosubida_4ta == "SAN PABLO"), 
                                        "SAN PABLO L5"), 
           paraderosubida_4ta = replace(paraderosubida_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_subida_4 %in% c("L5", "L5-L3") &
                                                paraderosubida_4ta == "IRARRAZAVAL"), 
                                        "IRARRAZAVAL L5"), 
           paraderosubida_4ta = replace(paraderosubida_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_subida_4 %in% c("L5", "L5-L6") &
                                                paraderosubida_4ta %in% c("NUBLE", "NUBLE EU")), 
                                        "NUBLE L5"), 
           paraderosubida_4ta = replace(paraderosubida_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_subida_4 %in% c("L5", "L5-L3") &
                                                paraderosubida_4ta == "PLAZA DE ARMAS"), 
                                        "PLAZA DE ARMAS L5"),
           paraderosubida_4ta = replace(paraderosubida_4ta,
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_subida_4 %in% c("L6", "L6-L2") &
                                                paraderosubida_4ta == "FRANKLIN"), 
                                        "FRANKLIN L6"), 
           paraderosubida_4ta = replace(paraderosubida_4ta,
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_subida_4 %in% c("L6", "L6-MT") &
                                                paraderosubida_4ta == "LO VALLEDOR"), 
                                        "LO VALLEDOR L6"), 
           paraderosubida_4ta = replace(paraderosubida_4ta,
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_subida_4 %in% c("L6", "L6-L5") &
                                                paraderosubida_4ta %in% c("NUBLE", "NUBLE EU")), 
                                        "NUBLE L6"), 
           paraderosubida_4ta = replace(paraderosubida_4ta,
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_subida_4 %in% c("L6", "L6-L3") &
                                                paraderosubida_4ta == "NUNOA"), 
                                        "NUNOA L6"), 
           paraderosubida_4ta = replace(paraderosubida_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_subida_4 %in% c("L3", "L3-L2") &
                                                paraderosubida_4ta == "CAL Y CANTO"), 
                                        "CAL Y CANTO L3"),
           paraderosubida_4ta = replace(paraderosubida_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_subida_4 %in% c("L3", "L3-L5") &
                                                paraderosubida_4ta == "PLAZA DE ARMAS"), 
                                        "PLAZA DE ARMAS L3"), 
           paraderosubida_4ta = replace(paraderosubida_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_subida_4 %in% c("L3", "L3-L1") &
                                                paraderosubida_4ta == "UNIVERSIDAD DE CHILE"), 
                                        "UNIVERSIDAD DE CHILE L3"), 
           paraderosubida_4ta = replace(paraderosubida_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_subida_4 %in% c("L3", "L3-L5") & 
                                                paraderosubida_4ta == "IRARRAZAVAL"), 
                                        "IRARRAZAVAL L3"), 
           paraderosubida_4ta = replace(paraderosubida_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_subida_4 %in% c("L3", "L3-L6") & 
                                                paraderosubida_4ta == "NUNOA"), 
                                        "NUNOA L3"), 
           paraderosubida_4ta = replace(paraderosubida_4ta,
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_subida_4 %in% c("L3", "L3-L4") & 
                                                paraderosubida_4ta == "PLAZA EGANA"), 
                                        "PLAZA EGANA L3"),
           paraderobajada_1era = replace(paraderobajada_1era, 
                                         which(tipotransporte_1era == "METRO" &
                                                 linea_metro_bajada_1 %in% c("L1", "L1-L5") & 
                                                 paraderobajada_1era == "BAQUEDANO"), 
                                         "BAQUEDANO L1"), 
           paraderobajada_1era = replace(paraderobajada_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_bajada_1 %in% c("L1", "L1-L5") & 
                                                 paraderobajada_1era == "SAN PABLO"), 
                                         "SAN PABLO L1"),
           paraderobajada_1era = replace(paraderobajada_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_bajada_1 %in% c("L1", "L1-L2") & 
                                                 paraderobajada_1era == "LOS HEROES"), 
                                         "LOS HEROES L1"), 
           paraderobajada_1era = replace(paraderobajada_1era, 
                                         which(tipotransporte_1era == "METRO" &
                                                 linea_metro_bajada_1 %in% c("L1", "L1-L2") & 
                                                 paraderobajada_1era == "LOS LEONES"), 
                                         "LOS LEONES L1"),
           paraderobajada_1era = replace(paraderobajada_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_bajada_1 %in% c("L1", "L1-L4") & 
                                                 paraderobajada_1era == "TOBALABA"), 
                                         "TOBALABA L1"),
           paraderobajada_1era = replace(paraderobajada_1era, 
                                         which(tipotransporte_1era == "METRO" &
                                                 linea_metro_bajada_1 %in% c("L1", "L1-L3") & 
                                                 paraderobajada_1era == "UNIVERSIDAD DE CHILE"),
                                         "UNIVERSIDAD DE CHILE L1"), 
           paraderobajada_1era = replace(paraderobajada_1era,
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_bajada_1 %in% c("L2", "L2-L3") & 
                                                 paraderobajada_1era == "CAL Y CANTO"), 
                                         "CAL Y CANTO L2"), 
           paraderobajada_1era = replace(paraderobajada_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_bajada_1 %in% c("L2", "L2-L6") &
                                                 paraderobajada_1era == "FRANKLIN"), 
                                         "FRANKLIN L2"), 
           paraderobajada_1era = replace(paraderobajada_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_bajada_1 %in% c("L2", "L2-L4A") &
                                                 paraderobajada_1era == "LA CISTERNA"), 
                                         "LA CISTERNA L2"), 
           paraderobajada_1era = replace(paraderobajada_1era,
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_bajada_1 %in% c("L2", "L2-L1") &
                                                 paraderobajada_1era == "LOS HEROES"), 
                                         "LOS HEROES L2"), 
           paraderobajada_1era = replace(paraderobajada_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_bajada_1 %in% c("L2", "L2-L5") & 
                                                 paraderobajada_1era == "SANTA ANA"), 
                                         "SANTA ANA L2"), 
           paraderobajada_1era = replace(paraderobajada_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_bajada_1 %in% c("L4", "L4-L3") &
                                                 paraderobajada_1era == "PLAZA EGANA"), 
                                         "PLAZA EGANA L4"), 
           paraderobajada_1era = replace(paraderobajada_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_bajada_1 %in% c("L4", "L4-L1") &
                                                 paraderobajada_1era == "TOBALABA"), 
                                         "TOBALABA L4"), 
           paraderobajada_1era = replace(paraderobajada_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_bajada_1 %in% c("L4", "L4-L5") &
                                                 paraderobajada_1era == "VICENTE VALDES"), 
                                         "VICENTE VALDES L4"), 
           paraderobajada_1era = replace(paraderobajada_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_bajada_1 %in% c("L4", "L4-L4A") &
                                                 paraderobajada_1era == "VICUNA MACKENNA"), 
                                         "VICUNA MACKENNA L4"), 
           paraderobajada_1era = replace(paraderobajada_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_bajada_1 %in% c("L5", "L5-L1") &
                                                 paraderobajada_1era == "BAQUEDANO"), 
                                         "BAQUEDANO L5"), 
           paraderobajada_1era = replace(paraderobajada_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_bajada_1 %in% c("L5", "L5-L1") &
                                                 paraderobajada_1era == "SAN PABLO"), 
                                         "SAN PABLO L5"), 
           paraderobajada_1era = replace(paraderobajada_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_bajada_1 %in% c("L5", "L5-L3") &
                                                 paraderobajada_1era == "IRARRAZAVAL"), 
                                         "IRARRAZAVAL L5"), 
           paraderobajada_1era = replace(paraderobajada_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_bajada_1 %in% c("L5", "L5-L6") &
                                                 paraderobajada_1era %in% c("NUBLE", "NUBLE EU")), 
                                         "NUBLE L5"), 
           paraderobajada_1era = replace(paraderobajada_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_bajada_1 %in% c("L5", "L5-L3") &
                                                 paraderobajada_1era == "PLAZA DE ARMAS"), 
                                         "PLAZA DE ARMAS L5"),
           paraderobajada_1era = replace(paraderobajada_1era,
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_bajada_1 %in% c("L6", "L6-L2") &
                                                 paraderobajada_1era == "FRANKLIN"), 
                                         "FRANKLIN L6"), 
           paraderobajada_1era = replace(paraderobajada_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_bajada_1 %in% c("L6", "L6-MT") &
                                                 paraderobajada_1era == "LO VALLEDOR"), 
                                         "LO VALLEDOR L6"), 
           paraderobajada_1era = replace(paraderobajada_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_bajada_1 %in% c("L6", "L6-L5") &
                                                 paraderobajada_1era %in% c("NUBLE", "NUBLE EU")), 
                                         "NUBLE L6"), 
           paraderobajada_1era = replace(paraderobajada_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_bajada_1 %in% c("L6", "L6-L3") & 
                                                 paraderobajada_1era == "NUNOA"), 
                                         "NUNOA L6"), 
           paraderobajada_1era = replace(paraderobajada_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_bajada_1 %in% c("L3", "L3-L2") &
                                                 paraderobajada_1era == "CAL Y CANTO"), 
                                         "CAL Y CANTO L3"),
           paraderobajada_1era = replace(paraderobajada_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_bajada_1 %in% c("L3", "L3-L5") &
                                                 paraderobajada_1era == "PLAZA DE ARMAS"), 
                                         "PLAZA DE ARMAS L3"), 
           paraderobajada_1era = replace(paraderobajada_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_bajada_1 %in% c("L3", "L3-L1") &
                                                 paraderobajada_1era == "UNIVERSIDAD DE CHILE"), 
                                         "UNIVERSIDAD DE CHILE L3"), 
           paraderobajada_1era = replace(paraderobajada_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_bajada_1 %in% c("L3", "L3-L5") & 
                                                 paraderobajada_1era == "IRARRAZAVAL"), 
                                         "IRARRAZAVAL L3"), 
           paraderobajada_1era = replace(paraderobajada_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_bajada_1 %in% c("L3", "L3-L6") & 
                                                 paraderobajada_1era == "NUNOA"), 
                                         "NUNOA L3"), 
           paraderobajada_1era = replace(paraderobajada_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_bajada_1 %in% c("L3", "L3-L4") & 
                                                 paraderobajada_1era == "PLAZA EGANA"), 
                                         "PLAZA EGANA L3"),
           paraderobajada_2da = replace(paraderobajada_2da, 
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_bajada_2 %in% c("L1", "L1-L5") & 
                                                paraderobajada_2da == "BAQUEDANO"), 
                                        "BAQUEDANO L1"),
           paraderobajada_2da = replace(paraderobajada_2da, 
                                        which(tipotransporte_2da == "METRO" &
                                                linea_metro_bajada_2 %in% c("L1", "L1-L5") & 
                                                paraderobajada_2da == "SAN PABLO"), 
                                        "SAN PABLO L1"),
           paraderobajada_2da = replace(paraderobajada_2da, 
                                        which(tipotransporte_2da == "METRO" &
                                                linea_metro_bajada_2 %in% c("L1", "L1-L2") & 
                                                paraderobajada_2da == "LOS HEROES"), 
                                        "LOS HEROES L1"),
           paraderobajada_2da = replace(paraderobajada_2da, 
                                        which(tipotransporte_2da == "METRO" &
                                                linea_metro_bajada_2 %in% c("L1", "L1-L2") & 
                                                paraderobajada_2da == "LOS LEONES"), 
                                        "LOS LEONES L1"),
           paraderobajada_2da = replace(paraderobajada_2da, 
                                        which(tipotransporte_2da == "METRO" &
                                                linea_metro_bajada_2 %in% c("L1", "L1-L4") & 
                                                paraderobajada_2da == "TOBALABA"), 
                                        "TOBALABA L1"),
           paraderobajada_2da = replace(paraderobajada_2da, 
                                        which(tipotransporte_2da == "METRO" &
                                                linea_metro_bajada_2 %in% c("L1", "L1-L3") & 
                                                paraderobajada_2da == "UNIVERSIDAD DE CHILE"),
                                        "UNIVERSIDAD DE CHILE L1"),
           paraderobajada_2da = replace(paraderobajada_2da, 
                                        which(tipotransporte_2da == "METRO" &
                                                linea_metro_bajada_2 %in% c("L2", "L2-L3") &
                                                paraderobajada_2da == "CAL Y CANTO"), 
                                        "CAL Y CANTO L2"),
           paraderobajada_2da = replace(paraderobajada_2da, 
                                        which(tipotransporte_2da == "METRO" &
                                                linea_metro_bajada_2 %in% c("L2", "L2-L6") &
                                                paraderobajada_2da == "FRANKLIN"), 
                                        "FRANKLIN L2"), 
           paraderobajada_2da = replace(paraderobajada_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_bajada_2 %in% c("L2", "L2-L4A") &
                                                paraderobajada_2da == "LA CISTERNA"), 
                                        "LA CISTERNA L2"), 
           paraderobajada_2da = replace(paraderobajada_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_bajada_2 %in% c("L2", "L2-L1") &
                                                paraderobajada_2da == "LOS HEROES"), 
                                        "LOS HEROES L2"), 
           paraderobajada_2da = replace(paraderobajada_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_bajada_2 %in% c("L2", "L2-L5") &
                                                paraderobajada_2da == "SANTA ANA"), 
                                        "SANTA ANA L2"),
           paraderobajada_2da = replace(paraderobajada_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_bajada_2 %in% c("L4", "L4-L3") &
                                                paraderobajada_2da == "PLAZA EGANA"), 
                                        "PLAZA EGANA L4"), 
           paraderobajada_2da = replace(paraderobajada_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_bajada_2 %in% c("L4", "L4-L1") &
                                                paraderobajada_2da == "TOBALABA"), 
                                        "TOBALABA L4"), 
           paraderobajada_2da = replace(paraderobajada_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_bajada_2 %in% c("L4", "L4-L5") &
                                                paraderobajada_2da == "VICENTE VALDES"), 
                                        "VICENTE VALDES L4"), 
           paraderobajada_2da = replace(paraderobajada_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_bajada_2 %in% c("L4", "L4-L4A") &
                                                paraderobajada_2da == "VICUNA MACKENNA"), 
                                        "VICUNA MACKENNA L4"),
           paraderobajada_2da = replace(paraderobajada_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_bajada_2 %in% c("L5", "L5-L1") &
                                                paraderobajada_2da == "BAQUEDANO"), 
                                        "BAQUEDANO L5"), 
           paraderobajada_2da = replace(paraderobajada_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_bajada_2 %in% c("L5", "L5-L3") &
                                                paraderobajada_2da == "IRARRAZAVAL"), 
                                        "IRARRAZAVAL L5"), 
           paraderobajada_2da = replace(paraderobajada_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_bajada_2 %in% c("L5", "L5-L6") &
                                                paraderobajada_2da %in% c("NUBLE", "NUBLE EU")), 
                                        "NUBLE L5"), 
           paraderobajada_2da = replace(paraderobajada_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_bajada_2 %in% c("L5", "L5-L3") &
                                                paraderobajada_2da == "PLAZA DE ARMAS"), 
                                        "PLAZA DE ARMAS L5"), 
           paraderobajada_2da = replace(paraderobajada_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_bajada_2 %in% c("L5", "L5-L1") &
                                                paraderobajada_2da == "SAN PABLO"), 
                                        "SAN PABLO L5"),
           paraderobajada_2da = replace(paraderobajada_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_bajada_2 %in% c("L5", "L5-L2") &
                                                paraderobajada_2da == "SANTA ANA"), 
                                        "SANTA ANA L5"),
           paraderobajada_2da = replace(paraderobajada_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_bajada_2 %in% c("L6", "L6-L2") &
                                                paraderobajada_2da == "FRANKLIN"), 
                                        "FRANKLIN L6"), 
           paraderobajada_2da = replace(paraderobajada_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_bajada_2 %in% c("L6", "L6-MT") &
                                                paraderobajada_2da == "LO VALLEDOR"), 
                                        "LO VALLEDOR L6"), 
           paraderobajada_2da = replace(paraderobajada_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_bajada_2 %in% c("L6", "L6-L5") &
                                                paraderobajada_2da %in% c("NUBLE", "NUBLE EU")), 
                                        "NUBLE L6"), 
           paraderobajada_2da = replace(paraderobajada_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_bajada_2 %in% c("L6", "L6-L3") &
                                                paraderobajada_2da == "NUNOA"), 
                                        "NUNOA L6"),
           paraderobajada_2da = replace(paraderobajada_2da, 
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_bajada_2 %in% c("L3", "L3-L2") &
                                                paraderobajada_2da == "CAL Y CANTO"), 
                                        "CAL Y CANTO L3"),
           paraderobajada_2da = replace(paraderobajada_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_bajada_2 %in% c("L3", "L3-L5") &
                                                paraderobajada_2da == "PLAZA DE ARMAS"), 
                                        "PLAZA DE ARMAS L3"), 
           paraderobajada_2da = replace(paraderobajada_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_bajada_2 %in% c("L3", "L3-L1") &
                                                paraderobajada_2da == "UNIVERSIDAD DE CHILE"), 
                                        "UNIVERSIDAD DE CHILE L3"), 
           paraderobajada_2da = replace(paraderobajada_2da, 
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_bajada_2 %in% c("L3", "L3-L5") & 
                                                paraderobajada_2da == "IRARRAZAVAL"), 
                                        "IRARRAZAVAL L3"), 
           paraderobajada_2da = replace(paraderobajada_2da, 
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_bajada_2 %in% c("L3", "L3-L6") & 
                                                paraderobajada_2da == "NUNOA"), 
                                        "NUNOA L3"), 
           paraderobajada_2da = replace(paraderobajada_2da,
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_bajada_2 %in% c("L3", "L3-L4") &
                                                paraderobajada_2da == "PLAZA EGANA"), 
                                        "PLAZA EGANA L3"),
           paraderobajada_3era = replace(paraderobajada_3era, 
                                         which(tipotransporte_3era == "METRO" &
                                                 linea_metro_bajada_3 %in% c("L1", "L1-L5") & 
                                                 paraderobajada_3era == "BAQUEDANO"), 
                                         "BAQUEDANO L1"), 
           paraderobajada_3era = replace(paraderobajada_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_bajada_3 %in% c("L1", "L1-L5") & 
                                                 paraderobajada_3era == "SAN PABLO"), 
                                         "SAN PABLO L1"),
           paraderobajada_3era = replace(paraderobajada_3era, 
                                         which(tipotransporte_3era == "METRO" &
                                                 linea_metro_bajada_3 %in% c("L1", "L1-L2") & 
                                                 paraderobajada_3era == "LOS HEROES"), 
                                         "LOS HEROES L1"), 
           paraderobajada_3era = replace(paraderobajada_3era, 
                                         which(tipotransporte_3era == "METRO" &
                                                 linea_metro_bajada_3 %in% c("L1", "L1-L2") & 
                                                 paraderobajada_3era == "LOS LEONES"), 
                                         "LOS LEONES L1"),
           paraderobajada_3era = replace(paraderobajada_3era, 
                                         which(tipotransporte_3era == "METRO" &
                                                 linea_metro_bajada_3 %in% c("L1", "L1-L4") & 
                                                 paraderobajada_3era == "TOBALABA"), 
                                         "TOBALABA L1"),
           paraderobajada_3era = replace(paraderobajada_3era, 
                                         which(tipotransporte_3era == "METRO" &
                                                 linea_metro_bajada_3 %in% c("L1", "L1-L3") & 
                                                 paraderobajada_3era == "UNIVERSIDAD DE CHILE"), 
                                         "UNIVERSIDAD DE CHILE L1"), 
           paraderobajada_3era = replace(paraderobajada_3era,
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_bajada_3 %in% c("L2", "L2-L3") &
                                                 paraderobajada_3era == "CAL Y CANTO"), 
                                         "CAL Y CANTO L2"), 
           paraderobajada_3era = replace(paraderobajada_3era,
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_bajada_3 %in% c("L2", "L2-L6") &
                                                 paraderobajada_3era == "FRANKLIN"), 
                                         "FRANKLIN L2"), 
           paraderobajada_3era = replace(paraderobajada_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_bajada_3 %in% c("L2", "L2-L4A") &
                                                 paraderobajada_3era == "LA CISTERNA"), 
                                         "LA CISTERNA L2"), 
           paraderobajada_3era = replace(paraderobajada_3era,
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_bajada_3 %in% c("L2", "L2-L1") &
                                                 paraderobajada_3era == "LOS HEROES"), 
                                         "LOS HEROES L2"), 
           paraderobajada_3era = replace(paraderobajada_3era,
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_bajada_3 %in% c("L2", "L2-L5") &
                                                 paraderobajada_3era == "SANTA ANA"), 
                                         "SANTA ANA L2"), 
           paraderobajada_3era = replace(paraderobajada_3era,
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_bajada_3 %in% c("L4", "L4-L3") &
                                                 paraderobajada_3era == "PLAZA EGANA"), 
                                         "PLAZA EGANA L4"), 
           paraderobajada_3era = replace(paraderobajada_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_bajada_3 %in% c("L4", "L4-L1") & 
                                                 paraderobajada_3era == "TOBALABA"), 
                                         "TOBALABA L4"), 
           paraderobajada_3era = replace(paraderobajada_3era,
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_bajada_3 %in% c("L4", "L4-L5") &
                                                 paraderobajada_3era == "VICENTE VALDES"), 
                                         "VICENTE VALDES L4"), 
           paraderobajada_3era = replace(paraderobajada_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_bajada_3 %in% c("L4", "L4-L4A") &
                                                 paraderobajada_3era == "VICUNA MACKENNA"), 
                                         "VICUNA MACKENNA L4"), 
           paraderobajada_3era = replace(paraderobajada_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_bajada_3 %in% c("L5", "L5-L1") &
                                                 paraderobajada_3era == "BAQUEDANO"), 
                                         "BAQUEDANO L5"),
           paraderobajada_3era = replace(paraderobajada_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_bajada_3 %in% c("L5", "L5-L1") &
                                                 paraderobajada_3era == "SAN PABLO"), 
                                         "SAN PABLO L5"),
           paraderobajada_3era = replace(paraderobajada_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_bajada_3 %in% c("L5", "L5-L3") &
                                                 paraderobajada_3era == "IRARRAZAVAL"), 
                                         "IRARRAZAVAL L5"), 
           paraderobajada_3era = replace(paraderobajada_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_bajada_3 %in% c("L5", "L5-L6") &
                                                 paraderobajada_3era %in% c("NUBLE", "NUBLE EU")), 
                                         "NUBLE L5"), 
           paraderobajada_3era = replace(paraderobajada_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_bajada_3 %in% c("L5", "L5-L3") &
                                                 paraderobajada_3era == "PLAZA DE ARMAS"), 
                                         "PLAZA DE ARMAS L5"),
           paraderobajada_3era = replace(paraderobajada_3era,
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_bajada_3 %in% c("L6", "L6-L2") &
                                                 paraderobajada_3era == "FRANKLIN"), 
                                         "FRANKLIN L6"), 
           paraderobajada_3era = replace(paraderobajada_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_bajada_3 %in% c("L6", "L6-MT") &
                                                 paraderobajada_3era == "LO VALLEDOR"), 
                                         "LO VALLEDOR L6"), 
           paraderobajada_3era = replace(paraderobajada_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_bajada_3 %in% c("L6", "L6-L5") & 
                                                 paraderobajada_3era %in% c("NUBLE", "NUBLE EU")), 
                                         "NUBLE L6"), 
           paraderobajada_3era = replace(paraderobajada_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_bajada_3 %in% c("L6", "L6-L3") & 
                                                 paraderobajada_3era == "NUNOA"), 
                                         "NUNOA L6"), 
           paraderobajada_3era = replace(paraderobajada_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_bajada_3 %in% c("L3", "L3-L2") & 
                                                 paraderobajada_3era == "CAL Y CANTO"), 
                                         "CAL Y CANTO L3"),
           paraderobajada_3era = replace(paraderobajada_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_bajada_3 %in% c("L3", "L3-L5") & 
                                                 paraderobajada_3era == "PLAZA DE ARMAS"), 
                                         "PLAZA DE ARMAS L3"), 
           paraderobajada_3era = replace(paraderobajada_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_bajada_3 %in% c("L3", "L3-L1") & 
                                                 paraderobajada_3era == "UNIVERSIDAD DE CHILE"), 
                                         "UNIVERSIDAD DE CHILE L3"), 
           paraderobajada_3era = replace(paraderobajada_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_bajada_3 %in% c("L3", "L3-L5") & 
                                                 paraderobajada_3era == "IRARRAZAVAL"), 
                                         "IRARRAZAVAL L3"), 
           paraderobajada_3era = replace(paraderobajada_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_bajada_3 %in% c("L3", "L3-L6") & 
                                                 paraderobajada_3era == "NUNOA"), 
                                         "NUNOA L3"), 
           paraderobajada_3era = replace(paraderobajada_3era,
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_bajada_3 %in% c("L3", "L3-L4") & 
                                                 paraderobajada_3era == "PLAZA EGANA"), 
                                         "PLAZA EGANA L3"), 
           paraderobajada_4ta = replace(paraderobajada_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_bajada_4 %in% c("L1", "L1-L5") & 
                                                paraderobajada_4ta == "BAQUEDANO"), 
                                        "BAQUEDANO L1"),
           paraderobajada_4ta = replace(paraderobajada_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_bajada_4 %in% c("L1", "L1-L5") & 
                                                paraderobajada_4ta == "SAN PABLO"), 
                                        "SAN PABLO L1"), 
           paraderobajada_4ta = replace(paraderobajada_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_bajada_4 %in% c("L1", "L1-L2") & 
                                                paraderobajada_4ta == "LOS HEROES"), 
                                        "LOS HEROES L1"),
           paraderobajada_4ta = replace(paraderobajada_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_bajada_4 %in% c("L1", "L1-L2") & 
                                                paraderobajada_4ta == "LOS LEONES"), 
                                        "LOS LEONES L1"),
           paraderobajada_4ta = replace(paraderobajada_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_bajada_4 %in% c("L1", "L1-L4") & 
                                                paraderobajada_4ta == "TOBALABA"), 
                                        "TOBALABA L1"),
           paraderobajada_4ta = replace(paraderobajada_4ta, 
                                        which(tipotransporte_4ta == "METRO" &
                                                linea_metro_bajada_4 %in% c("L1", "L1-L3") & 
                                                paraderobajada_4ta == "UNIVERSIDAD DE CHILE"), 
                                        "UNIVERSIDAD DE CHILE L1"), 
           paraderobajada_4ta = replace(paraderobajada_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_bajada_4 %in% c("L2", "L2-L3") & 
                                                paraderobajada_4ta == "CAL Y CANTO"), 
                                        "CAL Y CANTO L2"), 
           paraderobajada_4ta = replace(paraderobajada_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_bajada_4 %in% c("L2", "L2-L6") & 
                                                paraderobajada_4ta == "FRANKLIN"), 
                                        "FRANKLIN L2"), 
           paraderobajada_4ta = replace(paraderobajada_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_bajada_4 %in% c("L2", "L2-L4A") & 
                                                paraderobajada_4ta == "LA CISTERNA"), 
                                        "LA CISTERNA L2"), 
           paraderobajada_4ta = replace(paraderobajada_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_bajada_4 %in% c("L2", "L2-L1") &
                                                paraderobajada_4ta == "LOS HEROES"), 
                                        "LOS HEROES L2"), 
           paraderobajada_4ta = replace(paraderobajada_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_bajada_4 %in% c("L2", "L2-L5") & 
                                                paraderobajada_4ta == "SANTA ANA"), 
                                        "SANTA ANA L2"), 
           paraderobajada_4ta = replace(paraderobajada_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_bajada_4 %in% c("L4", "L4-L3") & 
                                                paraderobajada_4ta == "PLAZA EGANA"), 
                                        "PLAZA EGANA L4"), 
           paraderobajada_4ta = replace(paraderobajada_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_bajada_4 %in% c("L4", "L4-L1") & 
                                                paraderobajada_4ta == "TOBALABA"), 
                                        "TOBALABA L4"), 
           paraderobajada_4ta = replace(paraderobajada_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_bajada_4 %in% c("L4", "L4-L5") & 
                                                paraderobajada_4ta == "VICENTE VALDES"), 
                                        "VICENTE VALDES L4"), 
           paraderobajada_4ta = replace(paraderobajada_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_bajada_4 %in% c("L4", "L4-L4A") &
                                                paraderobajada_4ta == "VICUNA MACKENNA"), 
                                        "VICUNA MACKENNA L4"), 
           paraderobajada_4ta = replace(paraderobajada_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_bajada_4 %in% c("L5", "L5-L1") & 
                                                paraderobajada_4ta == "BAQUEDANO"), 
                                        "BAQUEDANO L5"),
           paraderobajada_4ta = replace(paraderobajada_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_bajada_4 %in% c("L5", "L5-L1") & 
                                                paraderobajada_4ta == "SAN PABLO"), 
                                        "SAN PABLO L5"),
           paraderobajada_4ta = replace(paraderobajada_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_bajada_4 %in% c("L5", "L5-L3") & 
                                                paraderobajada_4ta == "IRARRAZAVAL"), 
                                        "IRARRAZAVAL L5"), 
           paraderobajada_4ta = replace(paraderobajada_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_bajada_4 %in% c("L5", "L5-L6") &
                                                paraderobajada_4ta %in% c("NUBLE", "NUBLE EU")), 
                                        "NUBLE L5"), 
           paraderobajada_4ta = replace(paraderobajada_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_bajada_4 %in% c("L5", "L5-L3") & 
                                                paraderobajada_4ta == "PLAZA DE ARMAS"), 
                                        "PLAZA DE ARMAS L5"),
           paraderobajada_4ta = replace(paraderobajada_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_bajada_4 %in% c("L6", "L6-L2") & 
                                                paraderobajada_4ta == "FRANKLIN"), 
                                        "FRANKLIN L6"), 
           paraderobajada_4ta = replace(paraderobajada_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_bajada_4 %in% c("L6", "L6-MT") & 
                                                paraderobajada_4ta == "LO VALLEDOR"), 
                                        "LO VALLEDOR L6"), 
           paraderobajada_4ta = replace(paraderobajada_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_bajada_4 %in% c("L6", "L6-L5") &
                                                paraderobajada_4ta %in% c("NUBLE", "NUBLE EU")), 
                                        "NUBLE L6"), 
           paraderobajada_4ta = replace(paraderobajada_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_bajada_4 %in% c("L6", "L6-L3") &
                                                paraderobajada_4ta == "NUNOA"), 
                                        "NUNOA L6"), 
           paraderobajada_4ta = replace(paraderobajada_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_bajada_4 %in% c("L3", "L3-L2") & 
                                                paraderobajada_4ta == "CAL Y CANTO"), 
                                        "CAL Y CANTO L3"),
           paraderobajada_4ta = replace(paraderobajada_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_bajada_4 %in% c("L3", "L3-L5") & 
                                                paraderobajada_4ta == "PLAZA DE ARMAS"), 
                                        "PLAZA DE ARMAS L3"), 
           paraderobajada_4ta = replace(paraderobajada_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_bajada_4 %in% c("L3", "L3-L1") & 
                                                paraderobajada_4ta == "UNIVERSIDAD DE CHILE"), 
                                        "UNIVERSIDAD DE CHILE L3"), 
           paraderobajada_4ta = replace(paraderobajada_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_bajada_4 %in% c("L3", "L3-L5") & 
                                                paraderobajada_4ta == "IRARRAZAVAL"), 
                                        "IRARRAZAVAL L3"), 
           paraderobajada_4ta = replace(paraderobajada_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_bajada_4 %in% c("L3", "L3-L6") & 
                                                paraderobajada_4ta == "NUNOA"), 
                                        "NUNOA L3"), 
           paraderobajada_4ta = replace(paraderobajada_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_bajada_4 %in% c("L3", "L3-L4") & 
                                                paraderobajada_4ta == "PLAZA EGANA"), 
                                        "PLAZA EGANA L3"),
           paraderosubida_1era = replace(paraderosubida_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_subida_1 %in% c("L4A", "L4A-L4") & 
                                                 paraderosubida_1era == "LA CISTERNA"), 
                                         "LA CISTERNA L4A"), 
           paraderosubida_1era = replace(paraderosubida_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_subida_1 %in% c("L4A", "L4A-L4") & 
                                                 paraderosubida_1era == "VICUNA MACKENNA"), 
                                         "VICUNA MACKENNA L4A"),
           paraderosubida_2da = replace(paraderosubida_2da, 
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_subida_2 %in% c("L4A", "L4A-L4") & 
                                                paraderosubida_2da == "LA CISTERNA"), 
                                        "LA CISTERNA L4A"), 
           paraderosubida_2da = replace(paraderosubida_2da, 
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_subida_2 %in% c("L4A", "L4A-L4") & 
                                                paraderosubida_2da == "VICUNA MACKENNA"), 
                                        "VICUNA MACKENNA L4A"),
           paraderosubida_3era = replace(paraderosubida_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_subida_3 %in% c("L4A", "L4A-L4") & 
                                                 paraderosubida_3era == "LA CISTERNA"), 
                                         "LA CISTERNA L4A"), 
           paraderosubida_3era = replace(paraderosubida_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_subida_3 %in% c("L4A", "L4A-L4") & 
                                                 paraderosubida_3era == "VICUNA MACKENNA"), 
                                         "VICUNA MACKENNA L4A"),
           paraderosubida_4ta = replace(paraderosubida_4ta, 
                                        which(tipotransporte_4ta == "METRO" &
                                                linea_metro_subida_4 %in% c("L4A", "L4A-L4") &  
                                                paraderosubida_4ta == "LA CISTERNA"), 
                                        "LA CISTERNA L4A"), 
           paraderosubida_4ta = replace(paraderosubida_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_subida_4 %in% c("L4A", "L4A-L4") & 
                                                paraderosubida_4ta == "VICUNA MACKENNA"), 
                                        "VICUNA MACKENNA L4A"),
           paraderobajada_1era = replace(paraderobajada_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_bajada_1 %in% c("L4A", "L4A-L4") & 
                                                 paraderobajada_1era == "LA CISTERNA"), 
                                         "LA CISTERNA L4A"), 
           paraderobajada_1era = replace(paraderobajada_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_bajada_1 %in% c("L4A", "L4A-L4") & 
                                                 paraderobajada_1era == "VICUNA MACKENNA"), 
                                         "VICUNA MACKENNA L4A"),
           paraderobajada_2da = replace(paraderobajada_2da, 
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_bajada_2 %in% c("L4A", "L4A-L4") & 
                                                paraderobajada_2da == "LA CISTERNA"),
                                        "LA CISTERNA L4A"), 
           paraderobajada_2da = replace(paraderobajada_2da, 
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_bajada_2 %in% c("L4A", "L4A-L4") & 
                                                paraderobajada_2da == "VICUNA MACKENNA"), 
                                        "VICUNA MACKENNA L4A"),
           paraderobajada_3era = replace(paraderobajada_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_bajada_3 %in% c("L4A", "L4A-L4") &
                                                 paraderobajada_3era == "LA CISTERNA"), 
                                         "LA CISTERNA L4A"), 
           paraderobajada_3era = replace(paraderobajada_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_subida_3 %in% c("L4A", "L4A-L4") &  
                                                 paraderobajada_3era == "VICUNA MACKENNA"), 
                                         "VICUNA MACKENNA L4A"),
           paraderobajada_4ta = replace(paraderobajada_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_bajada_4 %in% c("L4A", "L4A-L4") & 
                                                paraderobajada_4ta == "LA CISTERNA"), 
                                        "LA CISTERNA L4A"), 
           paraderobajada_4ta = replace(paraderobajada_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_bajada_4 %in% c("L4A", "L4A-L4") & 
                                                paraderobajada_4ta == "VICUNA MACKENNA"), 
                                        "VICUNA MACKENNA L4A"),
           paraderosubida_1era = replace(paraderosubida_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_subida_1 %in% c("L5", "L5-L4") & 
                                                 paraderosubida_1era == "VICENTE VALDES"), 
                                         "VICENTE VALDES L5"),
           paraderosubida_2da = replace(paraderosubida_2da, 
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_subida_2 %in% c("L5", "L5-L4") & 
                                                paraderosubida_2da == "VICENTE VALDES"), 
                                        "VICENTE VALDES L5"),
           paraderosubida_3era = replace(paraderosubida_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_subida_3 %in% c("L5", "L5-L4") & 
                                                 paraderosubida_3era == "VICENTE VALDES"), 
                                         "VICENTE VALDES L5"),
           paraderosubida_4ta = replace(paraderosubida_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_subida_4 %in% c("L5", "L5-L4") & 
                                                paraderosubida_4ta == "VICENTE VALDES"), 
                                        "VICENTE VALDES L5"),
           paraderobajada_1era = replace(paraderobajada_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_bajada_1 %in% c("L5", "L5-L4") & 
                                                 paraderobajada_1era == "VICENTE VALDES"), 
                                         "VICENTE VALDES L5"),
           paraderobajada_2da = replace(paraderobajada_2da, 
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_bajada_2 %in% c("L5", "L5-L4") & 
                                                paraderobajada_2da == "VICENTE VALDES"), 
                                        "VICENTE VALDES L5"),
           paraderobajada_3era = replace(paraderobajada_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_bajada_3 %in% c("L5", "L5-L4") & 
                                                 paraderobajada_3era == "VICENTE VALDES"), 
                                         "VICENTE VALDES L5"),
           paraderobajada_4ta = replace(paraderobajada_4ta, 
                                        which(tipotransporte_4ta == "METRO" & 
                                                linea_metro_bajada_4 %in% c("L5", "L5-L4") & 
                                                paraderobajada_4ta == "VICENTE VALDES"), 
                                        "VICENTE VALDES L5"),
           paraderobajada_1era = replace(paraderobajada_1era, 
                                         which(tipotransporte_1era == "METRO" &
                                                 linea_metro_bajada_1 %in% c("L1", "L1-L5") & 
                                                 paraderobajada_1era == "SAN PABLO"), 
                                         "SAN PABLO L1"),
           paraderobajada_1era = replace(paraderobajada_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_bajada_1 %in% c("L4A", "L4A-L2") & 
                                                 paraderobajada_1era == "LA CISTERNA"), 
                                         "LA CISTERNA L4A"),
           paraderobajada_1era = replace(paraderobajada_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_bajada_1 %in% c("L5", "L5-L1") & 
                                                 paraderobajada_1era == "SAN PABLO"), 
                                         "SAN PABLO L5"),
           paraderobajada_1era = replace(paraderobajada_1era, 
                                         which(tipotransporte_1era == "METRO" & 
                                                 linea_metro_bajada_1 %in% c("L5", "L5-L2") & 
                                                 paraderobajada_1era == "SANTA ANA"), 
                                         "SANTA ANA L5"),
           paraderosubida_2da = replace(paraderosubida_2da, 
                                        which(tipotransporte_2da == "METRO" & 
                                                linea_metro_subida_2 %in% c("L5", "L5-L3") & 
                                                paraderosubida_2da == "PLAZA DE ARMAS"), 
                                        "PLAZA DE ARMAS L5"),
           paraderobajada_3era = replace(paraderobajada_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_bajada_3 %in% c("L1", "L1-L5") & 
                                                 paraderobajada_3era == "SAN PABLO"), 
                                         "SAN PABLO L1"),
           paraderobajada_3era = replace(paraderobajada_3era,
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_bajada_3 %in% c("L4A", "L4A-L4") & 
                                                 paraderobajada_3era == "VICUNA MACKENNA"), 
                                         "VICUNA MACKENNA L4A"),
           paraderobajada_3era = replace(paraderobajada_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_bajada_3 %in% c("L5", "L5-L1") & 
                                                 paraderobajada_3era == "SAN PABLO"), "SAN PABLO L5"),
           paraderobajada_3era = replace(paraderobajada_3era, 
                                         which(tipotransporte_3era == "METRO" & 
                                                 linea_metro_bajada_3 %in% c("L5", "L5-L2") & 
                                                 paraderobajada_3era == "SANTA ANA"), 
                                         "SANTA ANA L5"),
           paraderosubida = paraderosubida_1era,
           netapa = replace(netapa, 
                            which(netapa > 4), 
                            4)
           ) 
    
    
}
