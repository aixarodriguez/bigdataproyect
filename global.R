library(shiny)
library(dplyr)
library(DT)
library(formattable)
library(highcharter)
library(reshape2)

# ld_HoraUpdate <- Sys.time()
# save(dt_plns_rsm,
#      dtYears,
#      dtCarriers, 
#      file="C:/Users/arodriguezs/Desktop/UVG/BigData/Proyecto/Dashboard/PlnRsm.RData")


#load('C:/Users/arodriguezs/Desktop/UVG/BigData/Proyecto/Dashboard/PlnRsm.RData')
load('PlnRsm.RData')

# dtCarriers<-dt_plns_rsm %>%
#   filter(tbl_plns_rsmn.year!=2001) %>%
#   group_by(tbl_plns_rsmn.code,tbl_plns_rsmn.description) %>%
#   summarise(years=n_distinct(tbl_plns_rsmn.year)) %>%
#   filter(years>10)
# 
# 
# dtYears<-dcast(unique(dt_plns_rsm %>%
#                         filter(tbl_plns_rsmn.code %in% dtCarriers$tbl_plns_rsmn.code & tbl_plns_rsmn.year!='2001') %>%
#                         select(tbl_plns_rsmn.code,tbl_plns_rsmn.description,tbl_plns_rsmn.year) %>%
#                         mutate(val=1)),
#                tbl_plns_rsmn.year~tbl_plns_rsmn.description+tbl_plns_rsmn.code,value.var='val',fill=0)
