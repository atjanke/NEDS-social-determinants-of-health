#### Libraries/Functions/Definitions   ####
library(data.table)
library(tidyverse)
library(tidytable)
library(tictoc)
library(splitstackshape)

neds.colnames = c("age","amonth","aweekend","died_visit","discwt","disp_ed","dqtr","edevent","female",
  "hcupfile","hosp_ed","i10_dx1","i10_dx2","i10_dx3","i10_dx4","i10_dx5","i10_dx6","i10_dx7",
  "i10_dx8","i10_dx9","i10_dx10","i10_dx11","i10_dx12","i10_dx13","i10_dx14","i10_dx15",
  "i10_dx16","i10_dx17","i10_dx18","i10_dx19","i10_dx20","i10_dx21","i10_dx22","i10_dx23",
  "i10_dx24","i10_dx25","i10_dx26","i10_dx27","i10_dx28","i10_dx29","i10_dx30","i10_dx31",
  "i10_dx32","i10_dx33","i10_dx34","i10_dx35","i10_injury","i10_injury_cut","i10_injury_drown",
  "i10_injury_fall","i10_injury_fire","i10_injury_firearm","i10_injury_machinery","i10_injury_mvt",
  "i10_injury_nature","i10_injury_overexertion","i10_injury_poison","i10_injury_struck",
  "i10_injury_suffocation","i10_intent_assault","i10_intent_self_harm","i10_intent_unintentional",
  "i10_multinjury","i10_ndx","key_ed","neds_stratum","pay1","pay2","pl_nchs","race","totchg_ed",
  "year","zipinc_qrtl")

#### Load data                         ####
neds19 <- fread("../../../../Data/neds/2019_NEDS/NEDS_2019_Core.csv")

colnames(neds19) <- neds.colnames


#### Identify SDH visits               ####
SDH.codes <- list("Z55","Z56","Z57","Z58","Z59","Z60",
                  "Z61","Z62","Z63","Z64","Z65")

grab.sdh.list <- 
  rbind(
    fread("grep -E 'Z5' ../../../../Data/neds/2019_NEDS/NEDS_2019_Core.csv"), 
    fread("grep -E 'Z6' ../../../../Data/neds/2019_NEDS/NEDS_2019_Core.csv"))%>%
  setNames(neds.colnames)

sdh.indicators <- grab.sdh.list %>%
  select.(key_ed,i10_dx1:i10_dx35) %>%
  pivot_longer.(cols=i10_dx1:i10_dx35) %>%
  select.(key_ed,value) %>%
  mutate.(value = substr(value,1,3)) %>%
  filter.(value %in% SDH.codes) %>%
  unique() %>%
  pivot_wider.(id_cols=key_ed,names_from=value) %>%
  mutate(across(-key_ed,~ifelse(is.na(.),0,1))) %>%
  unique()
rm(grab.sdh.list)

neds19 <- neds19 %>%
  left_join.(sdh.indicators,by="key_ed")
  
neds19 <- neds19 %>%
  select.(-i10_dx2:i10_ndx)

neds19[,22:30][is.na(neds19[,22:30])] <- 0

#### Save data+5% stratified sample    ####
write.csv(neds19,"data-cleaned/Prepared-Data.csv",row.names=FALSE)
neds19.stratified <- stratified(neds19,c("hosp_ed"),0.14)
saveRDS(neds19.stratified,"data-cleaned/Prepared-Data-subsample.rds")
