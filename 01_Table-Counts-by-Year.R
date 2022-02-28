#### Libraries and Functions               ####
library(data.table)
library(tidyverse)
library(tidytable)
library(splitstackshape)

#### Table 1: Counts by Year               ####

output = data.frame()
for (i in 2017:2019) {
      setwd("../../../../Data/neds")
      year_ = i

      data_ = paste0(year_,"_NEDS/NEDS_",year_,"_CORE.csv")
      core <- fread(data_
                    #,nrow=1000000
                    )
      
      k <- nrow(core)
      
      if (year_==2019) {
        colnames(core) <- c("age","amonth","aweekend","died_visit","discwt","disp_ed","dqtr","edevent","female",
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
      }
      
      if (year_==2018) {
        colnames(core) <- c("age","amonth","aweekend","died_visit","discwt","DISP_ED","dqtr",
                            "edevent","FEMALE","hcupfile","HOSP_ED","i10_dx1","i10_dx2","i10_dx3","i10_dx4","i10_dx5","i10_dx6","i10_dx7",
                            "i10_dx8","i10_dx9","i10_dx10","i10_dx11","i10_dx12","i10_dx13","i10_dx14","i10_dx15","i10_dx16","i10_dx17",
                            "i10_dx18","i10_dx19","i10_dx20","i10_dx21","i10_dx22","i10_dx23","i10_dx24",
                            "i10_dx25","i10_dx26","i10_dx27","i10_dx28","i10_dx29","i10_dx30",
                            "i10_dx31","i10_dx32","i10_dx33","i10_dx34","i10_dx35",
                            "i10_injury","i10_injury_cut","I10_injury_drown","i10_injury_fall","i10_injury_fire",
                            "i10_injury_firearm","i10_injury_machinery","i10_injury_mvt","i10_injury_nature",
                            "i10_injury_overexertion","i10_injury_poison","i10_injury_struck","i10_injury_suffocation",
                            "i10_intent_assault","i10_intent_self_harm",
                            "i10_intent_unintentional","i10_multinjury","i10_ndx","key_ed","neds_stratum",
                            "pay1","pay2","pl_nchs","TOTCHG_ED","year","zipinc_qrtl")
      }
      
      if (year_==2017) {
        colnames(core) <- c("age","amonth","aweekend","died_visit","discwt","DISP_ED","dqtr","dxver",
                            "edevent","FEMALE","hcupfile","HOSP_ED","i10_dx1","i10_dx2","i10_dx3","i10_dx4","i10_dx5","i10_dx6","i10_dx7",
                            "i10_dx8","i10_dx9","i10_dx10","i10_dx11","i10_dx12","i10_dx13","i10_dx14","i10_dx15","i10_dx16","i10_dx17",
                            "i10_dx18","i10_dx19","i10_dx20","i10_dx21","i10_dx22","i10_dx23","i10_dx24",
                            "i10_dx25","i10_dx26","i10_dx27","i10_dx28","i10_dx29","i10_dx30",
                            "i10_dx31","i10_dx32","i10_dx33","i10_dx34","i10_dx35",
                            "i10_injury","i10_multinjury","i10_ndx","key_ed","neds_stratum",
                            "pay1","pay2","pl_nchs","TOTCHG_ED","year","zipinc_qrtl")
      }
      
      SDH.codes <- list("Z55","Z56","Z57","Z58","Z59","Z60",
                        "Z61","Z62","Z63","Z64","Z65")
      
      SDH.core <- core %>%
        filter.(substr(i10_dx1,1,3) %in% SDH.codes|substr(i10_dx2,1,3) %in% SDH.codes|substr(i10_dx3,1,3) %in% SDH.codes|substr(i10_dx4,1,3) %in% SDH.codes|substr(i10_dx5,1,3) %in% SDH.codes|substr(i10_dx6,1,3) %in% SDH.codes|substr(i10_dx7,1,3) %in% SDH.codes|substr(i10_dx8,1,3) %in% SDH.codes|substr(i10_dx9,1,3) %in% SDH.codes|substr(i10_dx10,1,3) %in% SDH.codes|substr(i10_dx11,1,3) %in% SDH.codes|substr(i10_dx12,1,3) %in% SDH.codes|substr(i10_dx13,1,3) %in% SDH.codes|substr(i10_dx14,1,3) %in% SDH.codes|substr(i10_dx15,1,3) %in% SDH.codes|substr(i10_dx16,1,3) %in% SDH.codes|substr(i10_dx17,1,3) %in% SDH.codes|substr(i10_dx18,1,3) %in% SDH.codes|substr(i10_dx19,1,3) %in% SDH.codes|substr(i10_dx20,1,3) %in% SDH.codes|substr(i10_dx21,1,3) %in% SDH.codes|substr(i10_dx22,1,3) %in% SDH.codes|substr(i10_dx23,1,3) %in% SDH.codes|substr(i10_dx24,1,3) %in% SDH.codes|substr(i10_dx25,1,3) %in% SDH.codes|substr(i10_dx26,1,3) %in% SDH.codes|substr(i10_dx27,1,3) %in% SDH.codes|substr(i10_dx28,1,3) %in% SDH.codes|substr(i10_dx29,1,3) %in% SDH.codes|substr(i10_dx30,1,3) %in% SDH.codes|substr(i10_dx31,1,3) %in% SDH.codes|substr(i10_dx32,1,3) %in% SDH.codes|substr(i10_dx33,1,3) %in% SDH.codes|substr(i10_dx34,1,3) %in% SDH.codes|substr(i10_dx35,1,3) %in% SDH.codes)
      
      SDH.core <- SDH.core %>% select(key_ed,neds_stratum,discwt,age,pay1,zipinc_qrtl,i10_dx1:i10_dx35) %>%
        mutate(Row=row_number())
      
      SDH.indicators <- SDH.core %>%
        mutate(Row = row_number()) %>%
        select.(Row,i10_dx1:i10_dx35) %>%
        pivot_longer.(cols=i10_dx1:i10_dx35) %>%
        filter.(substr(value,1,3) %in% SDH.codes) %>%
        mutate(value=substr(value,1,3)) %>%
        unique() %>%
        mutate(
          Z55 = ifelse(value=="Z55",1,0),Z56 = ifelse(value=="Z56",1,0),Z57 = ifelse(value=="Z57",1,0),
          Z58 = ifelse(value=="Z58",1,0),Z59 = ifelse(value=="Z59",1,0),Z60 = ifelse(value=="Z60",1,0),
          Z61 = ifelse(value=="Z61",1,0),Z62 = ifelse(value=="Z62",1,0),Z63 = ifelse(value=="Z63",1,0),
          Z64 = ifelse(value=="Z64",1,0),Z65 = ifelse(value=="Z65",1,0)) %>%
        group_by(Row) %>% summarise(across(Z55:Z65,~ifelse(sum(.x)>0,1,0)))
      
      SDH.core <- cbind(SDH.core,SDH.indicators) %>% select(-Row)
      
      library(survey)
      library(scales)
      cluster <- svydesign(
        id=~key_ed,
        strata=~neds_stratum,
        weights=~discwt,
        nest=TRUE,
        data=SDH.core,
        multicore=T)
      
      
      table <- data.frame()
      
      table[1, 1] <- "Problems related to education and literacy"
      table[2, 1] <- "Problems related to employment and unemployment"
      table[3, 1] <- "Occupational exposure to risk factors"
      table[4, 1] <- "Problems related to physical environment"
      table[5, 1] <- "Problems related to housing and economic circumstances"
      table[6, 1] <- "Problems related to social environment"
      table[7, 1] <- "Negative life event in childhood"
      table[8, 1] <- "Problems related to upbringing"
      table[9, 1] <- "Other problems related to primary support group, including family circumstances"
      table[10,1] <- "Problems related to certain psychosocial circumstances"
      table[11,1] <- "Problems related to other psychosocial circumstances"
      
      table[1, 2] <- nrow(SDH.core[SDH.core$Z55==1])
      table[2, 2] <- nrow(SDH.core[SDH.core$Z56==1])
      table[3, 2] <- nrow(SDH.core[SDH.core$Z57==1])
      table[4, 2] <- nrow(SDH.core[SDH.core$Z58==1])
      table[5, 2] <- nrow(SDH.core[SDH.core$Z59==1])
      table[6, 2] <- nrow(SDH.core[SDH.core$Z60==1])
      table[7, 2] <- nrow(SDH.core[SDH.core$Z61==1])
      table[8, 2] <- nrow(SDH.core[SDH.core$Z62==1])
      table[9, 2] <- nrow(SDH.core[SDH.core$Z63==1])
      table[10,2] <- nrow(SDH.core[SDH.core$Z64==1])
      table[11,2] <- nrow(SDH.core[SDH.core$Z65==1])
      
      table[1, 3] <- paste0(round(svytotal(~Z55,cluster,na.rm=TRUE,multicore=TRUE),0)," (",round(SE(svytotal(~Z55,cluster,na.rm=TRUE,multicore=TRUE)),0),")")
      table[2, 3] <- paste0(round(svytotal(~Z56,cluster,na.rm=TRUE,multicore=TRUE),0)," (",round(SE(svytotal(~Z56,cluster,na.rm=TRUE,multicore=TRUE)),0),")")
      table[3, 3] <- paste0(round(svytotal(~Z57,cluster,na.rm=TRUE,multicore=TRUE),0)," (",round(SE(svytotal(~Z57,cluster,na.rm=TRUE,multicore=TRUE)),0),")")
      table[4, 3] <- paste0(round(svytotal(~Z58,cluster,na.rm=TRUE,multicore=TRUE),0)," (",round(SE(svytotal(~Z58,cluster,na.rm=TRUE,multicore=TRUE)),0),")")
      table[5, 3] <- paste0(round(svytotal(~Z59,cluster,na.rm=TRUE,multicore=TRUE),0)," (",round(SE(svytotal(~Z59,cluster,na.rm=TRUE,multicore=TRUE)),0),")")
      table[6, 3] <- paste0(round(svytotal(~Z60,cluster,na.rm=TRUE,multicore=TRUE),0)," (",round(SE(svytotal(~Z60,cluster,na.rm=TRUE,multicore=TRUE)),0),")")
      table[7, 3] <- paste0(round(svytotal(~Z61,cluster,na.rm=TRUE,multicore=TRUE),0)," (",round(SE(svytotal(~Z61,cluster,na.rm=TRUE,multicore=TRUE)),0),")")
      table[8, 3] <- paste0(round(svytotal(~Z62,cluster,na.rm=TRUE,multicore=TRUE),0)," (",round(SE(svytotal(~Z62,cluster,na.rm=TRUE,multicore=TRUE)),0),")")
      table[9, 3] <- paste0(round(svytotal(~Z63,cluster,na.rm=TRUE,multicore=TRUE),0)," (",round(SE(svytotal(~Z63,cluster,na.rm=TRUE,multicore=TRUE)),0),")")
      table[10,3] <- paste0(round(svytotal(~Z64,cluster,na.rm=TRUE,multicore=TRUE),0)," (",round(SE(svytotal(~Z64,cluster,na.rm=TRUE,multicore=TRUE)),0),")")
      table[11,3] <- paste0(round(svytotal(~Z65,cluster,na.rm=TRUE,multicore=TRUE),0)," (",round(SE(svytotal(~Z65,cluster,na.rm=TRUE,multicore=TRUE)),0),")")
      
      table <- table %>%
        setNames(c("Code","n","Weighted"))
      
      table <- table %>%
        mutate(Prop = as.numeric(n)/k)
      
      table <- table %>%
        select(Code,n,Prop,Weighted) %>%
        mutate(Year = year_)
      
      output <- rbind(output,table)
      print(paste0("Completed ",i))
      
      setwd("~/Library/CloudStorage/Box-Box/NEDS-sdh")
      gc()
}

test <- output %>%
  pivot_wider(id_cols = c("Code"),values_from=c("n","Prop","Weighted"),names_from=Year)

write.csv(test,"table-outputs/Table-1.csv",row.names=FALSE)



