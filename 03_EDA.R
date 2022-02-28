#### Libraries/Functions/Definitions   ####
library(data.table)
library(tidyverse)
library(tidytable)
library(tictoc)
library(ggbeeswarm)
library(miceadds)
library(aod)

SDH.Code.Descriptions <- data.frame(
  Description=c(  "Problems related to education and literacy",
      "Problems related to employment and unemployment",
      "Occupational exposure to risk factors",
      "Problems related to physical environment",
      "Problems related to housing and economic circumstances",
      "Problems related to social environment",
      "Negative life event in childhood",
      "Problems related to upbringing",
      "Other problems related to primary support group, including",
      "Problems related to certain psychosocial circumstances",
      "Problems related to other psychosocial circumstances"),
  Code = c("Z55","Z56","Z57","Z58","Z59","Z60","Z61","Z62","Z63","Z64","Z65"))

Column.Headers.NEDS.Hosp <- c("discwt","hospwt","hosp_control","hosp_ed","hosp_region",
                              "hosp_trauma","hosp_urcat4","hosp_ur_teach","neds_stratum",
                              "n_disc_u","n_hosp_u","s_disc_u","s_hosp_u","total_edvisits","year")

#### Prepare data                      ####

# Load the icd10 labels
icd10 <- fread("data-raw/icd10-ccsr.csv") %>%
  select(c(1:2)) %>%
  setNames(c("i10_dx1","code.description"))
icd10$i10_dx1 <- gsub("\\'","",icd10$i10_dx1)

# Load data and create an indicator for SDH code present
df <- readRDS("data-cleaned/Prepared-Data-subsample.rds") %>%
  select(
    "age"      ,    "amonth"      , "aweekend"   ,  "died_visit",   "discwt"    ,   "disp_ed" ,    
    "dqtr"     ,    "edevent"     , "female"     ,  "hcupfile"  ,   "hosp_ed"   ,   "i10_dx1" ,    
    "key_ed"   ,    "neds_stratum", "pay1"       ,  "pay2"      ,   "pl_nchs"   ,   "race"    ,    
    "totchg_ed",    "year"        , "zipinc_qrtl",  "Z55"       ,   "Z56"       ,   "Z57"     ,    
    "Z59"      ,    "Z60"         , "Z62"        ,  "Z63"       ,   "Z64"       ,   "Z65")
hosp <- fread("data-raw/NEDS_2019_HOSPITAL.csv") %>%
  setNames(Column.Headers.NEDS.Hosp)

# Link to hospital-data and define hospital variables
df <- df %>%
  left_join(hosp,by="hosp_ed") %>%
  mutate(
    hosp_region = factor(case_when(
      hosp_region==1 ~ "Northeast",hosp_region==2 ~ "Midwest", 
      hosp_region==3 ~ "South", hosp_region==4~"West")),
    hosp_ur_teach = factor(case_when(
      hosp_ur_teach==0 ~ "Metropolitan non-teaching",
      hosp_ur_teach==1 ~ "Metropolitan teaching",
      hosp_ur_teach==2 ~ "Non-metropolitan")),
    hosp_urcat4 = factor(case_when(
      hosp_urcat4==1 ~ "Large metro >1 million",
      hosp_urcat4==2 ~ "Small metro <1 million",
      hosp_urcat4==3 ~ "Micropolitan",
      T ~ "Non-urban residual/collapsed categories")),
    total_edvisits = cut(total_edvisits,c(0,20000,40000,60000,80000,999999)))

# SDH indicator variable
df[,22:30][is.na(df[,22:30])] <- 0
df$SDH.Present <- df %>%
  select(Z55:Z65) %>%
  rowSums()
df$SDH.Present <- ifelse(df$SDH.Present>0,1,0)

df <- df %>%
# zipinc_qrtl as ordered factor
  mutate(zipinc_qrtl = relevel(factor(zipinc_qrtl,levels=c("1","2","3","4")),ref="1")) %>%
# female category
  mutate(female = case_when(
    female==1 ~ "female",
    female==0 ~ "male",
    T ~ "missing/other")) %>%
  mutate(female = factor(female)) %>%
# race categories
  mutate(race = case_when(
    race==1 ~ "white",
    race==2 ~ "black",
    race==3 ~ "hispanic",
    race==4 ~ "asian or pacific islander",
    race==5 ~ "native american",
    race==6 ~ "other",
    T ~ "missing/invalid")) %>%
  mutate(race = relevel(factor(race),ref="white")) %>%
# age category
  mutate(age_cat = relevel(factor(cut(age,c(0,18,25,32,40,64,999))),ref="(18,25]"))

#### EDA                               ####

# What proportions of visits have an SDH code?
sum(df$SDH.Present)/nrow(df)

# Which SDH codes are common?
df %>% select(Z55:Z65) %>%
  pivot_longer(cols=Z55:Z65) %>%
  group_by(name) %>%
  summarise(Count=sum(value),Prop=sum(value)/n()) %>%
  rename(Code=name) %>%
  full_join(SDH.Code.Descriptions,by="Code") %>%
  arrange(-Prop)

# Most common associated diagnosis codes
df %>%
  filter(SDH.Present==1) %>%
  group_by(i10_dx1) %>%
  summarise(Count=n()) %>%
  arrange(-Count) %>%
  slice(1:100) %>%
  left_join(icd10,by="i10_dx1") %>%
  View()

# How many hospitals never use an SDH code?
hospitals.w.no.SDH.codes <- df %>%
  group_by(hosp_ed) %>%
  summarise(SDH.Utilization = sum(SDH.Present)/n()) %>%
  filter(SDH.Utilization==0) %>%
  nrow()
hospitals.w.no.SDH.codes/length(unique(df$hosp_ed))

# What is SDH capture by hospital, raw counts
SDH.Capture.by.Hospital <- df %>% 
  group_by(hosp_ed) %>%
  summarise(
    SDH.Present = sum(SDH.Present),
    Total.Visits = n(),
    Proportion = sum(SDH.Present)/n()) 
SDH.Capture.by.Hospital %>%
  arrange(-Proportion)
SDH.Capture.by.Hospital %>%
  arrange(Proportion)

# Beeswarm for SDH utilization by hospital
df %>%
  group_by(hosp_ed) %>%
  summarise(SDH.Utilization = sum(SDH.Present)/n()) %>%
  #filter hospitals never reporting an SDH code
  filter(SDH.Utilization>0) %>%
  ggplot(aes(x="Hospitals",y=SDH.Utilization))+
  geom_quasirandom(alpha=0.3)+
  xlab("")+ylab("Proportion of Visits with SDH Code")+
  theme_bw()

# Without outlier
df %>%
  group_by(hosp_ed) %>%
  summarise(SDH.Utilization = sum(SDH.Present)/n()) %>%
  #filter hospitals never reporting an SDH code
  filter(SDH.Utilization>0 & SDH.Utilization <0.15) %>%
  ggplot(aes(x="Hospitals",y=SDH.Utilization))+
  geom_quasirandom(alpha=0.3)+
  scale_y_continuous(labels=scales::percent_format())+
  xlab("")+ylab("Proportion of Visits with SDH Code")+
  theme_bw()

# Beeswarm plots with colors for hospital characteristics
# Without outlier

# hosp_region, hosp_ur_teach, hosp_urcat4, total_edvisits
k <- "hosp_urcat4"
df %>%
  group_by(hosp_ed) %>%
  summarise(SDH.Utilization = sum(SDH.Present)/n(),
            hosp_region = first(hosp_region),
            hosp_ur_teach = first(hosp_ur_teach),
            hosp_urcat4 = first(hosp_urcat4),
            total_edvisits = first(total_edvisits)) %>%
  #filter hospitals never reporting an SDH code
  filter(SDH.Utilization>0 & SDH.Utilization <0.15) %>%
  ggplot(aes(x="Hospitals",y=SDH.Utilization,color=get(k),group=get(k)))+
  geom_quasirandom(alpha=0.7)+
  scale_y_continuous(labels=scales::percent_format())+
  scale_color_viridis_d()+
  xlab("")+ylab("Proportion of Visits with SDH Code")+
  theme_bw()+
  theme(legend.title = element_blank())

# What is the proportion of SDH use by hospital characteristics
df %>% group_by(hosp_region) %>%
  summarise(SDH.Utilization = sum(SDH.Present)/n())

df %>% group_by(hosp_ur_teach) %>%
  summarise(SDH.Utilization = sum(SDH.Present)/n())

df %>% group_by(hosp_urcat4) %>%
  summarise(SDH.Utilization = sum(SDH.Present)/n())

df %>% group_by(total_edvisits) %>%
  summarise(SDH.Utilization = sum(SDH.Present)/n())

# Distribution of code use by race
df %>%
  group_by(race) %>%
  summarise(
    Total.Observations = n(),
    SDH.Utilization = sum(SDH.Present)/n()) %>%
  arrange(-SDH.Utilization)

# Distribution of code use by ZIP code income quartile
df %>%
  group_by(zipinc_qrtl) %>%
  summarise(
    Total.Observations = n(),
    SDH.Utilization = sum(SDH.Present)/n()) %>%
  arrange(-SDH.Utilization)

# Distribution by male/female
df %>%
  group_by(female) %>%
  summarise(
    Total.Observations = n(),
    SDH.Utilization = sum(SDH.Present)/n()) %>%
  arrange(-SDH.Utilization)

# Distribution by age+disposition
df %>%
  mutate(Admit.Indicator = disp_ed==9) %>%
  group_by(age_cat, SDH.Present) %>%
  summarise(
    Admit.Rate = sum(Admit.Indicator)/n()
  ) %>%
  pivot_wider(id_cols=age_cat,values_from=Admit.Rate,names_from=SDH.Present) %>%
  arrange(age_cat)

# Logistic regression to generate odds ratios
# warning super slow
model.visits <- glm.cluster(data=df,formula = SDH.Present~zipinc_qrtl+female+race+age_cat,
                     cluster="hosp_ed",family="binomial")
summary(model.visits)
exp(cbind(coef(model.visits), confint(model.visits)))

hosp_data <- df %>%
  group_by(hosp_ed) %>%
  summarise(SDH.Coding.Rate = sum(SDH.Present)/n(),
            hosp_region = first(hosp_region),
            hosp_urcat4 = first(hosp_urcat4),
            hosp_ur_teach = first(hosp_ur_teach),
            total_edvisits = first(total_edvisits))

model.hosp <- glm(data=hosp_data,formula = SDH.Coding.Rate~
                    hosp_region+hosp_urcat4+hosp_ur_teach+total_edvisits,
                          family=quasibinomial(link = "logit"))

exp(cbind(coef(model.hosp), confint(model.hosp)))







#### 



