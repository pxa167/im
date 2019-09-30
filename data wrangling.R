#Load required packages
library(survey)
library(foreign)
library(tidyverse)

#load data files
fyc_2016 <- read.xport("fyc_2016.ssp")
pm_2016 <- read.xport("pm_2016.ssp")
mc_2016 <- read.xport("mc_2016.ssp")

#change all column names to lower case for convenience 
colnames(fyc_2016) <- tolower(colnames(fyc_2016))
colnames(pm_2016) <- tolower(colnames(pm_2016))
colnames(mc_2016) <- tolower(colnames(mc_2016))

#select only desired columns from each data set
fyc_16 <- fyc_2016 %>%
            select(dupersid, varstr, varpsu, perwt16f, age16x, bmindx53, sex, racethx, hideg, povcat16, inscov16, region16)

pm_16 <- pm_2016 %>%
            select(dupersid, varstr, varpsu, perwt16f, rxxp16x, rxsf16x, rxdrgnam, tc1s1, tc1s2)

mc_16 <- mc_2016 %>%
            select(dupersid, varstr, varpsu, perwt16f, icd10cdx, ipnum)

#remove the large complete data files
rm(fyc_2016, pm_2016, mc_2016)

#filter medical conditions file (ie mc_16) to only include those individuals with heart failure (ICD10 code = I50)
mc_hf <- mc_16 %>%
          filter(icd10cdx == "I50")

hf <- mc_hf$dupersid

#join medical condition data with fyc(demographic) data file
fyc_hf <- fyc_16 %>%
            filter(dupersid %in% hf)

pm_hf <- pm_16 %>%
            filter(dupersid %in% hf)

#remove all non HF-specific files
rm(fyc_16, mc_16, pm_16)

#filter prescribed medicines file for only desired medications (beta blockers = 47, ace inhibitors = 42, arbs = 56, and aldosterone antagonists = 340)
pm_hf_1 <- pm_hf %>%
          filter(tc1s1 %in% c(47, 42, 56) | tc1s2 %in% c(340))

#combine ACE and ARB into just ACE
pm_hf_2 <- pm_hf_1 %>%
  mutate(tc1s1 = recode(tc1s1, `42` = 42,
                        `56` = 42,
                        `47` = 47,
                        `49` = 49))

#add categorical variables for use of each type of drug
pm_hf_3 <- pm_hf_1 %>%
  transmute(dupersid = dupersid,
         bb = ifelse(tc1s1 == 47, 1, 0),
         ace = ifelse(tc1s1 == 42, 1, 0),
         aldo = ifelse(tc1s1 == 49, 1, 0)) %>%
  group_by(dupersid) %>%
  summarise(n_bb = sum(bb),
            n_ace = sum(ace),
            n_aldo = sum(aldo))


#try to create ideal data frame for just beta blockers --> beta blocker ID var | bb n_presc | bb tot_exp | bb oop_exp
pm_bb <- pm_hf_2 %>%
  filter(tc1s1 == 47) %>%
  group_by(dupersid) %>%
  summarise(bb_n_presc = n(),
            bb_exp_tot = sum(rxxp16x),
            bb_exp_oop = sum(rxsf16x)) %>%
  mutate(bb = 1) %>%
  select(bb, everything())

pm_ace <- pm_hf_2 %>%
  filter(tc1s1 == 42) %>%
  group_by(dupersid) %>%
  summarise(ace_n_presc = n(),
            ace_exp_tot = sum(rxxp16x),
            ace_exp_oop = sum(rxsf16x)) %>%
  mutate(ace = 1)%>%
  select(ace, everything())

pm_aldo <- pm_hf_2 %>%
  filter(tc1s1 == 49) %>%
  group_by(dupersid) %>%
  summarise(aldo_n_presc = n(),
            aldo_exp_tot = sum(rxxp16x),
            aldo_exp_oop = sum(rxsf16x)) %>%
  mutate(aldo = 1)%>%
  select(aldo, everything())


  
test <- left_join(fyc_hf, pm_bb, by = "dupersid") %>%
  left_join(pm_ace, by = "dupersid") %>%
  left_join(pm_aldo, by = "dupersid")

test[is.na(test)] <- 0
data <- test

#Recode demographic variables
data <- data %>%
  mutate(sex = recode_factor(sex, `1` = "Male",
                             `2` = "Female"),
         racethx = recode_factor(racethx, `1` = "Hispanic",
                                 `2` = "White",
                                 `3` = "Black",
                                 `4` = "Asian",
                                 `5` = "Other"),
         povcat16 = recode_factor(povcat16, `1` = "Poor",
                                  `2` = "Near Poor",
                                  `3` = "Low Income",
                                  `4` = "Middle Income",
                                  `5` = "High Income"),
         inscov16 = recode_factor(inscov16, `1` = "Any Private",
                                  `2` = "Public Only",
                                  `3` = "Uninsured"),
         region16 = recode_factor(region16, `1` = "Northeast",
                                  `2` = "Midwest",
                                  `3` = "South",
                                  `4` = "West",
                                  `-1` = "Inapplicable"))

#save final data set
save(data, file = "chf_data.RData")
