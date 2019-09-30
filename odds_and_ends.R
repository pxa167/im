#Load required packages
library(survey)
library(foreign)
library(tidyverse)
library(ggplot2)

# Set options to deal with lonely psu 
options(survey.lonely.psu='adjust');

#create survey object
mepsdsgn = svydesign(
  id = ~varpsu, 
  strata = ~varstr, 
  weights = ~perwt16f, 
  data = fyc_hf, 
  nest = TRUE)  

#Calculating population characteristics (here, we do gender, both weighted and unweighted)
tab_unw <- fyc_hf %>%
  group_by(sex) %>%
  summarise(Freq = n()) %>%
  mutate(Prop = Freq/sum(Freq)) %>%
  arrange(desc(Prop))

tab_unw

ggplot(data = tab_unw, aes(x = sex, y = Prop)) +
  geom_col() +
  coord_flip() + 
  scale_x_discrete(limits = tab_unw$sex)+
  labs(title = "Unweighted Distribution of Gender")

tab_w <- svytable(~ sex, mepsdsgn) %>%
  as.data.frame() %>%
  mutate(Prop = Freq / sum(Freq)) %>%
  arrange(desc(Prop))

tab_w

ggplot(tab_w, aes(x = sex, y = Prop)) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limits = tab_w$sex) +
  labs(title = "Weighted Distribution of Race")

##Characteristics of heart failure population

#Age



# Sex
svytable(~ sex, mepsdsgn) %>%
  as.data.frame() %>%
  mutate(Prop = Freq / sum(Freq)) %>%
  arrange(desc(Prop))

#Race
svytable(~ racethx, mepsdsgn) %>%
  as.data.frame() %>%
  mutate(Prop = Freq / sum(Freq)) %>%
  arrange(desc(Prop))

#Insurance Status
svytable(~ inscov16, mepsdsgn) %>%
  as.data.frame() %>%
  mutate(Prop = Freq / sum(Freq)) %>%
  arrange(desc(Prop))

#Family Income Level
svytable(~ povcat16, mepsdsgn) %>%
  as.data.frame() %>%
  mutate(Prop = Freq / sum(Freq)) %>%
  arrange(desc(Prop))

#Region
svytable(~ region16, mepsdsgn) %>%
  as.data.frame() %>%
  mutate(Prop = Freq / sum(Freq)) %>%
  arrange(desc(Prop))


#random code
svytable(~bb, mepsdsgn)
svymean(~bb + ace + aldo, mepsdsgn)

svytotal(~ bb_n_presc, mepsdsgn)

svytotal(~ bb_exp_tot + ace_exp_tot + arb_exp_tot + aldo_exp_tot, mepsdsgn)
svymean(~ bb_exp_tot + ace_exp_tot + arb_exp_tot + aldo_exp_tot, mepsdsgn)

svytotal(~ bb_exp_oop + ace_exp_oop + arb_exp_oop + aldo_exp_oop, mepsdsgn)
svymean(~ bb_exp_oop + ace_exp_oop + arb_exp_oop + aldo_exp_oop, mepsdsgn) %>%
  as.data.frame() %>%
  glimpse()

svyby(formula = ~ bb, by = ~ racethx,
      design = mepsdsgn, 
      FUN = svymean, na.rm = TRUE, 
      keep.names = FALSE) %>%
  as.data.frame() %>%
  ggplot(aes(x = racethx, y = bb)) +
  geom_col()
