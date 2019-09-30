#Load required packages
library(survey)
library(foreign)
library(tidyverse)
library(ggplot2)
library(scales)
library(broom)

# Set options to deal with lonely psu 
options(survey.lonely.psu='adjust');
cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                "#0072B2", "#D55E00", "#CC79A7")
#load data
load("chf_data.RData")

#create survey object
mepsdsgn = svydesign(
  id = ~varpsu, 
  strata = ~varstr, 
  weights = ~perwt16f, 
  data = data, 
  nest = TRUE)  


#Demographic Characteristics of Study Population---------------
svymean(~ sex , mepsdsgn)
svymean(~ racethx , mepsdsgn)
svymean(~ inscov16 , mepsdsgn)
svymean(~ povcat16 , mepsdsgn)
svymean(~ region16 , mepsdsgn)

svymean(~ sex , mepsdsgn) %>%
  as.data.frame() %>%
  add_column(gender = c("Male", "Female")) %>%
  ggplot(aes(y = mean, x = gender)) + 
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  xlab("Gender") +
  ggtitle("Breakdown of Study Population by Gender")

svymean(~ racethx , mepsdsgn) %>%
  as.data.frame() %>%
  add_column(racethx = levels(data$racethx)) %>%
  ggplot(aes(y = mean, x = racethx)) + 
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  xlab("Race/Ethnicity") +
  ggtitle("Breakdown of Study Population by Race/Ethnicity")

svymean(~ inscov16 , mepsdsgn) %>%
  as.data.frame() %>%
  add_column(inscov = levels(data$inscov16)) %>%
  ggplot(aes(y = mean, x = inscov)) + 
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  xlab("Type of Insurance Coverage") +
  ggtitle("Breakdown of Study Population by Type of Insurance Coverage")

svymean(~ povcat16 , mepsdsgn) %>%
  as.data.frame() %>%
  add_column(povcat = levels(data$povcat16)) %>%
  ggplot(aes(y = mean, x = povcat)) + 
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  xlab("Income") +
  ggtitle("Breakdown of Study Population by Type of Income")

svymean(~ region16 , mepsdsgn) %>%
  as.data.frame() %>%
  add_column(region = levels(data$region16)) %>%
  ggplot(aes(y = mean, x = region)) + 
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  xlab("Region") +
  ggtitle("Breakdown of Study Population by Region")


#What are the rates of take-up of each the three major drug classes in patients with HF?-----------------
svymean(~ bb + ace + aldo, mepsdsgn)

svymean(~ bb + ace + aldo, mepsdsgn) %>%
  as.data.frame() %>%
  add_column(med = factor(c("bb", "ace", "aldo"), levels = c("bb", "ace", "aldo"))) %>%
  ggplot(aes(x = med, y = mean)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  xlab("Medication Class") +
  scale_x_discrete(breaks=c("ace", "aldo", "bb"),
                   labels=c("ACEi/ARB", "Aldosterone Antagonist", "Beta-Blockers")) +
  ggtitle("Use by Medication Class") +
  ylab("Medication Use, %")


#What was total exp on each drug class? OOP exp on each drug class?---------------
svytotal(~ bb_exp_tot + ace_exp_tot + aldo_exp_tot, mepsdsgn)
svytotal(~ bb_exp_oop + ace_exp_oop + aldo_exp_oop, mepsdsgn)

a <- svytotal(~ bb_exp_tot + ace_exp_tot + aldo_exp_tot, mepsdsgn) %>%
  as.data.frame() %>%
  add_column(med = factor(c("bb", "ace", "aldo"), levels = c("bb", "ace", "aldo")),
             exp = "Total")

b <- svytotal(~ bb_exp_oop + ace_exp_oop + aldo_exp_oop, mepsdsgn) %>%
  as.data.frame() %>%
  add_column(med = factor(c("bb", "ace", "aldo"), levels = c("bb", "ace", "aldo")),
             exp = "Out of Pocket")

exp_df <- bind_rows(a,b)

ggplot(exp_df, aes(x = med, y = total, fill = exp)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(breaks=c("ace", "aldo", "bb"),
                   labels=c("ACEi/ARB", "Aldosterone Antagonist", "Beta-Blockers")) +
  scale_y_continuous(label = unit_format(unit = "m", scale = 1e-6)) +
  xlab("Drug Class") +
  ylab("Dollars Spent ($)") +
  labs(fill = "Expenditure Type") +
  scale_fill_manual(values=cb_palette) +
  ggtitle("Total and Out of Pocket Expenditures, MEPS 2016")


#What was average total exp on each drug class? Average OOP exp on each drug class?---------------
svymean(~ bb_exp_tot, subset(mepsdsgn, bb == 1))
svymean(~ ace_exp_tot, subset(mepsdsgn, ace == 1))
svymean(~ aldo_exp_tot, subset(mepsdsgn, aldo == 1))

svymean(~ bb_exp_oop, subset(mepsdsgn, bb == 1))
svymean(~ ace_exp_oop, subset(mepsdsgn, ace == 1))
svymean(~ aldo_exp_oop, subset(mepsdsgn, aldo == 1))

a <- svymean(~ bb_exp_tot, subset(mepsdsgn, bb == 1)) %>%
  as.data.frame()
b <- svymean(~ ace_exp_tot, subset(mepsdsgn, ace == 1)) %>%
  as.data.frame()
c <- svymean(~ aldo_exp_tot, subset(mepsdsgn, aldo == 1)) %>%
  as.data.frame()

d <- svymean(~ bb_exp_oop, subset(mepsdsgn, bb == 1)) %>%
  as.data.frame()
e <- svymean(~ ace_exp_oop, subset(mepsdsgn, ace == 1)) %>%
  as.data.frame()
f <- svymean(~ aldo_exp_oop, subset(mepsdsgn, aldo == 1)) %>%
  as.data.frame()

avg_tot_exp_df <- bind_rows(a,b,c) %>%
  gather(key = "med", value = "se", bb_exp_tot:aldo_exp_tot) %>%
  select(mean, se) %>%
  drop_na() %>%
  add_column(med = factor(c("bb", "ace", "aldo"), levels = c("bb", "ace", "aldo")),
             exp = "Total")

avg_oop_exp_df <- bind_rows(d,e,f) %>%
  gather(key = "med", value = "se", bb_exp_oop:aldo_exp_oop) %>%
  select(mean, se) %>%
  drop_na() %>%
  add_column(med = factor(c("bb", "ace", "aldo"), levels = c("bb", "ace", "aldo")),
             exp = "Out of Pocket")

bind_rows(avg_tot_exp_df, avg_oop_exp_df) %>%
  ggplot(aes(x = med, y = mean, fill = exp)) +
  geom_col(position = "dodge") +
  scale_x_discrete(breaks=c("ace", "aldo", "bb"),
                   labels=c("ACEi/ARB", "Aldosterone Antagonist", "Beta-Blockers")) +
  xlab("Drug Class") +
  ylab("Dollars Spent ($)") +
  labs(fill = "Expenditure Type") +
  scale_fill_manual(values=cb_palette) +
  ggtitle("Average Total and Out of Pocket Expenditures, MEPS 2016")

#How did take-up differ across demographic variables? ie gender, race, income, region, insurance coverage---------
##Region--------------
a <- svyby(formula = ~ bb, by = ~ region16,
           design = mepsdsgn, 
           FUN = svymean, na.rm = TRUE, 
           keep.names = FALSE) %>%
  as.data.frame() 

b <- svyby(formula = ~ ace, by = ~ region16,
           design = mepsdsgn, 
           FUN = svymean, na.rm = TRUE, 
           keep.names = FALSE) %>%
  as.data.frame() 

c <- svyby(formula = ~ aldo, by = ~ region16,
           design = mepsdsgn, 
           FUN = svymean, na.rm = TRUE, 
           keep.names = FALSE) %>%
  as.data.frame() 

region_df <- bind_rows(a,b,c) %>% 
  select(region16, se, everything()) %>% 
  gather(key = "med", value = "prop", bb:aldo) %>% 
  drop_na() %>%
  mutate(med = factor(med, levels = c("bb", "ace", "aldo")))

ggplot(region_df, aes(x = med, y = prop, fill = region16)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(breaks=c("ace", "aldo", "bb"),
                   labels=c("ACEi/ARB", "Aldosterone Antagonist", "Beta-Blockers")) +
  labs(fill = "Region") +
  scale_fill_manual(values=cb_palette) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Medication Class") +
  ggtitle("Breakdown by Region") +
  ylab("Medication Use, %")

## Race-------------
a <- svyby(formula = ~ bb, by = ~ racethx,
           design = mepsdsgn, 
           FUN = svymean, na.rm = TRUE, 
           keep.names = FALSE) %>%
  as.data.frame() 

b <- svyby(formula = ~ ace, by = ~ racethx,
           design = mepsdsgn, 
           FUN = svymean, na.rm = TRUE, 
           keep.names = FALSE) %>%
  as.data.frame() 

c <- svyby(formula = ~ aldo, by = ~ racethx,
           design = mepsdsgn, 
           FUN = svymean, na.rm = TRUE, 
           keep.names = FALSE) %>%
  as.data.frame() 

region_df <- bind_rows(a,b,c) %>% 
  select(racethx, se, everything()) %>% 
  gather(key = "med", value = "prop", bb:aldo) %>% 
  drop_na() %>%
  mutate(med = factor(med, levels = c("bb", "ace", "aldo")))

ggplot(region_df, aes(x = med, y = prop, fill = racethx)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(breaks=c("ace", "aldo", "bb"),
                   labels=c("ACEi/ARB", "Aldosterone Antagonist", "Beta-Blockers")) +
  labs(fill = "Race/Ethnicity") +
  scale_fill_manual(values=cb_palette) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Medication Class") +
  ggtitle("Breakdown by Race/Ethnicity") +
  ylab("Medication Use, %")

##Gender-------------
a <- svyby(formula = ~ bb, by = ~ sex,
           design = mepsdsgn, 
           FUN = svymean, na.rm = TRUE, 
           keep.names = FALSE) %>%
  as.data.frame() 

b <- svyby(formula = ~ ace, by = ~ sex,
           design = mepsdsgn, 
           FUN = svymean, na.rm = TRUE, 
           keep.names = FALSE) %>%
  as.data.frame() 

c <- svyby(formula = ~ aldo, by = ~ sex,
           design = mepsdsgn, 
           FUN = svymean, na.rm = TRUE, 
           keep.names = FALSE) %>%
  as.data.frame() 

region_df <- bind_rows(a,b,c) %>% 
  select(sex, se, everything()) %>% 
  gather(key = "med", value = "prop", bb:aldo) %>% 
  drop_na() %>%
  mutate(med = factor(med, levels = c("bb", "ace", "aldo")))

ggplot(region_df, aes(x = med, y = prop, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(breaks=c("ace", "aldo", "bb"),
                   labels=c("ACEi/ARB", "Aldosterone Antagonist", "Beta-Blockers")) +
  labs(fill = "Gender") +
  scale_fill_manual(values=cb_palette) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Medication Class") +
  ggtitle("Breakdown by Gender") +
  ylab("Medication Use, %")
##Insurance Status-------------
a <- svyby(formula = ~ bb, by = ~ inscov16,
           design = mepsdsgn, 
           FUN = svymean, na.rm = TRUE, 
           keep.names = FALSE) %>%
  as.data.frame() 

b <- svyby(formula = ~ ace, by = ~ inscov16,
           design = mepsdsgn, 
           FUN = svymean, na.rm = TRUE, 
           keep.names = FALSE) %>%
  as.data.frame() 

c <- svyby(formula = ~ aldo, by = ~ inscov16,
           design = mepsdsgn, 
           FUN = svymean, na.rm = TRUE, 
           keep.names = FALSE) %>%
  as.data.frame() 

region_df <- bind_rows(a,b,c) %>% 
  select(inscov16, se, everything()) %>% 
  gather(key = "med", value = "prop", bb:aldo) %>% 
  drop_na() %>%
  mutate(med = factor(med, levels = c("bb", "ace", "aldo")))

ggplot(region_df, aes(x = med, y = prop, fill = inscov16)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(breaks=c("ace", "aldo", "bb"),
                   labels=c("ACEi/ARB", "Aldosterone Antagonist", "Beta-Blockers")) +
  labs(fill = "Type of Insurance Coverage") +
  scale_fill_manual(values=cb_palette) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Medication Class") +
  ggtitle("Breakdown by Type of Insurance Coverage") +
  ylab("Medication Use, %")
##Income Level-----------------------
a <- svyby(formula = ~ bb, by = ~ povcat16,
           design = mepsdsgn, 
           FUN = svymean, na.rm = TRUE, 
           keep.names = FALSE) %>%
  as.data.frame() 

b <- svyby(formula = ~ ace, by = ~ povcat16,
           design = mepsdsgn, 
           FUN = svymean, na.rm = TRUE, 
           keep.names = FALSE) %>%
  as.data.frame() 

c <- svyby(formula = ~ aldo, by = ~ povcat16,
           design = mepsdsgn, 
           FUN = svymean, na.rm = TRUE, 
           keep.names = FALSE) %>%
  as.data.frame() 

region_df <- bind_rows(a,b,c) %>% 
  select(povcat16, se, everything()) %>% 
  gather(key = "med", value = "prop", bb:aldo) %>% 
  drop_na() %>%
  mutate(med = factor(med, levels = c("bb", "ace", "aldo")))

ggplot(region_df, aes(x = med, y = prop, fill = povcat16)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(breaks=c("ace", "aldo", "bb"),
                   labels=c("ACEi/ARB", "Aldosterone Antagonist", "Beta-Blockers")) +
  labs(fill = "Income Category") +
  scale_fill_manual(values=cb_palette) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Medication Class") +
  ggtitle("Breakdown by Income Category") +
  ylab("Medication Use, %")

#Inference-------------
svychisq(~ bb + sex, mepsdsgn)
svychisq(~ ace + sex, mepsdsgn)
svychisq(~ aldo + sex, mepsdsgn) 

model <- svyglm(bb ~ sex, design = mepsdsgn, family = quasibinomial())
tidy_m <- tidy(model)
tidy_m$OR <- exp(tidy_m$estimate)
tidy_m$lower_CI <- exp(tidy_m$estimate - 1.96 * tidy_m$std.error)
tidy_m$upper_CI <- exp(tidy_m$estimate + 1.96 * tidy_m$std.error)
tidy_m

svyglm(bb ~ sex, design = mepsdsgn, family = quasibinomial()) %>%
  tidy() %>%
  mutate(or = exp(estimate),
         lower_ci = exp(estimate - 1.96 * std.error),
         upper_ci = exp(estimate + 1.96 * std.error))



##Beta blocker odds ratios by predictors UNIVARIATE------------------------
svyglm(bb ~ region16, design = mepsdsgn, family = quasibinomial()) %>%
  tidy() %>%
  mutate(or = exp(estimate),
         lower_ci = exp(estimate - 1.96 * std.error),
         upper_ci = exp(estimate + 1.96 * std.error))
svyglm(bb ~ raceth, design = mepsdsgn, family = quasibinomial()) %>%
  tidy() %>%
  mutate(or = exp(estimate),
         lower_ci = exp(estimate - 1.96 * std.error),
         upper_ci = exp(estimate + 1.96 * std.error))
svyglm(bb ~ povcat16, design = mepsdsgn, family = quasibinomial()) %>%
  tidy() %>%
  mutate(or = exp(estimate),
         lower_ci = exp(estimate - 1.96 * std.error),
         upper_ci = exp(estimate + 1.96 * std.error))
svyglm(bb ~ inscov16, design = mepsdsgn, family = quasibinomial()) %>%
  tidy() %>%
  mutate(or = exp(estimate),
         lower_ci = exp(estimate - 1.96 * std.error),
         upper_ci = exp(estimate + 1.96 * std.error))

## ACE ARB odds ratios by predictors UNIVARIATE------------------------
svyglm(ace ~ sex, design = mepsdsgn, family = quasibinomial()) %>%
  tidy() %>%
  mutate(or = exp(estimate),
         lower_ci = exp(estimate - 1.96 * std.error),
         upper_ci = exp(estimate + 1.96 * std.error))
svyglm(ace ~ region16, design = mepsdsgn, family = quasibinomial()) %>%
  tidy() %>%
  mutate(or = exp(estimate),
         lower_ci = exp(estimate - 1.96 * std.error),
         upper_ci = exp(estimate + 1.96 * std.error))
svyglm(ace ~ raceth, design = mepsdsgn, family = quasibinomial()) %>%
  tidy() %>%
  mutate(or = exp(estimate),
         lower_ci = exp(estimate - 1.96 * std.error),
         upper_ci = exp(estimate + 1.96 * std.error))
svyglm(ace ~ povcat16, design = mepsdsgn, family = quasibinomial()) %>%
  tidy() %>%
  mutate(or = exp(estimate),
         lower_ci = exp(estimate - 1.96 * std.error),
         upper_ci = exp(estimate + 1.96 * std.error))
svyglm(ace ~ inscov16, design = mepsdsgn, family = quasibinomial()) %>%
  tidy() %>%
  mutate(or = exp(estimate),
         lower_ci = exp(estimate - 1.96 * std.error),
         upper_ci = exp(estimate + 1.96 * std.error))

## ALDO ANTAG odds ratios by predictors UNIVARIATE------------------------
svyglm(aldo ~ sex, design = mepsdsgn, family = quasibinomial()) %>%
  tidy() %>%
  mutate(or = exp(estimate),
         lower_ci = exp(estimate - 1.96 * std.error),
         upper_ci = exp(estimate + 1.96 * std.error))
svyglm(aldo ~ region16, design = mepsdsgn, family = quasibinomial()) %>%
  tidy() %>%
  mutate(or = exp(estimate),
         lower_ci = exp(estimate - 1.96 * std.error),
         upper_ci = exp(estimate + 1.96 * std.error))
svyglm(aldo ~ raceth, design = mepsdsgn, family = quasibinomial()) %>%
  tidy() %>%
  mutate(or = exp(estimate),
         lower_ci = exp(estimate - 1.96 * std.error),
         upper_ci = exp(estimate + 1.96 * std.error))
svyglm(aldo ~ povcat16, design = mepsdsgn, family = quasibinomial()) %>%
  tidy() %>%
  mutate(or = exp(estimate),
         lower_ci = exp(estimate - 1.96 * std.error),
         upper_ci = exp(estimate + 1.96 * std.error))
svyglm(aldo ~ inscov16, design = mepsdsgn, family = quasibinomial()) %>%
  tidy() %>%
  mutate(or = exp(estimate),
         lower_ci = exp(estimate - 1.96 * std.error),
         upper_ci = exp(estimate + 1.96 * std.error))

##Multivariate model
svyglm(bb ~ sex + racethx + inscov16 + povcat16 + region16, design = mepsdsgn, family = quasibinomial()) %>%
  tidy() %>%
  mutate(or = exp(estimate),
         lower_ci = exp(estimate - 1.96 * std.error),
         upper_ci = exp(estimate + 1.96 * std.error))