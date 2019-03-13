
library(dplyr)
as.data.frame((variable.names(cps_import)))

options(digits = 16)
names(cps_import) <- tolower(names(cps_import))

cps <- cps_import %>% 
  select(inctot, age, sex, educ99, year, cpsid, cpsidp, marbasecidp, month, mish) %>%
  filter(educ99 >= 15, 
         year >= 2014 & year <= 2017, 
         inctot != is.na(inctot)) %>% # BA grads (no postgrad)
  mutate(sex = factor(ifelse(sex == 2, 1, 0), levels = c(0, 1), labels = c("male", "female")),
         year = as.factor(year),
         bachelors = factor(ifelse(educ99 == 15, 1, 0), levels = c(0, 1), 
                            labels = c("no BA", "BA")), # dummy for BA or not
         inctot = as.numeric(inctot),
         educ99 = as.factor(educ99),
         age = as.factor(age), 
         cpsid = as.factor(cpsid),
         cpsidp = as.factor(cpsidp), # CPSIDP is just CPSID with additional person-level identifier
         marbasecidp = as.factor(marbasecidp), 
         month = as.factor(month), 
         mish = as.integer(mish))


sample_14 <- cps %>% filter(year == 2014 & mish <= 4, 
                            cpsidp != 0) # new obs in 2014 pool, does not select ASEC only cases

sample_15_1 <- cps %>% filter(year == 2015 & mish >= 5, 
                              cpsidp != 0) 

sample_15_2 <- cps %>% filter(year == 2015 & mish <= 4, 
                              cpsidp != 0) 

sample_16_1 <- cps %>% filter(year == 2016 & mish >= 5, 
                              cpsidp != 0)

sample_16_2 <- cps %>% filter(year == 2016 & mish <= 4, 
                              cpsidp != 0) 

sample_17 <- cps %>% filter(year == 2017 & mish >= 5, 
                            cpsidp != 0) 


cross_sect_dim <- as.data.frame(rbind(c("2014", dim(sample_14)), 
                                      c("2015(1)", dim(sample_15_1)), c("2015(2)", dim(sample_15_2)), 
                                      c("2016(1)", dim(sample_16_1))), c("2016(1)", dim(sample_16_2)), 
                                c("2017(1)", dim(sample_17_1)))

cross_sect_dim <- cross_sect_dim %>% 
  mutate(Year = V1, Obs = V2, Num_Var = V3) %>% 
  select(Year, Obs, Num_Var)

cross_sect_dim # Dimensions of cross-sectional data


p1 <- rbind(sample_14, sample_15_1) # Starting with total of 28,880 obs
nrow(as.data.frame(unique(p1$cpsidp))) # Number of unique personal identifiers in p1 is 19,938

p1_eval <- p1 %>% group_by(cpsidp) %>% 
  summarise("num_obs" = n()) %>% 
  mutate(keep = factor(ifelse(num_obs == 2, 1, 0), labels = c("no", "yes")))
p1_drop <- p1_eval %>% filter(num_obs == 1)
p1_keep <- anti_join(p1, p1_drop, by = "cpsidp")
p1_keep <- p1_keep %>% mutate(panel_wave = as.factor(1)) # add panel wave value

nrow(p1_keep) + nrow(p1_drop) == nrow(p1)


p2 <- rbind(sample_15_2, sample_16_1) # Starting with 27,808 obs
nrow(as.data.frame(unique(p2$cpsidp))) # Number of unique personal identifiers in p1 is 18,190

p2_eval <- p2 %>% group_by(cpsidp) %>% 
  summarise("num_obs" = n()) %>% 
  mutate(keep = factor(ifelse(num_obs == 2, 1, 0), labels = c("no", "yes")))
p2_drop <- p2_eval %>% filter(num_obs == 1) # dropping 8,572 rows that don't have 2 obs for whatever reason
p2_keep <- anti_join(p2, p2_drop, by = "cpsidp")  # end with 19,236 obs 
p2_keep <- p2_keep %>% mutate(panel_wave = as.factor(2)) # add panelwave value

nrow(p2_keep) + nrow(p2_drop) == nrow(p2) # Check if adds up 

p3 <- rbind(sample_16_2, sample_17) # Starting with 27,808 obs
nrow(as.data.frame(unique(p3$cpsidp))) # Number of unique personal identifiers in p1 is 18,190

p3_eval <- p3 %>% group_by(cpsidp) %>% 
  summarise("num_obs" = n()) %>% 
  mutate(keep = factor(ifelse(num_obs == 2, 1, 0), labels = c("no", "yes")))
p3_drop <- p3_eval %>% filter(num_obs == 1) # dropping 8,572 rows that don't have 2 obs for whatever reason
p3_keep <- anti_join(p3, p3_drop, by = "cpsidp")  # end with 19,236 obs 
p3_keep <- p3_keep %>% mutate(panel_wave = as.factor(2)) # add panelwave value

nrow(p2_keep) + nrow(p2_drop) == nrow(p2) # Check if adds up 

panel <- rbind(p1_keep, p2_keep, p3_keep)
panel %>% group_by(cpsidp) %>% 
  summarise("num_obs" = n()) %>% filter(num_obs != 2) # confirms that all individuals have 2 observations

dim(panel) # Final number of panel observations is 38,360. Does not match Dearden panel size of 21,028.


