library(tidyverse)
library(srvyr)
library(stargazer)

load("D:/Books, Notes/APU/SEM 1/Quantitative and Research methodology/Working Directory/hh.per1.dg2.Rdata")

hh.per1.mn <- hh.per1.dg2 %>% 
  mutate(final_weights = if_else(ns.count.ssss.x == ns.count.sss.x, 
                                 plfs.mlt.x/400, 
                                 plfs.mlt.x/800)) %>% 
  filter(age >= 18 & age <= 60) %>%
  mutate(labor.f = factor(ifelse(upa.status.f == "se:own acc" | upa.status.f == "se:employer" | upa.status.f == "se:unpaid"| 
                                   upa.status.f == "rws" | upa.status.f == "casual:public" | upa.status.f == "casual:oth" | 
                                   upa.status.f == "unemp","In Labor Force", "Out of Labor Force" ))) %>% 
  mutate(labor2.f = factor(ifelse(upa.status.f == "unemp", "Unemployed", "Employed"))) %>% filter(labor2.f == "Employed")

hh.per1.mn<- hh.per1.mn %>% mutate(informal.f=ifelse(is.na(upa.status.f),NA,
                                                     ifelse(upa.status.f=="unemp"|upa.status.f=="education"|upa.status.f=="domestic duties"|
                                                              upa.status.f=="home use production"|upa.status.f=="rentiers etc"|
                                                              upa.status.f=="disabled"|upa.status.f=="other",NA,
                                                            ifelse(upa.status.f=="se:own acc"|upa.status.f=="se:unpaid"|
                                                                     upa.status.f=="casual:public"|upa.status.f=="casual:oth",1,
                                                                   ifelse((upa.status.f=="se:employer") & (enterprise.size.f=="<6"|enterprise.size.f=="6-10"),1,
                                                                          ifelse((upa.status.f=="rws") & (contract.type.f=="no written"|ss.benefits.f=="not eligible for any of above"),1,0))))))
# PHEW! just a bunch of nested ifelse statements.
# now convert to factor variable
hh.per1.mn$informal.f<-factor(hh.per1.mn$informal.f, levels=c("1","0"),labels=c("Informal","Formal"))
summary(hh.per1.mn$informal.f)  

levels(hh.per1.mn$sex.f) <- list("Male"=c("Male"), "Female/Oth"=c("Female", "Other"))

#Creating survey design
plfs_des <- hh.per1.mn %>%
  as_survey_design(weights = final_weights)

#Creating proportions for formal and informal workers
formal.informal <- plfs_des %>% 
  summarise(employed.informal = 100*survey_ratio(informal.f == 'Informal',
                                                 informal.f == 'Formal' | 
                                                   informal.f == 'Informal',
                                                 na.rm = T),
            employed.formal = 100*survey_ratio(informal.f == 'Formal', 
                                               informal.f == 'Formal' | 
                                                 informal.f == 'Informal',
                                               na.rm = T)) %>% 
  select(employed.formal, employed.informal)
#Informality by state
formal.informal.state <- plfs_des %>% 
  group_by(state.f) %>% 
  summarise(employed.informal = 100*survey_ratio(informal.f == 'Informal',
                                                 informal.f == 'Formal' | 
                                                   informal.f == 'Informal',
                                                 na.rm = T),
            employed.formal = 100*survey_ratio(informal.f == 'Formal',
                                               informal.f == 'Formal' |
                                                 informal.f == 'Informal',
                                               na.rm = T))
#Informality by caste
formal.informal.caste <- plfs_des %>% 
  group_by(social.group.f) %>% 
  summarise(employed.informal = 100*survey_ratio(informal.f == 'Informal', 
                                                 informal.f == 'Formal' | 
                                                   informal.f == 'Informal',
                                                 na.rm = T),
            employed.formal = 100*survey_ratio(informal.f == 'Formal', 
                                               informal.f == 'Formal' | 
                                                 informal.f == 'Informal',
                                               na.rm = T)) %>% 
  select(social.group.f, employed.informal, employed.formal)
#Informality by gender
formal.informal.gender <- plfs_des %>% 
  group_by(sex.f) %>% 
  summarise(employed.informal = 100*survey_ratio(informal.f == 'Informal',
                                                 informal.f == 'Formal' | 
                                                   informal.f == 'Informal',
                                                 na.rm = T),
            employed.formal = 100*survey_ratio(informal.f == 'Formal',
                                               informal.f == 'Formal' | 
                                                 informal.f == 'Informal',
                                               na.rm = T)) %>% 
  select(sex.f, employed.informal, employed.formal) %>% 
  rename(Informal = employed.informal, Formal = employed.formal) %>% 
  gather(Type, Value, Informal, Formal)

ggplot(formal.informal.gender,
       aes(x = sex.f, y = Value, fill = Type)) +
  geom_col(position = 'dodge')  + 
  labs( x = 'Gender', y = 'Distribution of Labor (%)') +
  labs(fill = 'Informality Status') +
  geom_text(aes(label=round(Value,1)), position=position_dodge(width=0.9), vjust=-0.25)+
  ggtitle("Informality Status (%) by Gender")

#Informality and incomes
formal.informal.income <- plfs_des %>% 
  group_by(informal.f) %>% 
  summarise(mean_income = survey_mean(tot.earnings.n, na.rm = T)) %>% 
  filter(is.na(informal.f ) == F) %>% 
  select(informal.f, mean_income)

formal.informal.gender.caste <- plfs_des %>% 
  group_by(sex.f, social.group.f) %>% 
  summarise(employed.informal = 100*survey_ratio(informal.f == 'Informal',
                                                 informal.f == 'Formal' |
                                                   informal.f == 'Informal',
                                                 na.rm = T),
            employed.formal = 100*survey_ratio(informal.f == 'Formal',
                                               informal.f == 'Formal' | 
                                                 informal.f == 'Informal',
                                               na.rm = T))

formal.informal.gender.income <- plfs_des %>% 
  group_by(informal.f, sex.f) %>% 
  summarise(mean_income = survey_mean(mpce, na.rm = T)) %>% 
  filter(is.na(informal.f ) == F)


formal.informal.caste.income <- plfs_des %>% 
  group_by(informal.f, social.group.f) %>% 
  summarise(mean_income = survey_mean(tot.earnings.n, na.rm = T)) %>% 
  filter(is.na(informal.f ) == F) %>% 
  select(informal.f, social.group.f, mean_income) %>% 
  pivot_wider(names_from = informal.f, values_from = mean_income)

formal.informal.urban.rural <- plfs_des %>% 
  group_by(sector.f) %>% 
  summarise(employed.informal = 100*survey_ratio(informal.f == 'Informal',
                                                 informal.f == 'Formal' |
                                                   informal.f == 'Informal',
                                                 na.rm = T),
            employed.formal = 100*survey_ratio(informal.f == 'Formal',
                                               informal.f == 'Formal' | 
                                                 informal.f == 'Informal',
                                               na.rm = T)) %>% 
  rename(Informal = employed.informal, Formal = employed.formal) %>% 
  gather(Type, Value, Informal, Formal) %>% 
  select(sector.f, Type, Value)

ggplot(formal.informal.urban.rural,
       aes(x = sector.f, y = Value, fill = Type)) +
  geom_col(position = 'dodge')  + 
  labs( x = 'Sector', y = 'Distribution of Labor (%)') +
  labs(fill = 'Informality Status') +
  geom_text(aes(label=round(Value,1)), position=position_dodge(width=0.9), vjust=-0.25)+
  ggtitle("Informality Status (%) by Sector")

#stargazer

stargazer(as.data.frame(formal.informal),
          type = "html",
          summary = FALSE,
          digits = 1,
          title = "Share of Formal and Informal Workforce (%)",
          out = "table1.html",
          notes=c("Source: Periodic Labour Force Survey, 2017-18"),
          covariate.labels = c("S. No.", "Formal (%)", "Informal(%)"))

stargazer(as.data.frame(formal.informal.caste),
          type = "html",
          summary = FALSE,
          digits = 1,
          title = "Share of Formal and Informal Workforce by Caste (%)",
          out = "table2.html",
          notes=c("Source: Periodic Labour Force Survey, 2017-18"),
          covariate.labels = c("S. No.", "Caste", "Informal (%)", "Formal(%)"))

stargazer(as.data.frame(formal.informal.gender),
          type = "html",
          summary = FALSE,
          digits = 1,
          title = "Share of Formal and Informal Workforce by Gender (%)",
          out = "table3.html",
          notes=c("Source: Periodic Labour Force Survey, 2017-18"),
          covariate.labels = c("S. No.", "Gender", "Informal (%)", "Formal(%)"))

stargazer(as.data.frame(formal.informal.caste.income),
          type = "html",
          summary = FALSE,
          digits = 1,
          title = "Mean Incomes by Caste (INR)",
          out = "table4.html",
          notes=c("Source: Periodic Labour Force Survey, 2017-18"),
          covariate.labels = c("S. No.", "Caste", "Informal (INR)", "Formal (INR)"))