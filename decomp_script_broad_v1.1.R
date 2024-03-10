#New decomposition script
#22nd March 2020
#Jonny Currie StR in Public Health and Primary Care

#Packages

library(tidyverse)
library(readxl)
library(viridis)
library(janitor)
library(patchwork)
library(kableExtra)
library(rmarkdown)
library(PHEindicatormethods)

#Get mortality data (by LSOA)

deaths <- read.csv("deaths_sql_v2.csv") %>%
  rename(Year = DeathRegistrationCalendarYear, 
         Sex = DeceasedSexDescription, 
         icd10_code = DeathCauseDiagnosisUnderlyingCode, 
         lsoa_code = DeceasedLowerSuperOutputAreaCode) %>%
  mutate(icd10_code = substr(icd10_code, 1, 3))

#Need to switch to long format, merge with LSOA WIMD19 lookup and map each ICD-10 code to category
#Do this in reverse order!

#Get ICD-10 cause of death categorisation lookup
#sheet 2 for broad, sheet 1 for narrow

icd10_narrow <- readxl::read_xlsx("icd10_lookup.xlsx", 
                                 sheet = 2) %>%
  select(-c("icd10_category", "icd10_no"))

#Merge cause of death lookup with mortality data

deaths_categs <- left_join(deaths, icd10_narrow, 
                           by = c("icd10_code")) %>%
  mutate(category = tidyr::replace_na(category, "Other"))

#Merge this data with LSOA quintile lookup

lsoa_wimd19_lookup <- read.csv("lsoa_quin_lookup.csv")

deaths_lsoa_quins <- left_join(lsoa_wimd19_lookup,
                               deaths_categs,
                               by = "lsoa_code")

#Now convert this dataset to long

deaths_lsoa_quins <- deaths_lsoa_quins %>%
  pivot_longer(names_to = "ageband", 
               values_to = "deaths", 
               cols = -c(Wales_Quintile_2019, lsoa_code, Year,
                         Sex, icd10_code, category)) %>%
  mutate(ageband = case_when(
    ageband == "X00_00" ~ "0", 
    ageband == "X01_04" ~ "1", 
    ageband == "X05_09" ~ "5",
    ageband == "X10_14" ~ "10", 
    ageband == "X15_19" ~ "15", 
    ageband == "X20_24" ~ "20", 
    ageband == "X25_29" ~ "25", 
    ageband == "X30_34" ~ "30", 
    ageband == "X35_39" ~ "35", 
    ageband == "X40_44" ~ "40", 
    ageband == "X45_49" ~ "45", 
    ageband == "X50_54" ~ "50", 
    ageband == "X55_59" ~ "55", 
    ageband == "X60_64" ~ "60", 
    ageband == "X65_69" ~ "65", 
    ageband == "X70_74" ~ "70", 
    ageband == "X75_79" ~ "75", 
    ageband == "X80_84" ~ "80", 
    ageband == "X85_89" ~ "85", 
    ageband == "X90." ~ "90", 
    TRUE ~ as.character(ageband)
  ))

#Aggregate up to disease categories and WIMD19 quintiles and 3-year rolling figures
#First for disease categories and WIMDQs

deaths_lsoa_quins_agg <- deaths_lsoa_quins %>%
  ungroup() %>%
  group_by(Year, Sex, ageband,
           Wales_Quintile_2019, category) %>%
  summarise(total_deaths = sum(deaths)) %>% 
  arrange(Year, Sex, ageband, Wales_Quintile_2019, category)

#Next for 3-year rolling figures

deaths_lsoa_quins_agg <- deaths_lsoa_quins_agg %>%
  ungroup() %>%
  pivot_wider(names_from = Year, 
              values_from = total_deaths, 
              names_prefix = "deaths_") %>%
  mutate(deaths_0103 = deaths_2001 + deaths_2002 + deaths_2003, 
         deaths_0204 = deaths_2002 + deaths_2003 + deaths_2004, 
         deaths_0305 = deaths_2003 + deaths_2004 + deaths_2005, 
         deaths_0406 = deaths_2004 + deaths_2005 + deaths_2006,
         deaths_0507 = deaths_2005 + deaths_2006 + deaths_2007,
         deaths_0608 = deaths_2006 + deaths_2007 + deaths_2008, 
         deaths_0709 = deaths_2007 + deaths_2008 + deaths_2009, 
         deaths_0810 = deaths_2008 + deaths_2009 + deaths_2010, 
         deaths_0911 = deaths_2009 + deaths_2010 + deaths_2011, 
         deaths_1012 = deaths_2010 + deaths_2011 + deaths_2012,
         deaths_1113 = deaths_2011 + deaths_2012 + deaths_2013, 
         deaths_1214 = deaths_2012 + deaths_2013 + deaths_2014, 
         deaths_1315 = deaths_2013 + deaths_2014 + deaths_2015, 
         deaths_1416 = deaths_2014 + deaths_2015 + deaths_2016, 
         deaths_1517 = deaths_2015 + deaths_2016 + deaths_2017, 
         deaths_1618 = deaths_2016 + deaths_2017 + deaths_2018) %>%
  select(-contains("2001")) %>%
  select(-contains("2002")) %>%
  select(-contains("2003")) %>%
  select(-contains("2004")) %>%
  select(-contains("2005")) %>%
  select(-contains("2006")) %>%
  select(-contains("2007")) %>%
  select(-contains("2008")) %>%
  select(-contains("2009")) %>%
  select(-contains("2010")) %>%
  select(-contains("2011")) %>%
  select(-contains("2012")) %>%
  select(-contains("2013")) %>%
  select(-contains("2014")) %>%
  select(-contains("2015")) %>%
  select(-contains("2016")) %>%
  select(-contains("2017")) %>%
  select(-contains("2018")) %>%
  pivot_longer(names_to = c(".value", "Year"), 
               cols = -c("Sex", "ageband", "Wales_Quintile_2019", 
                         "category"), 
               names_sep = "_",
               values_drop_na = FALSE) %>%
  mutate(Year = case_when(
    Year == "0103" ~ "2001-03",
    Year == "0204" ~ "2002-04", 
    Year == "0305" ~ "2003-05",
    Year == "0406" ~ "2004-06",
    Year == "0507" ~ "2005-07",
    Year == "0608" ~ "2006-08",
    Year == "0709" ~ "2007-09",
    Year == "0810" ~ "2008-10",
    Year == "0911" ~ "2009-11",
    Year == "1012" ~ "2010-12",
    Year == "1113" ~ "2011-13",
    Year == "1214" ~ "2012-14",
    Year == "1315" ~ "2013-15",
    Year == "1416" ~ "2014-16",
    Year == "1517" ~ "2015-17",
    Year == "1618" ~ "2016-18",
    TRUE ~ NA_character_)) %>% 
  mutate(ageband = as.numeric(ageband))

#Aggregate 0-1 and 1-5 age categories

deaths_lsoa_quins_agg <- deaths_lsoa_quins_agg %>%
  mutate(ageband = case_when(
    ageband == 0 ~ 0, 
    ageband == 1 ~ 0, 
    TRUE ~ as.numeric(ageband)))

deaths_lsoa_quins_agg <- deaths_lsoa_quins_agg %>% 
  group_by(Sex, ageband, Wales_Quintile_2019, category, Year) %>%
  summarise(deaths = sum(deaths)) %>% ungroup()

#Now get population data by year, sex and quintile for same period

popns <- read.csv("C://Users/Jo122989/Desktop/PHST3/Ineq_analysis/SQL_practice/pops_sql_v0.csv")

#This dataset is already aggregated to quintile level, just need to convert to long and to 3-year rolling figures before generating life tables

agg_popns <- popns %>%
  mutate(Sex = case_when(
    sex == "1" ~ "Male", 
    sex == "2" ~ "Female", 
    TRUE ~ as.character(sex)
  )) %>%
  select(-sex) %>%
  rename(Year = yr) %>%
  pivot_longer(names_to = "ageband", 
               values_to = "popn", 
               cols = -c(Year, Wales_Quintile_2019, Sex)) %>%
  mutate(ageband = case_when(
    ageband == "X00_00" ~ "0",
    ageband == "X01_04" ~ "1", 
    ageband == "X05_09" ~ "5",
    ageband == "X10_14" ~ "10", 
    ageband == "X15_19" ~ "15", 
    ageband == "X20_24" ~ "20", 
    ageband == "X25_29" ~ "25", 
    ageband == "X30_34" ~ "30", 
    ageband == "X35_39" ~ "35", 
    ageband == "X40_44" ~ "40", 
    ageband == "X45_49" ~ "45", 
    ageband == "X50_54" ~ "50", 
    ageband == "X55_59" ~ "55", 
    ageband == "X60_64" ~ "60", 
    ageband == "X65_69" ~ "65", 
    ageband == "X70_74" ~ "70", 
    ageband == "X75_79" ~ "75", 
    ageband == "X80_84" ~ "80", 
    ageband == "X85_89" ~ "85", 
    ageband == "X90." ~ "90", 
    TRUE ~ as.character(ageband)
  )) %>%
  pivot_wider(names_from = Year, 
              values_from = popn, 
              names_prefix = "popn_") %>%
  mutate(popn_0204 = popn_2002 + popn_2003 + popn_2004, 
         popn_0305 = popn_2003 + popn_2004 + popn_2005, 
         popn_0406 = popn_2004 + popn_2005 + popn_2006,
         popn_0507 = popn_2005 + popn_2006 + popn_2007,
         popn_0608 = popn_2006 + popn_2007 + popn_2008, 
         popn_0709 = popn_2007 + popn_2008 + popn_2009, 
         popn_0810 = popn_2008 + popn_2009 + popn_2010, 
         popn_0911 = popn_2009 + popn_2010 + popn_2011, 
         popn_1012 = popn_2010 + popn_2011 + popn_2012,
         popn_1113 = popn_2011 + popn_2012 + popn_2013, 
         popn_1214 = popn_2012 + popn_2013 + popn_2014, 
         popn_1315 = popn_2013 + popn_2014 + popn_2015, 
         popn_1416 = popn_2014 + popn_2015 + popn_2016, 
         popn_1517 = popn_2015 + popn_2016 + popn_2017, 
         popn_1618 = popn_2016 + popn_2017 + popn_2018) %>%
  select(-contains("2002")) %>%
  select(-contains("2003")) %>%
  select(-contains("2004")) %>%
  select(-contains("2005")) %>%
  select(-contains("2006")) %>%
  select(-contains("2007")) %>%
  select(-contains("2008")) %>%
  select(-contains("2009")) %>%
  select(-contains("2010")) %>%
  select(-contains("2011")) %>%
  select(-contains("2012")) %>%
  select(-contains("2013")) %>%
  select(-contains("2014")) %>%
  select(-contains("2015")) %>%
  select(-contains("2016")) %>%
  select(-contains("2017")) %>%
  select(-contains("2018")) %>%
  pivot_longer(names_to = c(".value", "Year"), 
               cols = -c("Sex", "ageband", "Wales_Quintile_2019"), 
               names_sep = "_",
               values_drop_na = FALSE) %>%
  mutate(Year = case_when(
    Year == "0103" ~ "2001-03",
    Year == "0204" ~ "2002-04", 
    Year == "0305" ~ "2003-05",
    Year == "0406" ~ "2004-06",
    Year == "0507" ~ "2005-07",
    Year == "0608" ~ "2006-08",
    Year == "0709" ~ "2007-09",
    Year == "0810" ~ "2008-10",
    Year == "0911" ~ "2009-11",
    Year == "1012" ~ "2010-12",
    Year == "1113" ~ "2011-13",
    Year == "1214" ~ "2012-14",
    Year == "1315" ~ "2013-15",
    Year == "1416" ~ "2014-16",
    Year == "1517" ~ "2015-17",
    Year == "1618" ~ "2016-18",
    TRUE ~ NA_character_)) %>%
  mutate(ageband = as.numeric(ageband))

#Aggregate population dataset to combine first 2 age categories

agg_popns <- agg_popns %>%
  mutate(ageband = case_when(
    ageband == 0 ~ 0, 
    ageband == 1 ~ 0, 
    TRUE ~ as.numeric(ageband)))

agg_popns <- agg_popns %>% 
  group_by(Sex, ageband, Wales_Quintile_2019, Year) %>%
  summarise(popn = sum(popn)) %>% ungroup()

#Join mortality and population datasets

deaths_popn_long <- left_join(agg_popns, 
                     deaths_lsoa_quins_agg, 
                     by = c("Year", "Sex", "Wales_Quintile_2019", "ageband")) %>%
  filter(Year != "2001-03")

#Need to remove NAs from perinatal death category variable

deaths_popn_long <- deaths_popn_long %>%
  mutate(deaths = tidyr::replace_na(deaths, 0))

#Need to aggregate up mortality stats to all-cause

le_deaths <- deaths_popn_long %>%
  select(Year, Sex, ageband, Wales_Quintile_2019, deaths) %>%
  group_by(Year, Sex, ageband, Wales_Quintile_2019) %>%
  summarise(deaths = sum(deaths))

le_popns <- deaths_popn_long %>%
  select(Year, Sex, ageband, Wales_Quintile_2019, popn)

#Rejoin

le_data <- left_join(le_deaths, le_popns, 
            by = c("Year", "Sex", "ageband", "Wales_Quintile_2019")) %>%
  unique()

#Generate life table

unique(le_data$ageband)

le_data <- le_data %>% arrange(ageband)

life_table <- le_data %>%
  group_by(Year, Sex, Wales_Quintile_2019) %>%
  mutate(ax #average fraction of interval lived
         = if_else(ageband %in% c("0"), 0.1, 0.5),
         Mx #age-specific death rate
         = deaths/popn, 
         n #number of years between intervals
         = if_else(ageband <90, dplyr::lead(ageband)-ageband, 2/Mx), 
         qx #proportion dying in interval
         = if_else(ageband <90, (n*Mx)/(1+n*(1-ax)*Mx), 1),
         px #proportion surviving 
         = 1-qx,
         lx #number alive at age x
         = if_else(ageband %in% c("0"), 100000, 1),
         px_lag = dplyr::lag(px, default = 1),
         lx = if_else(ageband >0, cumprod(lx*px_lag), lx),
         dx #number dying in interval
         = lx - dplyr::lead(lx, default = 0),
         Lx #total years lived in interval
         = if_else(ageband <90, n*(dplyr::lead(lx)+(ax*dx)), lx/Mx),
         Tx #total years lived beyond age x
         = rev(cumsum(rev(Lx))),
         ex #estimated life expectancy
         = Tx/lx)

#Create CIs for LE estimates

life_table <- life_table %>%
  mutate(step1 = ((qx^2)*(1-qx))/deaths) %>%
  mutate(step2 = (lx^2)*(((1-ax)*n+lead(ex)^2)*step1)) %>%
  replace_na(list(step2 = 0)) %>%
  mutate(step3 = rev(cumsum(rev(step2)))) %>% 
  mutate(step4 = step3/(lx^2)) %>% 
  mutate(se = sqrt(step4)) %>% 
  mutate(lci = ex-(1.96*se), 
         uci = ex+(1.96*se))

#Plot LE trends

life_table %>%
  select(ageband, Sex, ex, Year, Wales_Quintile_2019) %>% 
  filter(ageband == 0) %>%
  filter(Wales_Quintile_2019 == "1"| Wales_Quintile_2019 == "5") %>%
  pivot_wider(names_from = "Wales_Quintile_2019", 
              values_from = ex) %>%
  mutate(quintile_diff = `1` - `5`) %>%
  ggplot(aes(Year, quintile_diff, group = Sex, colour = Sex)) + 
  theme(axis.text.x = element_text(angle = 45)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~Sex)

#tabulate

life_table %>%
  select(ageband, Sex, ex, Year, Wales_Quintile_2019) %>% 
  filter(ageband == 0) %>%
  filter(Wales_Quintile_2019 == "1"| Wales_Quintile_2019 == "5") %>%
  pivot_wider(names_from = "Wales_Quintile_2019", 
              values_from = ex) %>%
  mutate(quintile_diff = `1` - `5`) %>%
  select(Sex, Year, quintile_diff) %>%
  arrange(Sex, Year) %>% view()


#Replot using patchwork

m <- life_table %>%
  ungroup() %>%
  filter(Sex == "Male") %>%
  select(ageband, ex, Year, Wales_Quintile_2019, se, deaths) %>% 
  filter(ageband == 0) %>%
  filter(Wales_Quintile_2019 == "1"| Wales_Quintile_2019 == "5") %>%
  pivot_wider(names_from = "Wales_Quintile_2019", 
              values_from = c("ex", "se", "deaths")) %>%
  mutate(quintile_diff = `ex_1` - `ex_5`, 
         se = sqrt((se_5^2/deaths_5) + (se_1^2/deaths_1)), 
         lci = quintile_diff - (1.96*se), 
         uci = quintile_diff + (1.96*se)) %>%
  ggplot(aes(Year, quintile_diff, group = 1)) + 
  theme(axis.text.x = element_text(angle = 90), 
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(colour = "grey"), 
        axis.ticks = element_blank()) + 
  geom_point(colour = "darkblue") + 
  geom_line(colour = "darkblue") + 
  geom_errorbar(aes(ymin=lci, ymax=uci), width=0.2, colour = "darkblue") + 
  labs(x=NULL, 
       y=NULL, 
       title = "Males") + 
  expand_limits(y = c(0, 8))

f <- life_table %>%
  ungroup() %>%
  filter(Sex == "Female") %>%
  select(ageband, ex, Year, Wales_Quintile_2019, se, deaths) %>% 
  filter(ageband == 0) %>%
  filter(Wales_Quintile_2019 == "1"| Wales_Quintile_2019 == "5") %>%
  pivot_wider(names_from = "Wales_Quintile_2019", 
              values_from = c("ex", "se", "deaths")) %>%
  mutate(quintile_diff = `ex_1` - `ex_5`, 
         se = sqrt((se_5^2/deaths_5) + (se_1^2/deaths_1)), 
         lci = quintile_diff - (1.96*se), 
         uci = quintile_diff + (1.96*se)) %>%
  ggplot(aes(Year, quintile_diff, group = 1)) + 
  theme(axis.text.x = element_text(angle = 90), 
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(colour = "grey"), 
        axis.ticks = element_blank()) + 
  geom_point(colour = "red") + 
  geom_line(colour = "red") + 
  geom_errorbar(aes(ymin=lci, ymax=uci), width=0.2, colour = "red") + 
  labs(x=NULL, 
       y="Life expectancy gap between 20% most and 20% least deprived areas (years)",
       title = "Females") + 
  expand_limits(y = c(0,8))

f + m + plot_annotation(title = "Trends in life expectancy inequalities in Wales by gender, 2002-2018", 
                        caption = "Source: ONS")

#Table trends in LE gap by gender

life_table %>%
  ungroup() %>%
  select(ageband, Sex, ex, Year, Wales_Quintile_2019) %>% 
  filter(ageband == 0) %>%
  filter(Wales_Quintile_2019 == "1"| Wales_Quintile_2019 == "5") %>%
  select(-ageband) %>%
  pivot_wider(names_from = Wales_Quintile_2019, 
              values_from = ex) %>%
  mutate(quintile_diff = `1` - `5`) %>% view()

#Plot trends in LE by quintile for both genders

ex_m <- life_table %>%
  filter(Sex == "Male") %>%
  ungroup() %>%
  mutate(Wales_Quintile_2019 = as.character(Wales_Quintile_2019)) %>%
  mutate(Wales_Quintile_2019 = case_when(
    Wales_Quintile_2019 == "1" ~ "5 - Least deprived", 
    Wales_Quintile_2019 == "2" ~ "4", 
    Wales_Quintile_2019 == "3" ~ "3", 
    Wales_Quintile_2019 == "4" ~ "2",
    Wales_Quintile_2019 == "5" ~ "1 - Most deprived",
    TRUE ~ as.character(Wales_Quintile_2019)
  )) %>%
  select(ageband, Sex, ex, Year, Wales_Quintile_2019, lci, uci) %>% 
  filter(ageband == 0) %>%
  ggplot(aes(x=Year, y=ex, group=Wales_Quintile_2019, colour=Wales_Quintile_2019)) + 
  geom_point() + 
  geom_line() + 
  geom_errorbar(aes(ymin=lci, ymax=uci), width=0.1) + 
  theme(panel.background = element_blank(), 
        panel.grid.major.y = element_line(colour = "grey"), 
        panel.grid.minor.y = element_line(colour = "grey"),
        axis.ticks = element_blank(), 
        axis.text.x = element_text(angle = 90)) + 
  scale_colour_viridis_d() + 
  expand_limits(y=c(70,85)) + 
  labs(x=NULL, y=NULL, 
       title = "Males") + 
  geom_vline(xintercept = 9.2, 
             linetype = "dotted")


ex_f <- life_table %>%
  filter(Sex == "Female") %>%
  ungroup() %>%
  mutate(Wales_Quintile_2019 = as.character(Wales_Quintile_2019)) %>%
  mutate(Wales_Quintile_2019 = case_when(
    Wales_Quintile_2019 == "1" ~ "5 - Least deprived", 
    Wales_Quintile_2019 == "2" ~ "4", 
    Wales_Quintile_2019 == "3" ~ "3", 
    Wales_Quintile_2019 == "4" ~ "2",
    Wales_Quintile_2019 == "5" ~ "1 - Most deprived",
    TRUE ~ as.character(Wales_Quintile_2019)
  )) %>%
  select(ageband, Sex, ex, Year, Wales_Quintile_2019, lci, uci) %>% 
  filter(ageband == 0) %>%
  ggplot(aes(x=Year, y=ex, group=Wales_Quintile_2019, colour=Wales_Quintile_2019)) + 
  geom_point() + 
  geom_line() + 
  geom_errorbar(aes(ymin=lci, ymax=uci), width=0.1) + 
  theme(panel.background = element_blank(), 
        panel.grid.major.y = element_line(colour = "grey"), 
        panel.grid.minor.y = element_line(colour = "grey"), 
        axis.ticks = element_blank(), 
        axis.text.x = element_text(angle = 90)) + 
  scale_colour_viridis_d() + 
  expand_limits(y = c(70,85)) + 
  labs(x=NULL, y="Life expectancy(years)", 
       title = "Females") + 
  geom_vline(xintercept = 9.2, 
             linetype = "dotted")

ex_f + ex_m + plot_layout(guides = "collect") + 
  plot_annotation(title = "Trends in life expectancy at birth for males and females in Wales, 2002-2018", 
                  caption = "Source: ONS.")

#Table of trends

life_table %>%
  ungroup() %>%
  select(ageband, Sex, ex, Year, Wales_Quintile_2019) %>% 
  filter(ageband == 0) %>%
  filter(Wales_Quintile_2019 == "1"| Wales_Quintile_2019 == "5") %>%
  select(-ageband) %>%
  view()

#Now undertake decomposition
#First for males

lt_m_bottom <- life_table %>%
  filter(Sex == "Male", Wales_Quintile_2019 == "1")

lt_m_top <- life_table %>%
  filter(Sex == "Male", Wales_Quintile_2019 == "5")

CalculateDiffTable <- function(lt1, lt2) {
  # If rates and sexes are provided, then the life table is calculated using
  # these inputs. Otherwise, the life tables of the two populations, lt1 and lt2, 
  # must be provided. It returns a merged life table for the two populations, 
  # used to calculate the contribution of each age group towards the change in 
  # life expectancy in CalculateAgeContribution()
  stopifnot(lt1$ageband == lt2$ageband)
  data.frame(
    age = lt1$ageband,
    lx1 = (lt1$lx)/100000,
    Lx1 = (lt1$Lx)/100000, 
    lx2 = (lt2$lx)/100000,
    Lx2 = (lt2$Lx)/100000, 
    ex1 = lt1$ex,
    ex2 = lt2$ex, 
    Year = lt1$Year)
}

lt_agedecomp_m <- CalculateDiffTable(lt1 = lt_m_bottom, 
                                     lt2 = lt_m_top)

lt_agedecomp_m <- lt_agedecomp_m %>%
  group_by(Year) %>%
  mutate(de #direct effect
         = lx1*((Lx2/lx2)-(Lx1/lx1)),
         ie #indirect effect 
         = (lx1*dplyr::lead(lx2)/lx2-dplyr::lead(lx1))*dplyr::lead(ex2),
         #There are no age categories beyond 90+ so indirect effect =0
         ie=if_else(age %in% c(90), 0, ie),
         te #total effect
         = de+ie
  )

#Next extract mortality dataframes as before

mort_cause_dec1_m <- deaths_popn_long %>%
  ungroup() %>%
  filter(Sex == "Male" & Wales_Quintile_2019 == "1") %>%
  mutate(mx = deaths/popn) %>%
  select(-Sex, -Wales_Quintile_2019, -deaths, -popn) %>%
  rename(mx2 = mx)

mort_cause_dec5_m <- deaths_popn_long %>%
  ungroup() %>%
  filter(Sex == "Male", 
         Wales_Quintile_2019 == "5") %>%
  mutate(mx = deaths/popn) %>%
  select(-Sex, -Wales_Quintile_2019, -deaths, -popn) %>%
  rename(mx1 = mx)

cause_decomp_m <- left_join(mort_cause_dec5_m,
                            mort_cause_dec1_m,
                            by = c("Year", "category", "ageband"))

#Convert to wide format so causes each have a column

cause_decomp_m_wide <- cause_decomp_m %>%
  ungroup() %>%
  group_by(Year, ageband) %>%
  pivot_wider(values_from = c("mx1", "mx2"), names_from = c("category"),
              names_sep = "_") %>%
  ungroup()

cause_decomp_m_wide$ageband <- as.numeric(cause_decomp_m_wide$ageband)

#Need to merge this data frame with age-specific decomposition data

#Remove unecessary variables frmo age decomposition data frame

age_decomp_m <- lt_agedecomp_m %>%
  select(age, te, Year) %>%
  rename(ageband = age)

names(cause_decomp_m_wide) <- make.names(names(cause_decomp_m_wide), unique = TRUE)

cause_decomp_m_wide$ageband <- as.numeric(cause_decomp_m_wide$ageband)

cause_age_decomp_m <- left_join(cause_decomp_m_wide, age_decomp_m, 
                                by = c("ageband", "Year"))

#Re-examining methodology from measureevaluation.org, need all-cause variable
#for each decile

#Quickest will be to identify all variables with string 'mx1' or 'mx2' and sum each

cause_age_decomp_m <- cause_age_decomp_m %>%
  mutate(mx1_all_cause = rowSums(cause_age_decomp_m[grep("mx1", 
                                                         names(cause_age_decomp_m))]), 
         mx2_all_cause = rowSums(cause_age_decomp_m[grep("mx2", 
                                                         names(cause_age_decomp_m))]))

#Calculate differences in rates by cause then contribution of each cause to 
#that ageband of mortality

names(cause_age_decomp_m) <- make.names(names(cause_age_decomp_m), unique = TRUE)

#Create functions (narrow and broad decomp approaches) for male and female decomposition

decomp_m_broad <- function(data){
  data <- data %>%
    mutate(diff_cancer = mx2_Cancer - mx1_Cancer,
           diff_circ = mx2_Circulatory - mx1_Circulatory, 
           diff_dementia = mx2_Dementia.and.Alzheimers - mx1_Dementia.and.Alzheimers, 
           diff_diabetes = mx2_Diabetes.mellitus - mx1_Diabetes.mellitus, 
           diff_dig = mx2_Digestive - mx1_Digestive, 
           diff_drug = mx2_Drug.related - mx1_Drug.related, 
           diff_external = mx2_External - mx1_External,
           diff_gu = mx2_Genitourinary - mx1_Genitourinary,
           diff_illdef = mx2_Ill.defined - mx1_Ill.defined,
           diff_inf = mx2_Infectious.diseases - mx1_Infectious.diseases,
           diff_cns = mx2_Nervous.system.diseases.excluding.Alzheimers - mx1_Nervous.system.diseases.excluding.Alzheimers,
           diff_other = mx2_Other - mx1_Other,
           diff_perinatal = mx2_Perinatal.conditions - mx1_Perinatal.conditions,
           diff_residual = mx2_Residual - mx1_Residual,
           diff_resp = mx2_Respiratory - mx1_Respiratory,
           diff_all_cause = mx2_all_cause - mx1_all_cause,
           prop_cancer = diff_cancer/diff_all_cause,
           prop_circ = diff_circ/diff_all_cause,
           prop_dementia = diff_dementia/diff_all_cause,
           prop_diabetes = diff_diabetes/diff_all_cause,
           prop_dig = diff_dig/diff_all_cause,
           prop_drug = diff_drug/diff_all_cause,
           prop_external = diff_external/diff_all_cause,
           prop_gu = diff_gu/diff_all_cause,
           prop_illdef = diff_illdef/diff_all_cause,
           prop_inf = diff_inf/diff_all_cause,
           prop_cns = diff_cns/diff_all_cause,
           prop_other = diff_other/diff_all_cause,
           prop_perinatal = diff_perinatal/diff_all_cause,
           prop_residual = diff_residual/diff_all_cause,
           prop_resp = diff_resp/diff_all_cause,
           contrib_cancer = prop_cancer*te,
           contrib_circ = prop_circ*te,
           contrib_dementia = prop_dementia*te,
           contrib_diabetes = prop_diabetes*te,
           contrib_dig = prop_dig*te,
           contrib_drug = prop_drug*te,
           contrib_external = prop_external*te,
           contrib_gu = prop_gu*te,
           contrib_illdef = prop_illdef*te,
           contrib_inf = prop_inf*te,
           contrib_cns = prop_cns*te,
           contrib_other = prop_other*te,
           contrib_perinatal = prop_perinatal*te,
           contrib_resp = prop_resp*te,
           contrib_residual = prop_residual*te) %>%
    select(ageband, Year, contains("contrib"))
}

#Run decomposition code 

male_decomp <- cause_age_decomp_m %>%
  decomp_m_broad()

#Convert to long format and recode cause data

male_decomp_long <- male_decomp %>%
  gather(cause, contribution, contrib_cancer:contrib_residual, 
         factor_key = TRUE)

#Explore

unique(male_decomp_long$Year)

male_decomp_long %>%
  filter(Year == "2016-18") %>%
  mutate(contribution = contribution * -1) %>%
  group_by(cause) %>%
  summarise(total_contribution = sum(contribution)) %>%
  arrange(-total_contribution) %>%
  adorn_totals("row")

#Plot in heat map for single 3-year period

male_decomp_long %>%
  mutate(contribution = contribution * -1) %>%
  filter(Year == "2002-04") %>%
  ggplot(aes(ageband, y=reorder(cause, contribution))) + 
  geom_tile(aes(fill = contribution), width = 5) + 
  scale_fill_viridis_c()

#Do similar for female data

lt_f_bottom <- life_table %>%
  filter(Sex == "Female", Wales_Quintile_2019 == "1")

lt_f_top <- life_table %>%
  filter(Sex == "Female", Wales_Quintile_2019 == "5")

CalculateDiffTable <- function(lt1, lt2) {
  # If rates and sexes are provided, then the life table is calculated using
  # these inputs. Otherwise, the life tables of the two populations, lt1 and lt2, 
  # must be provided. It returns a merged life table for the two populations, 
  # used to calculate the contribution of each age group towards the change in 
  # life expectancy in CalculateAgeContribution()
  stopifnot(lt1$ageband == lt2$ageband)
  data.frame(
    age = lt1$ageband,
    lx1 = (lt1$lx)/100000,
    Lx1 = (lt1$Lx)/100000, 
    lx2 = (lt2$lx)/100000,
    Lx2 = (lt2$Lx)/100000, 
    ex1 = lt1$ex,
    ex2 = lt2$ex, 
    Year = lt1$Year)
}

lt_agedecomp_f <- CalculateDiffTable(lt1 = lt_f_bottom, 
                                     lt2 = lt_f_top)

lt_agedecomp_f <- lt_agedecomp_f %>%
  group_by(Year) %>%
  mutate(de #direct effect
         = lx1*((Lx2/lx2)-(Lx1/lx1)),
         ie #indirect effect 
         = (lx1*dplyr::lead(lx2)/lx2-dplyr::lead(lx1))*dplyr::lead(ex2),
         #There are no age categories beyond 90+ so indirect effect =0
         ie=if_else(age %in% c(90), 0, ie),
         te #total effect
         = de+ie
  )

#Next extract mortality dataframes as before

mort_cause_dec1_f <- deaths_popn_long %>%
  ungroup() %>%
  filter(Sex == "Female" & Wales_Quintile_2019 == "1") %>%
  mutate(mx = deaths/popn) %>%
  select(-Sex, -Wales_Quintile_2019, -deaths, -popn) %>%
  rename(mx1 = mx)

mort_cause_dec5_f <- deaths_popn_long %>%
  ungroup() %>%
  filter(Sex == "Female", 
         Wales_Quintile_2019 == "5") %>%
  mutate(mx = deaths/popn) %>%
  select(-Sex, -Wales_Quintile_2019, -deaths, -popn) %>%
  rename(mx2 = mx)

cause_decomp_f <- left_join(mort_cause_dec1_f, 
                            mort_cause_dec5_f, 
                            by = c("Year", "category", "ageband"))

#Convert to wide format so causes each have a column

cause_decomp_f_wide <- cause_decomp_f %>%
  ungroup() %>%
  group_by(Year, ageband) %>%
  pivot_wider(values_from = c("mx1", "mx2"), names_from = c("category"),
              names_sep = "_") %>%
  ungroup()

cause_decomp_m_wide$ageband <- as.numeric(cause_decomp_m_wide$ageband)

#Need to merge this data frame with age-specific decomposition data

#Remove unecessary variables frmo age decomposition data frame

age_decomp_f <- lt_agedecomp_f %>%
  select(age, te, Year) %>%
  rename(ageband = age)

names(cause_decomp_f_wide) <- make.names(names(cause_decomp_f_wide), unique = TRUE)

cause_decomp_f_wide$ageband <- as.numeric(cause_decomp_f_wide$ageband)

cause_age_decomp_f <- left_join(cause_decomp_f_wide, age_decomp_f, 
                                by = c("ageband", "Year"))

#Re-examining methodology from measureevaluation.org, need all-cause variable
#for each decile

#Quickest will be to identify all variables with string 'mx1' or 'mx2' and sum each

cause_age_decomp_f <- cause_age_decomp_f %>%
  mutate(mx1_all_cause = rowSums(cause_age_decomp_f[grep("mx1", 
                                                         names(cause_age_decomp_f))]), 
         mx2_all_cause = rowSums(cause_age_decomp_f[grep("mx2", 
                                                         names(cause_age_decomp_f))]))

#Calculate differences in rates by cause then contribution of each cause to 
#that ageband of mortality

names(cause_age_decomp_f) <- make.names(names(cause_age_decomp_f), unique = TRUE)

#Create function for male and female decomposition

decomp_f_broad <- function(data){
  data <- data %>%
    mutate(diff_cancer = mx1_Cancer - mx2_Cancer,
           diff_circ = mx1_Circulatory - mx2_Circulatory, 
           diff_dementia = mx1_Dementia.and.Alzheimers - mx2_Dementia.and.Alzheimers, 
           diff_diabetes = mx1_Diabetes.mellitus - mx2_Diabetes.mellitus, 
           diff_dig = mx1_Digestive - mx2_Digestive, 
           diff_drug = mx1_Drug.related - mx2_Drug.related, 
           diff_external = mx1_External - mx2_External,
           diff_gu = mx1_Genitourinary - mx2_Genitourinary,
           diff_illdef = mx1_Ill.defined - mx2_Ill.defined,
           diff_inf = mx1_Infectious.diseases - mx2_Infectious.diseases,
           diff_cns = mx1_Nervous.system.diseases.excluding.Alzheimers - mx2_Nervous.system.diseases.excluding.Alzheimers,
           diff_other = mx1_Other - mx2_Other,
           diff_perinatal = mx1_Perinatal.conditions - mx2_Perinatal.conditions,
           diff_residual = mx1_Residual - mx2_Residual,
           diff_resp = mx1_Respiratory - mx2_Respiratory,
           diff_all_cause = mx1_all_cause - mx2_all_cause,
           prop_cancer = diff_cancer/diff_all_cause,
           prop_circ = diff_circ/diff_all_cause,
           prop_dementia = diff_dementia/diff_all_cause,
           prop_diabetes = diff_diabetes/diff_all_cause,
           prop_dig = diff_dig/diff_all_cause,
           prop_drug = diff_drug/diff_all_cause,
           prop_external = diff_external/diff_all_cause,
           prop_gu = diff_gu/diff_all_cause,
           prop_illdef = diff_illdef/diff_all_cause,
           prop_inf = diff_inf/diff_all_cause,
           prop_cns = diff_cns/diff_all_cause,
           prop_other = diff_other/diff_all_cause,
           prop_perinatal = diff_perinatal/diff_all_cause,
           prop_residual = diff_residual/diff_all_cause,
           prop_resp = diff_resp/diff_all_cause,
           contrib_cancer = prop_cancer*te,
           contrib_circ = prop_circ*te,
           contrib_dementia = prop_dementia*te,
           contrib_diabetes = prop_diabetes*te,
           contrib_dig = prop_dig*te,
           contrib_drug = prop_drug*te,
           contrib_external = prop_external*te,
           contrib_gu = prop_gu*te,
           contrib_illdef = prop_illdef*te,
           contrib_inf = prop_inf*te,
           contrib_cns = prop_cns*te,
           contrib_other = prop_other*te,
           contrib_perinatal = prop_perinatal*te,
           contrib_resp = prop_resp*te,
           contrib_residual = prop_residual*te) %>%
    select(ageband, Year, contains("contrib"))
}

#Run decomposition code 

female_decomp <- cause_age_decomp_f %>%
  decomp_f_broad()

#Convert to long format and recode cause data

female_decomp_long <- female_decomp %>%
  gather(cause, contribution, contrib_cancer:contrib_residual, 
         factor_key = TRUE)

#Explore

female_decomp_long %>%
  filter(Year == "2016-18") %>%
  mutate(contribution = contribution * -1) %>%
  group_by(cause) %>%
  summarise(total_contribution = sum(contribution)) %>%
  arrange(total_contribution) %>%
  adorn_totals("row")

#Plot in heat map for single 3-year period

female_decomp_long %>%
  filter(Year == "2016-18") %>%
  mutate(contribution = contribution * -1) %>%
  ggplot(aes(ageband, y=reorder(cause, contribution))) + 
  geom_tile(aes(fill = contribution), width = 5) + 
  scale_fill_viridis()



#Produce patchwork merged visualisations as previous
#First for females

#Change factor levels for cause data

female_decomp_long <- female_decomp_long %>%
  mutate(cause = fct_recode(cause,
                            Cancer = "contrib_cancer", 
                            Circulatory = "contrib_circ", 
                            Dementia = "contrib_dementia", 
                            Diabetes = "contrib_diabetes", 
                            Digestive = "contrib_dig", 
                            Drug = "contrib_drug", 
                            External = "contrib_external", 
                            Genitourinary = "contrib_gu", 
                            Illdefined = "contrib_illdef", 
                            Infections = "contrib_inf", 
                            Neurological = "contrib_cns", 
                            Other = "contrib_other", 
                            Perinatal = "contrib_perinatal", 
                            Respiratory = "contrib_resp", 
                            Residual = "contrib_residual"
  ))

#Amend labels for agebands

female_decomp_long <- female_decomp_long %>%
  mutate(ageband = case_when(
    ageband == "0" ~ "0-4", 
    ageband == "5" ~ "5-9", 
    ageband == "10" ~ "10-14", 
    ageband == "15" ~ "15-19", 
    ageband == "20" ~ "20-24", 
    ageband == "25" ~ "25-29", 
    ageband == "30" ~ "30-34", 
    ageband == "35" ~ "35-39", 
    ageband == "40" ~ "40-44", 
    ageband == "45" ~ "45-49", 
    ageband == "50" ~ "50-54", 
    ageband == "55" ~ "55-59", 
    ageband == "60" ~ "60-64", 
    ageband == "65" ~ "65-69", 
    ageband == "70" ~ "70-74", 
    ageband == "75" ~ "75-79", 
    ageband == "80" ~ "80-84", 
    ageband == "85" ~ "85-89", 
    ageband == "90" ~ "90+",
    TRUE ~ as.character(ageband)))

female_decomp_long <- female_decomp_long %>% 
  mutate(ageband = factor(ageband, levels = c("0-4", "5-9", "10-14", "15-19", "20-24", 
                                              "25-29", "30-34", "35-39", "40-44", 
                                              "45-49", "50-54", "55-59", "60-64", 
                                              "65-69", "70-74", "75-79", "80-84", 
                                              "85-89", "90+")))

heat_f <- female_decomp_long %>%
  filter(Year == "2016-18") %>%
  mutate(contribution = contribution * -1) %>%
  ggplot(aes(ageband, y=reorder(cause, contribution))) + 
  geom_tile(aes(fill = contribution)) + 
  theme(panel.background = element_blank(), 
        axis.ticks = element_blank()) + 
  scale_fill_viridis(name = "Contribution to life expectancy gap (years)") + 
  labs(x=NULL, y=NULL, title=NULL, subtitle=NULL)

cause_f <- female_decomp_long %>%
  mutate(contribution = contribution * -1) %>%
  filter(Year == "2016-18") %>%
  group_by(Year, cause) %>%
  summarise(cause_contrib = sum(contribution)) %>%
  ggplot(aes(x=reorder(cause, cause_contrib), y=cause_contrib)) + 
  geom_bar(fill="purple", stat="identity") + 
  coord_flip() + 
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        axis.text.y = element_blank(), 
        plot.margin = unit(c(0,0,0,0), "cm"), 
        axis.ticks.y = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(y="Total contribution (years)", 
       x=NULL, 
       title = "All ages") + 
  scale_y_continuous(expand = c(0,0))

age_f <- female_decomp_long %>%
  mutate(contribution = contribution * -1) %>%
  filter(Year == "2016-18") %>%
  group_by(ageband) %>%
  summarise(te = sum(contribution)) %>%
  arrange(ageband) %>%
  ggplot(aes(x=ageband, y=te)) + 
  geom_bar(stat="identity", fill="purple") + 
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        plot.margin = unit(c(0,0,0,0), "cm"), 
        axis.text.x = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(x=NULL, 
       y="Total contribution (years)", 
       title = "All causes") + 
  scale_y_continuous(expand = c(0,0))

age_f + guide_area() + heat_f + cause_f + plot_layout(ncol = 2, guides = "collect", 
                                                      widths = c(2,1), 
                                                      heights = c(1,2)) + 
  plot_annotation(title = "Females", 
                  theme = theme(plot.title = element_text(size = 20)))


#Next for males

#Change factor levels for cause data

male_decomp_long <- male_decomp_long %>%
  mutate(cause = fct_recode(cause,
                            Cancer = "contrib_cancer", 
                            Circulatory = "contrib_circ", 
                            Dementia = "contrib_dementia", 
                            Diabetes = "contrib_diabetes", 
                            Digestive = "contrib_dig", 
                            Drug = "contrib_drug", 
                            External = "contrib_external", 
                            Genitourinary = "contrib_gu", 
                            Illdefined = "contrib_illdef", 
                            Infections = "contrib_inf", 
                            Neurological = "contrib_cns", 
                            Other = "contrib_other", 
                            Perinatal = "contrib_perinatal", 
                            Respiratory = "contrib_resp", 
                            Residual = "contrib_residual"
  ))

male_decomp_long <- male_decomp_long %>%
  mutate(ageband = case_when(
    ageband == "0" ~ "0-4", 
    ageband == "5" ~ "5-9", 
    ageband == "10" ~ "10-14", 
    ageband == "15" ~ "15-19", 
    ageband == "20" ~ "20-24", 
    ageband == "25" ~ "25-29", 
    ageband == "30" ~ "30-34", 
    ageband == "35" ~ "35-39", 
    ageband == "40" ~ "40-44", 
    ageband == "45" ~ "45-49", 
    ageband == "50" ~ "50-54", 
    ageband == "55" ~ "55-59", 
    ageband == "60" ~ "60-64", 
    ageband == "65" ~ "65-69", 
    ageband == "70" ~ "70-74", 
    ageband == "75" ~ "75-79", 
    ageband == "80" ~ "80-84", 
    ageband == "85" ~ "85-89", 
    ageband == "90" ~ "90+",
    TRUE ~ as.character(ageband)))

male_decomp_long <- male_decomp_long %>% 
  mutate(ageband = factor(ageband, levels = c("0-4", "5-9", "10-14", "15-19", "20-24", 
                                              "25-29", "30-34", "35-39", "40-44", 
                                              "45-49", "50-54", "55-59", "60-64", 
                                              "65-69", "70-74", "75-79", "80-84", 
                                              "85-89", "90+")))

heat_m <- male_decomp_long %>%
  filter(Year == "2016-18") %>%
  mutate(contribution = contribution * -1) %>%
  ggplot(aes(ageband, y=reorder(cause, contribution))) + 
  geom_tile(aes(fill = contribution)) + 
  theme(panel.background = element_blank(), 
        axis.ticks = element_blank()) + 
  scale_fill_viridis(name = "Contribution to life expectancy gap (years)") + 
  labs(x=NULL, y=NULL, title=NULL, subtitle=NULL)

cause_m <- male_decomp_long %>%
  mutate(contribution = contribution * -1) %>%
  filter(Year == "2016-18") %>%
  group_by(Year, cause) %>%
  summarise(cause_contrib = sum(contribution)) %>%
  ggplot(aes(x=reorder(cause, cause_contrib), y=cause_contrib)) + 
  geom_bar(fill="purple", stat="identity") + 
  coord_flip() + 
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        axis.text.y = element_blank(), 
        plot.margin = unit(c(0,0,0,0), "cm"), 
        axis.ticks.y = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(y="Total contribution (years)", 
       x=NULL, 
       title = "All ages") + 
  scale_y_continuous(expand = c(0,0))

age_m <- male_decomp_long %>%
  mutate(contribution = contribution * -1) %>%
  filter(Year == "2016-18") %>%
  group_by(ageband) %>%
  summarise(te = sum(contribution)) %>%
  arrange(ageband) %>%
  ggplot(aes(x=ageband, y=te)) + 
  geom_bar(stat="identity", fill="purple") + 
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        plot.margin = unit(c(0,0,0,0), "cm"), 
        axis.text.x = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(x=NULL, 
       y="Total contribution (years)", 
       title = "All causes") + 
  scale_y_continuous(expand = c(0,0))

age_m + guide_area() + heat_m + cause_m + plot_layout(ncol = 2, guides = "collect", 
                                                      widths = c(2,1), 
                                                      heights = c(1,2)) + 
  plot_annotation(title = "Males", 
                  theme = theme(plot.title = element_text(size = 20)))

#Tables of decomposition by age and cause for both genders for latest period
#Males

male_decomp_long %>%
  filter(Year == "2016-18") %>%
  mutate(contribution = contribution * -1) %>%
  group_by(cause) %>%
  summarise(total_contribution = sum(contribution)) %>%
  arrange(-total_contribution) %>%
  kable() %>% kable_styling()

female_decomp_long %>%
  filter(Year == "2016-18") %>%
  mutate(contribution = contribution * -1) %>%
  group_by(cause) %>%
  summarise(total_contribution = sum(contribution)) %>%
  arrange(-total_contribution) %>%
  kable() %>% kable_styling()


#Try to deploy this in Shiny!
#starting with all-Wales data for males 

# First save the dataframe in csv format to Github

glimpse(male_decomp_long)
unique(male_decomp_long$cause)

#Change factor levels for cause data

male_decomp_long <- male_decomp_long %>%
  mutate(cause = fct_recode(cause,
    Cancer = "contrib_cancer", 
    Circulatory = "contrib_circ", 
    Dementia = "contrib_dementia", 
    Diabetes = "contrib_diabetes", 
    Digestive = "contrib_dig", 
    Drug = "contrib_drug", 
    External = "contrib_external", 
    Genitourinary = "contrib_gu", 
    Illdefined = "contrib_illdef", 
    Infections = "contrib_inf", 
    Neurological = "contrib_cns", 
    Other = "contrib_other", 
    Perinatal = "contrib_perinatal", 
    Respiratory = "contrib_resp", 
    Residual = "contrib_residual"
    ))

male_decomp_long %>%
  write.csv("male_decomp_long.csv")

#Now write new script (shiny_test_decomp.R) to make shiny dashboard

#Next explore subdomains of amenable + preventable mortality codes, coding other for those ICD codes not included

#Note broad categories are now in sheet 2 of the icd-10 lookup

#Then having done this understand how to rank LSOAs by LHB/RPB area

#Remove panel/plot background from heat map

male_decomp_long %>%
  filter(Year == "2016-18") %>%
  mutate(contribution = contribution * -1) %>%
  ggplot(aes(ageband, y=reorder(cause, contribution))) + 
  geom_tile(aes(fill = contribution), width = 5) + 
  theme(panel.background = element_blank()) + 
  scale_fill_viridis()

#Create charts showing change over time

male_0204_decomp <- male_decomp_long %>%
  filter(Year == "2002-04")

male_1618_decomp <- male_decomp_long %>%
  filter(Year == "2016-18")

male_change <- bind_rows(male_0204_decomp, male_1618_decomp)

male_change$Year <- as.factor(as.character(male_change$Year))

male_change <- male_change %>%
  pivot_wider(values_from = contribution, 
              names_from = Year)

names(male_change) <- make.names(names(male_change), unique = TRUE)

male_change <- male_change %>%
  mutate(change = X2016.18 - X2002.04)

#Produce composite chart

#Change factor levels for cause data

male_change <- male_change %>%
  mutate(cause = fct_recode(cause,
                            Cancer = "contrib_cancer", 
                            Circulatory = "contrib_circ", 
                            Dementia = "contrib_dementia", 
                            Diabetes = "contrib_diabetes", 
                            Digestive = "contrib_dig", 
                            Drug = "contrib_drug", 
                            External = "contrib_external", 
                            Genitourinary = "contrib_gu", 
                            Illdefined = "contrib_illdef", 
                            Infections = "contrib_inf", 
                            Neurological = "contrib_cns", 
                            Other = "contrib_other", 
                            Perinatal = "contrib_perinatal", 
                            Respiratory = "contrib_resp", 
                            Residual = "contrib_residual"
  ))

male_change <- male_change %>% 
  mutate(ageband = factor(ageband, levels = c("0-4", "5-9", "10-14", "15-19", "20-24", 
                                              "25-29", "30-34", "35-39", "40-44", 
                                              "45-49", "50-54", "55-59", "60-64", 
                                              "65-69", "70-74", "75-79", "80-84", 
                                              "85-89", "90+")))

heat_m_change <- male_change %>%
  mutate(change = change * -1) %>%
  ggplot(aes(ageband, y=reorder(cause, change))) + 
  geom_tile(aes(fill = change), stat = "identity") + 
  theme(panel.background = element_blank(), 
        axis.ticks = element_blank()) + 
  scale_fill_viridis(name = "Contribution to change in life expectancy gap (years)", 
                     breaks = c(-0.10, -0.05, 0, 0.05, 0.10),  
                     labels = c(-0.10, -0.05, 0, 0.05, 0.10)) +
  labs(x=NULL, y=NULL, title=NULL, subtitle=NULL)

cause_m_change <- male_change %>%
  mutate(change = change * -1) %>%
  group_by(cause) %>%
  summarise(cause_change = sum(change)) %>%
  ggplot(aes(x=reorder(cause, cause_change), y=cause_change, fill=cause_change<0)) + 
  geom_bar(stat="identity") + 
  coord_flip() + 
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        axis.text.y = element_blank(), 
        plot.margin = unit(c(0,0,0,0), "cm"), 
        axis.ticks.y = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(y="Total change (years)", 
       x=NULL, 
       title = "All ages") + 
  scale_y_continuous(expand = c(0,0)) + 
  scale_fill_manual(values = c("red", "darkgreen")) + guides(fill = FALSE)

age_m_change <- male_change %>%
  mutate(change = change * -1) %>%
  group_by(ageband) %>%
  summarise(te = sum(change)) %>%
  arrange(ageband) %>%
  ggplot(aes(x=ageband, y=te, fill=te<0)) + 
  geom_bar(stat="identity") + 
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        plot.margin = unit(c(0,0,0,0), "cm"), 
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust=0.5)) + 
  labs(x=NULL, 
       y="Total change (years)", 
       title = "All causes") + 
  scale_y_continuous(expand = c(0,0)) + 
  scale_fill_manual(values = c("red", "darkgreen")) + guides(fill = FALSE)

age_m_change + guide_area() + heat_m_change + cause_m_change + plot_layout(ncol = 2, guides = "collect", 
                                                      widths = c(2,1), 
                                                      heights = c(1,2)) + 
  plot_annotation(title = "Males", 
                  theme = theme(plot.title = element_text(size = 20)))

#Graph same for females

female_0204_decomp <- female_decomp_long %>%
  filter(Year == "2002-04")

female_1618_decomp <- female_decomp_long %>%
  filter(Year == "2016-18")

female_change <- bind_rows(female_0204_decomp, female_1618_decomp)

female_change$Year <- as.factor(as.character(female_change$Year))

female_change <- female_change %>%
  pivot_wider(values_from = contribution, 
              names_from = Year)

names(female_change) <- make.names(names(female_change), unique = TRUE)

female_change <- female_change %>%
  mutate(change = X2016.18 - X2002.04)

#Produce composite chart

#Change factor levels for cause data

female_change <- female_change %>%
  mutate(cause = fct_recode(cause,
                            Cancer = "contrib_cancer", 
                            Circulatory = "contrib_circ", 
                            Dementia = "contrib_dementia", 
                            Diabetes = "contrib_diabetes", 
                            Digestive = "contrib_dig", 
                            Drug = "contrib_drug", 
                            External = "contrib_external", 
                            Genitourinary = "contrib_gu", 
                            Illdefined = "contrib_illdef", 
                            Infections = "contrib_inf", 
                            Neurological = "contrib_cns", 
                            Other = "contrib_other", 
                            Perinatal = "contrib_perinatal", 
                            Respiratory = "contrib_resp", 
                            Residual = "contrib_residual"
  ))

female_change <- female_change %>% 
  mutate(ageband = factor(ageband, levels = c("0-4", "5-9", "10-14", "15-19", "20-24", 
                                              "25-29", "30-34", "35-39", "40-44", 
                                              "45-49", "50-54", "55-59", "60-64", 
                                              "65-69", "70-74", "75-79", "80-84", 
                                              "85-89", "90+")))

heat_f_change <- female_change %>%
  mutate(change = change * -1) %>%
  ggplot(aes(ageband, y=reorder(cause, change))) + 
  geom_tile(aes(fill = change)) + 
  theme(panel.background = element_blank(), 
        axis.ticks = element_blank()) + 
  scale_fill_viridis(name = "Contribution to change in life expectancy gap (years)", 
                     breaks = c(-0.10, -0.05, 0, 0.05, 0.10),  
                     labels = c(-0.10, -0.05, 0, 0.05, 0.10)) + 
  labs(x=NULL, y=NULL, title=NULL, subtitle=NULL)

cause_f_change <- female_change %>%
  mutate(change = change * -1) %>%
  group_by(cause) %>%
  summarise(cause_change = sum(change)) %>%
  ggplot(aes(x=reorder(cause, cause_change), y=cause_change, fill=cause_change<0)) + 
  geom_bar(stat="identity") + 
  coord_flip() + 
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        axis.text.y = element_blank(), 
        plot.margin = unit(c(0,0,0,0), "cm"), 
        axis.ticks.y = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(y="Total change (years)", 
       x=NULL, 
       title = "All ages") + 
  scale_y_continuous(expand = c(0,0)) + 
  scale_fill_manual(values = c("red", "darkgreen")) + guides(fill = FALSE)

age_f_change <- female_change %>%
  mutate(change = change * -1) %>%
  group_by(ageband) %>%
  summarise(te = sum(change)) %>%
  arrange(ageband) %>%
  ggplot(aes(x=ageband, y=te, fill=te<0)) + 
  geom_bar(stat="identity") + 
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        plot.margin = unit(c(0,0,0,0), "cm"), 
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust=0.5)) + 
  labs(x=NULL, 
       y="Total change (years)", 
       title = "All causes") + 
  scale_y_continuous(expand = c(0,0)) + 
  scale_fill_manual(values = c("red", "darkgreen")) + guides(fill = FALSE)

age_f_change + guide_area() + heat_f_change + cause_f_change + plot_layout(ncol = 2, guides = "collect", 
                                                                           widths = c(2,1), 
                                                                           heights = c(1,2)) + 
  plot_annotation(title = "Females", 
                  theme = theme(plot.title = element_text(size = 20)))

#Table

male_change %>%
  mutate(change = change * -1) %>%
  group_by(cause) %>%
  summarise(tot_change = sum(change)) %>%
  arrange(-tot_change) %>%
  kable() %>% kable_styling()

female_change %>%
  mutate(change = change * -1) %>%
  group_by(cause) %>%
  summarise(tot_change = sum(change)) %>%
  arrange(-tot_change) %>%
  kable() %>% kable_styling()
