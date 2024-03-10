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
                                 sheet = 1) %>%
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

decomp_m_narrow <- function(data){
  data <- data %>%
    mutate(diff_accidents = mx1_Accidents - mx2_Accidents,
           diff_bowelca = mx1_Bowel.cancer - mx2_Bowel.cancer, 
           diff_breastca = mx1_Breast.cancer - mx2_Breast.cancer, 
           diff_cerebrovasc = mx1_Cerebrovascular - mx2_Cerebrovascular, 
           diff_chd = mx1_CHD - mx2_CHD, 
           diff_CLD = mx1_Chronic.lower.respiratory.diseases - mx2_Chronic.lower.respiratory.diseases, 
           diff_liver = mx1_Cirrhosis.and.other.diseases.of.the.liver - mx2_Cirrhosis.and.other.diseases.of.the.liver,
           diff_dementia = mx1_Dementia.and.Alzheimers - mx2_Dementia.and.Alzheimers,
           diff_diabetes = mx1_Diabetes.mellitus - mx2_Diabetes.mellitus,
           diff_dig = mx1_Digestive.excluding.cirrhosis - mx2_Digestive.excluding.cirrhosis,
           diff_drug = mx1_Drug.related - mx2_Drug.related,
           diff_flu_pneum = mx1_Flu.and.pneumonia - mx2_Flu.and.pneumonia,
           diff_gu = mx1_Genitourinary - mx2_Genitourinary,
           diff_illdef = mx1_Ill.defined - mx2_Ill.defined,
           diff_inf = mx1_Infectious.diseases - mx2_Infectious.diseases,
           diff_lung_ca = mx1_Lung.cancer - mx2_Lung.cancer,
           diff_cns = mx1_Nervous.system.diseases.excluding.Alzheimers - mx2_Nervous.system.diseases.excluding.Alzheimers,
           diff_other = mx1_Other - mx2_Other,
           diff_other_canc = mx1_Other.cancer - mx2_Other.cancer, 
           diff_other_circ = mx1_Other.circulatory - mx2_Other.circulatory,
           diff_other_ext = mx1_Other.external - mx2_Other.external,
           diff_other_resp = mx1_Other.respiratory - mx2_Other.respiratory,
           diff_perinatal = mx1_Perinatal.conditions - mx2_Perinatal.conditions,
           diff_prostate_ca = mx1_Prostate.cancer - mx2_Prostate.cancer,
           diff_residual = mx1_Residual - mx2_Residual,
           diff_suicides = mx1_Suicides - mx2_Suicides,
           diff_mental = mx1_Mental.and.behavioural.disorders.excluding.dementia - mx2_Mental.and.behavioural.disorders.excluding.dementia,
           diff_all_cause = mx1_all_cause - mx2_all_cause,
           prop_accidents = diff_accidents/diff_all_cause,
           prop_bowelca = diff_bowelca/diff_all_cause,
           prop_breastca = diff_breastca/diff_all_cause,
           prop_cerebrovasc = diff_cerebrovasc/diff_all_cause,
           prop_chd  = diff_chd/diff_all_cause,
           prop_CLD = diff_CLD/diff_all_cause,
           prop_liver = diff_liver/diff_all_cause,
           prop_dementia = diff_dementia/diff_all_cause,
           prop_diabetes = diff_diabetes/diff_all_cause,
           prop_dig = diff_dig/diff_all_cause,
           prop_drug = diff_drug/diff_all_cause,
           prop_flu_pneum = diff_flu_pneum/diff_all_cause,
           prop_gu = diff_gu/diff_all_cause,
           prop_illdef = diff_illdef/diff_all_cause,
           prop_inf = diff_inf/diff_all_cause,          
           prop_lung_ca = diff_lung_ca/diff_all_cause,
           prop_cns = diff_cns/diff_all_cause,
           prop_other = diff_other/diff_all_cause,
           prop_other_canc = diff_other_canc/diff_all_cause,
           prop_other_circ = diff_other_circ/diff_all_cause,           
           prop_other_ext = diff_other_ext/diff_all_cause,
           prop_other_resp = diff_other_resp/diff_all_cause,
           prop_perinatal = diff_perinatal/diff_all_cause,
           prop_prostate_ca = diff_prostate_ca/diff_all_cause,
           prop_residual = diff_residual/diff_all_cause,
           prop_suicides = diff_suicides/diff_all_cause,
           prop_mental = diff_mental/diff_all_cause,
           contrib_accidents = prop_accidents*te,
           contrib_bowelca  = prop_bowelca*te,
           contrib_breastca = prop_breastca*te, 
           contrib_cerebrovasc = prop_cerebrovasc*te, 
           contrib_chd = prop_chd*te, 
           contrib_CLD = prop_CLD*te, 
           contrib_liver = prop_liver*te, 
           contrib_dementia = prop_dementia*te, 
           contrib_diabetes = prop_diabetes*te, 
           contrib_dig = prop_dig*te, 
           contrib_drug = prop_drug*te,
           contrib_flu_pneum = prop_flu_pneum*te, 
           contrib_gu = prop_gu*te, 
           contrib_illdef = prop_illdef*te, 
           contrib_inf = prop_inf*te, 
           contrib_lung_ca = prop_lung_ca*te, 
           contrib_cns = prop_cns*te, 
           contrib_other = prop_other*te, 
           contrib_other_canc = prop_other_canc*te, 
           contrib_other_circ = prop_other_circ,
           contrib_other_ext = prop_other_ext*te, 
           contrib_other_resp = prop_other_resp*te, 
           contrib_perinatal = prop_perinatal*te, 
           contrib_prostate_ca = prop_prostate_ca*te, 
           contrib_residual = prop_residual*te, 
           contrib_suicides = prop_suicides*te, 
           contrib_mental = prop_mental*te) %>%
    select(ageband, Year, contains("contrib"))
}

#Run decomposition code 

male_decomp <- cause_age_decomp_m %>%
  decomp_m_narrow()

#Convert to long format and recode cause data

male_decomp_long <- male_decomp %>%
  gather(cause, contribution, contrib_accidents:contrib_mental, 
         factor_key = TRUE)

#Explore

unique(male_decomp_long$Year)

male_decomp_long %>%
  filter(Year == "2016-18") %>%
  mutate(contribution = contribution * -1) %>%
  group_by(cause) %>%
  summarise(total_contribution = sum(contribution)) %>%
  arrange(-total_contribution)


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

decomp_f_narrow <- function(data){
  data <- data %>%
    mutate(diff_accidents = mx1_Accidents - mx2_Accidents,
           diff_bowelca = mx1_Bowel.cancer - mx2_Bowel.cancer, 
           diff_breastca = mx1_Breast.cancer - mx2_Breast.cancer, 
           diff_cerebrovasc = mx1_Cerebrovascular - mx2_Cerebrovascular, 
           diff_chd = mx1_CHD - mx2_CHD, 
           diff_CLD = mx1_Chronic.lower.respiratory.diseases - mx2_Chronic.lower.respiratory.diseases, 
           diff_liver = mx1_Cirrhosis.and.other.diseases.of.the.liver - mx2_Cirrhosis.and.other.diseases.of.the.liver,
           diff_dementia = mx1_Dementia.and.Alzheimers - mx2_Dementia.and.Alzheimers,
           diff_diabetes = mx1_Diabetes.mellitus - mx2_Diabetes.mellitus,
           diff_dig = mx1_Digestive.excluding.cirrhosis - mx2_Digestive.excluding.cirrhosis,
           diff_drug = mx1_Drug.related - mx2_Drug.related,
           diff_flu_pneum = mx1_Flu.and.pneumonia - mx2_Flu.and.pneumonia,
           diff_gu = mx1_Genitourinary - mx2_Genitourinary,
           diff_illdef = mx1_Ill.defined - mx2_Ill.defined,
           diff_inf = mx1_Infectious.diseases - mx2_Infectious.diseases,
           diff_lung_ca = mx1_Lung.cancer - mx2_Lung.cancer,
           diff_cns = mx1_Nervous.system.diseases.excluding.Alzheimers - mx2_Nervous.system.diseases.excluding.Alzheimers,
           diff_other = mx1_Other - mx2_Other,
           diff_other_canc = mx1_Other.cancer - mx2_Other.cancer, 
           diff_other_circ = mx1_Other.circulatory - mx2_Other.circulatory,
           diff_other_ext = mx1_Other.external - mx2_Other.external,
           diff_other_resp = mx1_Other.respiratory - mx2_Other.respiratory,
           diff_perinatal = mx1_Perinatal.conditions - mx2_Perinatal.conditions,
           diff_residual = mx1_Residual - mx2_Residual,
           diff_suicides = mx1_Suicides - mx2_Suicides,
           diff_mental = mx1_Mental.and.behavioural.disorders.excluding.dementia - mx2_Mental.and.behavioural.disorders.excluding.dementia,
           diff_all_cause = mx1_all_cause - mx2_all_cause,
           prop_accidents = diff_accidents/diff_all_cause,
           prop_bowelca = diff_bowelca/diff_all_cause,
           prop_breastca = diff_breastca/diff_all_cause,
           prop_cerebrovasc = diff_cerebrovasc/diff_all_cause,
           prop_chd  = diff_chd/diff_all_cause,
           prop_CLD = diff_CLD/diff_all_cause,
           prop_liver = diff_liver/diff_all_cause,
           prop_dementia = diff_dementia/diff_all_cause,
           prop_diabetes = diff_diabetes/diff_all_cause,
           prop_dig = diff_dig/diff_all_cause,
           prop_drug = diff_drug/diff_all_cause,
           prop_flu_pneum = diff_flu_pneum/diff_all_cause,
           prop_gu = diff_gu/diff_all_cause,
           prop_illdef = diff_illdef/diff_all_cause,
           prop_inf = diff_inf/diff_all_cause,          
           prop_lung_ca = diff_lung_ca/diff_all_cause,
           prop_cns = diff_cns/diff_all_cause,
           prop_other = diff_other/diff_all_cause,
           prop_other_canc = diff_other_canc/diff_all_cause,
           prop_other_circ = diff_other_circ/diff_all_cause,           
           prop_other_ext = diff_other_ext/diff_all_cause,
           prop_other_resp = diff_other_resp/diff_all_cause,
           prop_perinatal = diff_perinatal/diff_all_cause,
           prop_residual = diff_residual/diff_all_cause,
           prop_suicides = diff_suicides/diff_all_cause,
           prop_mental = diff_mental/diff_all_cause,
           contrib_accidents = prop_accidents*te,
           contrib_bowelca  = prop_bowelca*te,
           contrib_breastca = prop_breastca*te, 
           contrib_cerebrovasc = prop_cerebrovasc*te, 
           contrib_chd = prop_chd*te, 
           contrib_CLD = prop_CLD*te, 
           contrib_liver = prop_liver*te, 
           contrib_dementia = prop_dementia*te, 
           contrib_diabetes = prop_diabetes*te, 
           contrib_dig = prop_dig*te, 
           contrib_drug = prop_drug*te,
           contrib_flu_pneum = prop_flu_pneum*te, 
           contrib_gu = prop_gu*te, 
           contrib_illdef = prop_illdef*te, 
           contrib_inf = prop_inf*te, 
           contrib_lung_ca = prop_lung_ca*te, 
           contrib_cns = prop_cns*te, 
           contrib_other = prop_other*te, 
           contrib_other_canc = prop_other_canc*te, 
           contrib_other_circ = prop_other_circ,
           contrib_other_ext = prop_other_ext*te, 
           contrib_other_resp = prop_other_resp*te, 
           contrib_perinatal = prop_perinatal*te, 
           contrib_residual = prop_residual*te, 
           contrib_suicides = prop_suicides*te, 
           contrib_mental = prop_mental*te) %>%
    select(ageband, Year, contains("contrib"))
}

#Run decomposition code 

female_decomp <- cause_age_decomp_f %>%
  decomp_f_narrow()

#Convert to long format and recode cause data

female_decomp_long <- female_decomp %>%
  gather(cause, contribution, contrib_accidents:contrib_mental, 
         factor_key = TRUE)

#Explore

female_decomp_long %>%
  filter(Year == "2016-18") %>%
  mutate(contribution = contribution * -1) %>%
  group_by(cause) %>%
  summarise(total_contribution = sum(contribution)) %>%
  arrange(total_contribution) %>%
  adorn_totals("row") %>%
  write.table()

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

unique(female_decomp_long$cause)

female_decomp_long <- female_decomp_long %>%
  mutate(cause = fct_recode(cause,
                            Accidents = "contrib_accidents", 
                            Bowel_cancer = "contrib_bowelca", 
                            Breast_cancer = "contrib_breastca", 
                            Cerebrovascular = "contrib_cerebrovasc", 
                            IHD = "contrib_chd", 
                            Chronic_respiratory = "contrib_CLD", 
                            Liver = "contrib_liver", 
                            Dementia = "contrib_dementia", 
                            Diabetes = "contrib_diabetes", 
                            Digestive = "contrib_dig", 
                            Drug_Alcohol = "contrib_drug", 
                            Flu_Pneumonia = "contrib_flu_pneum", 
                            Genitourinary = "contrib_gu", 
                            Ill_defined = "contrib_illdef", 
                            Infectious_diseases = "contrib_inf", 
                            Lung_cancer = "contrib_lung_ca", 
                            Neurological = "contrib_cns", 
                            Other = "contrib_other", 
                            Other_cancers = "contrib_other_canc", 
                            Other_circulatory = "contrib_other_circ", 
                            Other_external = "contrib_other_ext", 
                            Other_respiratory = "contrib_other_resp", 
                            Perinatal = "contrib_perinatal", 
                            Residual = "contrib_residual", 
                            Suicides = "contrib_suicides", 
                            Mental_and_behavioural = "contrib_mental"
  ))

heat_f <- female_decomp_long %>%
  filter(Year == "2016-18") %>%
  mutate(contribution = contribution * -1) %>%
  ggplot(aes(ageband, y=reorder(cause, contribution))) + 
  geom_tile(aes(fill = contribution), width = 5) + 
  scale_fill_viridis()

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
  geom_bar(stat="identity", fill="purple", width = 4) + 
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        plot.margin = unit(c(0,0,0,0), "cm"), 
        axis.text.x = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(x=NULL, 
       y="Total contribution (years)", 
       title = "All causes") + 
  scale_x_continuous(labels = seq(0,90,5), 
                     breaks = seq(0,90,5), 
                     expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0))

age_f + guide_area() + heat_f + cause_f + plot_layout(ncol = 2, guides = "collect", 
                                                      widths = c(2,1), 
                                                      heights = c(1,2))

#Next for males

#Change factor levels for cause data

male_decomp_long <- male_decomp_long %>%
  mutate(cause = fct_recode(cause,
                            Accidents = "contrib_accidents", 
                            Bowel_cancer = "contrib_bowelca", 
                            Breast_cancer = "contrib_breastca", 
                            Cerebrovascular = "contrib_cerebrovasc", 
                            IHD = "contrib_chd", 
                            Chronic_respiratory = "contrib_CLD", 
                            Liver = "contrib_liver", 
                            Dementia = "contrib_dementia", 
                            Diabetes = "contrib_diabetes", 
                            Digestive = "contrib_dig", 
                            Drug_Alcohol = "contrib_drug", 
                            Flu_Pneumonia = "contrib_flu_pneum", 
                            Genitourinary = "contrib_gu", 
                            Ill_defined = "contrib_illdef", 
                            Infectious_diseases = "contrib_inf", 
                            Lung_cancer = "contrib_lung_ca", 
                            Neurological = "contrib_cns", 
                            Other = "contrib_other", 
                            Other_cancers = "contrib_other_canc", 
                            Other_circulatory = "contrib_other_circ", 
                            Other_external = "contrib_other_ext", 
                            Other_respiratory = "contrib_other_resp", 
                            Perinatal = "contrib_perinatal", 
                            Residual = "contrib_residual", 
                            Suicides = "contrib_suicides", 
                            Mental_and_behavioural = "contrib_mental"
  ))

heat_m <- male_decomp_long %>%
  filter(Year == "2016-18") %>%
  mutate(contribution = contribution * -1) %>%
  ggplot(aes(ageband, y=reorder(cause, contribution))) + 
  geom_tile(aes(fill = contribution), width = 5) + 
  scale_fill_viridis()

cause_m <- male_decomp_long %>%
  filter(Year == "2016-18") %>%
  mutate(contribution = contribution * -1) %>%
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
  geom_bar(stat="identity", fill="purple", width = 4) + 
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        plot.margin = unit(c(0,0,0,0), "cm"), 
        axis.text.x = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(x=NULL, 
       y="Total contribution (years)", 
       title = "All causes") + 
  scale_x_continuous(labels = seq(0,90,5), 
                     breaks = seq(0,90,5), 
                     expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0))

age_m + guide_area() + heat_m + cause_m + plot_layout(ncol = 2, guides = "collect", 
                                                      widths = c(2,1), 
                                                      heights = c(1,2))

#Tables of decomposition by age and cause for both genders for latest period
#Males

male_decomp_long %>%
  filter(Year == "2016-18") %>%
  mutate(contribution = contribution * -1) %>%
  group_by(cause) %>%
  summarise(total_contribution = sum(contribution)) %>%
  arrange(-total_contribution) %>%
  adorn_totals("row") %>%
  write.table("clipboard", sep="\t", row.names=F)

female_decomp_long %>%
  filter(Year == "2016-18") %>%
  mutate(contribution = contribution * -1) %>%
  group_by(cause) %>%
  summarise(total_contribution = sum(contribution)) %>%
  arrange(-total_contribution) %>%
  adorn_totals("row") %>%
  write.table("clipboard", sep="\t", row.names=F)

#Avoidable (amenable+avoidable) deaths may be useful sensitivity analysis to illustrate contribution from these causes from health services + policy interventions (or lack of), however ONS reported in 2018 these made up ~20% total deaths (likely mainly due to only counting <75 deaths)

#Given trends in rising mortality at older ages makes sense to stick with current decomp approach +/- refine

#Need to add in CIs based on bootstrap method

#Then decide if want to visualise change in life expectancy (and approach to this)

#Chart and table narrow cause data

male_decomp_long %>%
  filter(Year == "2016-18") %>%
  mutate(contribution = contribution * -1) %>%
  group_by(Year, cause) %>%
  summarise(cause_contrib = sum(contribution)) %>%
  ggplot(aes(x=reorder(cause, cause_contrib), y=cause_contrib)) + 
  geom_bar(fill="purple", stat="identity") + 
  coord_flip() + 
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        plot.margin = unit(c(0,0,0,0), "cm"), 
        axis.ticks.y = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(y="Total contribution (years)", 
       x=NULL, 
       title = "All ages") + 
  scale_y_continuous(expand = c(0,0)) + 
  geom_text(aes(label = round(cause_contrib, 2)))

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

heat_m_change <- male_change %>%
  mutate(change = change * -1) %>%
  ggplot(aes(ageband, y=reorder(cause, change))) + 
  geom_tile(aes(fill = change), width = 5) + 
  theme(panel.background = element_blank(), 
        axis.ticks = element_blank()) + 
  scale_fill_viridis(name = "Contribution to change in life expectancy gap (years)") + 
  scale_x_continuous(labels = seq(0,90,5), 
                     breaks = seq(0,90,5), 
                     expand = c(0,0)) + 
  labs(x=NULL, y=NULL, title=NULL, subtitle=NULL)

cause_m_change <- male_change %>%
  mutate(change = change * -1) %>%
  group_by(cause) %>%
  summarise(cause_change = sum(change)) %>%
  ggplot(aes(x=reorder(cause, cause_change), y=cause_change)) + 
  geom_bar(fill="purple", stat="identity") + 
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
  scale_y_continuous(expand = c(0,0))

age_m_change <- male_change %>%
  mutate(change = change * -1) %>%
  group_by(ageband) %>%
  summarise(te = sum(change)) %>%
  arrange(ageband) %>%
  ggplot(aes(x=ageband, y=te)) + 
  geom_bar(stat="identity", fill="purple", width = 4) + 
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        plot.margin = unit(c(0,0,0,0), "cm"), 
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust=0.5)) + 
  labs(x=NULL, 
       y="Total change (years)", 
       title = "All causes") + 
  scale_x_continuous(labels = seq(0,90,5), 
                     breaks = seq(0,90,5), 
                     expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0))

age_m_change + guide_area() + heat_m_change + cause_m_change + plot_layout(ncol = 2, guides = "collect", 
                                                                           widths = c(2,1), 
                                                                           heights = c(1,2)) + 
  plot_annotation(title = "Males", 
                  theme = theme(plot.title = element_text(size = 20)))

s#Graph same for females

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

heat_f_change <- female_change %>%
  mutate(change = change * -1) %>%
  ggplot(aes(ageband, y=reorder(cause, change))) + 
  geom_tile(aes(fill = change), width = 5) + 
  theme(panel.background = element_blank(), 
        axis.ticks = element_blank()) + 
  scale_fill_viridis(name = "Contribution to change in life expectancy gap (years)", 
                     breaks = c(-0.10, -0.05, 0, 0.05, 0.10),  
                     labels = c(-0.10, -0.05, 0, 0.05, 0.10)) + 
  scale_x_continuous(labels = seq(0,90,5), 
                     breaks = seq(0,90,5), 
                     expand = c(0,0)) + 
  labs(x=NULL, y=NULL, title=NULL, subtitle=NULL)

cause_f_change <- female_change %>%
  mutate(change = change * -1) %>%
  group_by(cause) %>%
  summarise(cause_change = sum(change)) %>%
  ggplot(aes(x=reorder(cause, cause_change), y=cause_change)) + 
  geom_bar(fill="purple", stat="identity") + 
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
  scale_y_continuous(expand = c(0,0))

age_f_change <- female_change %>%
  mutate(change = change * -1) %>%
  group_by(ageband) %>%
  summarise(te = sum(change)) %>%
  arrange(ageband) %>%
  ggplot(aes(x=ageband, y=te)) + 
  geom_bar(stat="identity", fill="purple", width = 4) + 
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        plot.margin = unit(c(0,0,0,0), "cm"), 
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust=0.5)) + 
  labs(x=NULL, 
       y="Total change (years)", 
       title = "All causes") + 
  scale_x_continuous(labels = seq(0,90,5), 
                     breaks = seq(0,90,5), 
                     expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0))

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
  write.table("clipboard", sep="\t", row.names=F)

female_change %>%
  mutate(change = change * -1) %>%
  group_by(cause) %>%
  summarise(tot_change = sum(change)) %>%
  arrange(-tot_change) %>%
  write.table("clipboard", sep="\t", row.names=F)
            