################################################################################
################## Study 1: Simulating Student Destributions ###################
################################################################################

#_______________________________________________________________________________
#----------------------- 1. LOAD PACKAGES AND DATA -----------------------------
# Data Wrangling
library(tidyverse)
# Geo Data Wrangling
library(sf)
library(ggspatial)

# Set working directory
setwd("H:/Daten/Bayern Daten")

# All special school students in 2020
SS <- read.csv2("Bayern Daten Original/Daten_aufbereitet/Förderschulen 2020.csv")
# All primary school students in 2020
PS <- read.csv2("Bayern Daten Original/Daten_aufbereitet/Grundschulen 2020.csv")
# All schools in 2010, 2015 and 2020
Geo <- read.csv2("Daten_Schulen.csv")
# Geodata of districts
district <- st_read("Geodaten/Verwaltungseinheiten Deutschland/vg250_ebenen_0101/VG250_KRS.shp")
# Geodata of Bavaria
bavaria <- st_read("Geodaten/Verwaltungseinheiten Deutschland/Bundesländer_Ohne_Wasser.shp")
# Coordinates of all schools
coord <- read.csv2("Daten_Schulen_Koordinaten.csv")
# Funding type of primary and special schools
funding <- read.csv("Schulen_Bayern_Modell_1_2_finanzierung.csv")

#_______________________________________________________________________________
#------------------------------- 2. DATA PREP ----------------------------------

# 2.0 Description:
# ----------------
# Data are accumulated into one dataframe, to show the actual situation of
# primary and special schools and their students in Bavaria 2020 (= baseline). 
# Data are wrangled for the following descriptive statistics of the baseline in 
# 2020 (3) and the simulation of different inclusive school systems (4).

# 2.1. Number of Students:
# ------------------------
## Number of students enrolled in every special school is calculated. We assume
## 50% of special school students to be in primary level. This simulation is
## focused on primary school --> 50% of special school students are selected.
n_students_SS <- data.frame(count(SS,Schulnummer),Type="SS") %>% 
  mutate(n = n/2)
## Number of students enrolled in every primary school (= school size) is 
## calculated.
n_students_PS <- data.frame(count(PS, Schulnummer), Type="PS")
## Numbers of primary and special school students are combined into one df.
n_students <- rbind(n_students_PS, n_students_SS)
## Former df are removed.
rm(n_students_PS, n_students_SS)

# 2.2. Number of Schools per District:
# ------------------------------------
## This pipe filters only primary and special schools existent in 2020.
## Private primary schools are deleted from the df based on ministerial info
## on the funding type of the primary schools.
Geo_new <- Geo %>%
  filter(Jahr=="2020") %>%
  filter(Schulart %in% c("Grundschulen", "Foerderzentren")) %>%
  left_join(select(funding, c(Schulnummer, school_description)), 
            by="Schulnummer") %>%
  filter(!(school_description == "privat" & Schulart == "Grundschulen"))
## Number of primary and special schools in Bavaria per district
n_schools_PS_SS <- Geo_new %>%  count(RS_Kreis, Schulart)
n_schools <- Geo_new %>%count(RS_Kreis)
## School numbers of governmental primary schools
numbers <- Geo_new %>%
  filter(Schulart=="Grundschulen") %>%
  select("Schulnummer")

# 2.3 Students with SEN in Primary Schools
# ----------------------------------------
## Calculation of students with SEN per primary school
SPF <- data.frame(count(filter(PS, MSD.Förderschwerpunkt=="Ja"), Schulnummer))

# 2.5 Coordinates of Schools
# --------------------------
coord <- coord %>% 
  filter(Jahr==2020) %>% 
  select(c(Schulnummer, lat, lon)) %>%
  unique()

# 2.6 Creating Baseline
# ---------------------
## This pipe combines all information (number of students per school, number of 
## students with SEN in primary schools and support rate of each primary school) 
## into one df to create the baseline (= model_0) of 2020. Further, the new df 
## is wrangled and prepared for simulations and calculations. Therefore, 
## remaining middle schools are deleted in the df, town and district structure 
## types are combined and the number of schools and primary schools per district 
## are calculated and NAs in the number of students with SEN per primary school 
## is set to 0. Unnecessary variables are deleted.
model_0 <- Geo_new %>%
  left_join(n_students, by="Schulnummer") %>%
  left_join(SPF, by="Schulnummer") %>%
  left_join(n_schools, by=c("RS_Kreis")) %>%
  left_join(n_schools_PS_SS, by=c("RS_Kreis", "Schulart")) %>%
  left_join(coord, by="Schulnummer") %>%
  select(-c(lat.x, lon.x)) %>%
  rename(n_students_school=n.x) %>%
  rename(n_students_SEN_primary=n.y) %>%
  rename(n_school_district=n.x.x) %>%
  rename(n_school_SS_PS_district=n.y.y) %>%
  rename(lat=lat.y) %>%
  rename(lon=lon.y) %>%
  filter(!is.na(Type)) %>%
  mutate(n_students_SEN_primary = ifelse(
    Schulart == "Grundschulen", 
    replace(n_students_SEN_primary, is.na(n_students_SEN_primary), 0), 
    n_students_SEN_primary)) %>%
  select(-c(Inklusion,GEN,Type,Inklusionsanteil_Kreis,FB_gesamt,FB_inklusiv))
## New information are calculated and added to the df: The number of students
## in (primary and special) schools per district (each in one column), the 
## number of students at SFZs per district and the proportional primary school 
## size per district = (n of students per school / n of all students per district)
model_0 <- model_0 %>% group_by(RS_Kreis) %>%
  mutate(n_students_district = sum(n_students_school)) %>%
  mutate(n_students_SS_district = sum(
    n_students_school[Schulart == "Foerderzentren"])) %>%
  mutate(n_students_PS_district = sum(
    n_students_school[Schulart == "Grundschulen"])) %>%
  mutate(SFZ = ifelse(Schultyp %in% c("ESE", "L", "Sprache", "SFZ"),1, 0)) %>%
  mutate(n_students_SFZ_district = sum(n_students_school[SFZ == 1])) %>%
  mutate(n_students_SEN_district = sum(n_students_SEN_primary, na.rm=T)) %>%
  mutate(n_students_noSEN_district = sum(
    n_students_school - n_students_SEN_primary, na.rm=T)) %>%
  mutate(prop_school_size_district = n_students_school/n_students_district) %>%
  mutate(prop_PS_size_district = n_students_school/n_students_PS_district) %>%
  ungroup() %>%
  mutate(prop_support_rate = (n_students_SEN_primary/n_students_school)*100)

## Save model_0
write.csv(model_0, "model_0.csv")

## Delete unnecessary objects and dfs
rm(coord, funding, Geo, n_schools, n_students, SPF)

#_______________________________________________________________________________
#----------------------------- 3. SIMULATIONS ----------------------------------

# 3.0 Description:
# ----------------
## model_0: current school system (baseline)
## model_1: no special schools
## - model_1a: inclusion in every local primary school
## - model_1b: inclusion in inclusive primary schools (20% SEN)
## model_2: no SFZs (readiness model)
## - model_2a: inclusion in every local primary school
## - model_2b: inclusion in inclusive primary schools (20% SEN)

model_0 <- read.csv("Neue Tabellen/Modelle/model_0.csv")

# 3.1. Simulation 1a:
# -------------------
## Students without SEN, students with SEN in primary schools and students with
## SEN in special schools are distributed to all primary schools. All Special 
## schools are closed. This pipe further calculates the number
## of students with SEN per primary school and the resulting support rate.
model_1a <- model_0 %>%
  group_by(RS_Kreis) %>%
  mutate(n_students_PS_add = n_students_SS_district*prop_PS_size_district) %>%
  mutate(prop_students_PS_add = (n_students_PS_add/n_students_school)*100) %>%
  mutate(n_students_PS_new = n_students_school + n_students_PS_add) %>%
  mutate(prop_support_rate_new = 
           ((n_students_SEN_primary + n_students_PS_add) / n_students_PS_new)
         *100) %>%
  filter(Schulart!="Foerderzentren")
## Hospital schools are not closed and again added to the dataframe.
x <- model_0 %>% filter(Schultyp == "Kranke")
model_1a <- model_1a %>% rbind(x)
  
# 3.2. Simulation 1b:
# -------------------
## Students with SEN in special schools are distributed to selected inclusive 
## primary schools. It is calculated, how many places each school would have to 
## add to reach 20% SEN rate: (a+x)/(b+x)=20% bzw. x = (0,2b - a) / 0,8 with x
## being the new places of schools. Schools with more than 20% SEN rate do not
## have to add places. All special schools are closed. Students with SEN are 
## distributed to large schools first then descending in school size until all
## students are distributed to inclusive schools.
model_1b <- model_0 %>%
  filter(Schulart!="Foerderzentren") %>%
  group_by(RS_Kreis) %>%
  arrange(desc(n_students_school)) %>%
  mutate(Rangordnung = row_number()) %>%
  mutate(n_new_students_school = 
    ((0.2*n_students_school)-n_students_SEN_primary) / 0.8) %>%
  mutate(n_new_students_school = ifelse(
    n_new_students_school < 0, 0, n_new_students_school)) %>%
  mutate(z=cumsum(n_new_students_school)) %>%
  mutate(z = ifelse(z>n_students_SS_district, NA, z)) %>%
  mutate(zmax = max(z, na.rm=T)) %>%
  mutate(n_new_students_school = ifelse(is.na(z), n_new_students_school==NA,
                                        n_new_students_school)) %>%
  mutate(n_new_students_school = ifelse(is.na(z) & !is.na(lag(z)), 
                                        n_students_SS_district-zmax, 
                                        n_new_students_school))
## Three districts have very few students at special schools. The pipe does
## not work for them, which is why those districts are calculated manually.
three_districts <- filter(model_1b, RS_Kreis %in% c(9472, 9475, 9778)) %>%
  mutate(n_new_students_school = ifelse(
    row_number() == 1, n_students_SS_district, n_new_students_school))
## The three districts now are again added to model_1b. This pipe further 
## calculates how large the schools are after the special school closures and
## how many inclusive special schools there would be per district.
model_1b <- model_1b %>%
  filter(!(RS_Kreis %in% c(9472, 9475, 9778))) %>%
  rbind(three_districts) %>%
  select(-c(z, zmax)) %>%
  mutate(n_students_PS_new = n_students_school + n_new_students_school)
## Support Rates are calculated
model_1b <- model_1b %>%
  mutate(prop_support_rate_new = 
           ((n_students_SEN_primary+n_new_students_school) / n_students_PS_new)
         *100) %>%
  mutate(prop_support_rate_new = ifelse(is.na(prop_support_rate_new), 
                                        prop_support_rate, 
                                        prop_support_rate_new))
## Hospital schools are not closed and again added to the dataframe.
x <- model_0 %>% filter(Schultyp == "Kranke")
model_1b <- model_1b %>% rbind(x)
  
# 3.1. Simulation 2a:
# -------------------
## Students without SEN, students with SEN in primary schools and students with
## SEN in SFZs are distributed to all primary schools. SFZs are closed. This 
## pipe further calculates the number of students with SEN per primary school 
## and the resulting support rate.
model_2a <- model_0 %>%
  group_by(RS_Kreis) %>%
  mutate(n_students_PS_add = n_students_SFZ_district*prop_PS_size_district) %>%
  mutate(prop_students_PS_add = (n_students_PS_add/n_students_school)*100) %>%
  mutate(n_students_PS_new = n_students_school + n_students_PS_add) %>%
  mutate(prop_support_rate_new = 
           ((n_students_SEN_primary + n_students_PS_add) / n_students_PS_new)
         *100) %>%
  filter(!(Schulart == "Foerderzentren" & SFZ == 1))
## Hospital schools are not closed and again added to the dataframe.
x <- model_0 %>% filter(Schultyp == "Kranke")
model_2a <- model_2a %>% rbind(x)

# 3.2. Simulation 2b:
# -------------------
## Students with SEN in special schools are distributed to selected inclusive 
## primary schools. It is calculated, how many places each school would have to 
## add to reach 20% SEN rate: (a+x)/(b+x)=20% bzw. x = (0,2b - a) / 0,8 with x
## being the new places of schools. Schools with more than 20% SEN rate do not
## have to add places. All special schools are closed. Students with SEN are 
## distributed to large schools first then descending in school size until all
## students are distributed to inclusive schools.
model_2b <- model_0 %>%
  filter(Schulart!="Foerderzentren") %>%
  group_by(RS_Kreis) %>%
  arrange(desc(n_students_school)) %>%
  mutate(Rangordnung = row_number()) %>%
  mutate(n_new_students_school = 
           ((0.2*n_students_school)-n_students_SEN_primary) / 0.8) %>%
  mutate(n_new_students_school = ifelse(
    n_new_students_school < 0, 0, n_new_students_school)) %>%
  mutate(z=cumsum(n_new_students_school)) %>%
  mutate(z = ifelse(z>n_students_SFZ_district, NA, z)) %>%
  mutate(zmax = max(z, na.rm=T)) %>%
  mutate(n_new_students_school = ifelse(is.na(z), n_new_students_school==NA,
                                        n_new_students_school)) %>%
  mutate(n_new_students_school = ifelse(is.na(z) & !is.na(lag(z)), 
                                        n_students_SFZ_district-zmax, 
                                        n_new_students_school))
## Seven districts have very few students at SFZs. The pipe does not work for 
## them, which is why those districts are calculated manually.
seven_districts <- filter(
  model_2b, RS_Kreis %in% c(9188, 9261, 9472, 9475, 9476, 9573, 9778)) %>%
  mutate(n_new_students_school = ifelse(
    row_number() == 1, n_students_SFZ_district, n_new_students_school))
## The three districts now are again added to model_1b. This pipe further 
## calculates how large the schools are after the special school closures and
## how many inclusive special schools there would be per district.
model_2b <- model_2b %>%
  filter(!(RS_Kreis %in% c(9188, 9261, 9472, 9475, 9476, 9573, 9778))) %>%
  rbind(seven_districts) %>%
  select(-c(z, zmax)) %>%
  mutate(n_students_PS_new = n_students_school + n_new_students_school)
## Support Rates are calculated
model_2b <- model_2b %>%
  mutate(prop_support_rate_new = 
           ((n_students_SEN_primary+n_new_students_school) / n_students_PS_new)
         *100) %>%
  mutate(prop_support_rate_new = ifelse(is.na(prop_support_rate_new), 
                                        prop_support_rate, 
                                        prop_support_rate_new))
## Other special schools were not closed and again added to the dataframe.
x <- model_0 %>% filter(Schulart == "Foerderzentren" & SFZ == 0)
model_2b <- model_2b %>% rbind(x)
## Hospital schools are not closed and again added to the dataframe.
x <- model_0 %>% filter(Schultyp == "Kranke")
model_2b <- model_2b %>% rbind(x)

#_______________________________________________________________________________

rm(bavaria, seven_districts, three_districts, x)
write.csv(model_1a, "Neue Tabellen/Modelle/model_1a.csv")
write.csv(model_1b, "Neue Tabellen/Modelle/model_1b.csv")
write.csv(model_2a, "Neue Tabellen/Modelle/model_2a.csv")
write.csv(model_2b, "Neue Tabellen/Modelle/model_2b.csv")

#_______________________________________________________________________________
#------------------------- 4. FIGURE 1: ALL BAVARIA ----------------------------

model_1a <- read.csv("Neue Tabellen/Modelle/model_1a.csv")
model_1b <- read.csv("Neue Tabellen/Modelle/model_1b.csv")
model_2a <- read.csv("Neue Tabellen/Modelle/model_2a.csv")
model_2b<- read.csv("Neue Tabellen/Modelle/model_2b.csv")

# 4.1 Plot 1: Bavarian Maps of Model 0
# ------------------------------------
## Map of Bavaria
bavaria <- bavaria %>% filter(GEN=="Bayern") %>% select(GEN)
## Points of special schools in Bavaria
bavaria_SS <- model_0 %>% ungroup() %>%
  filter(Schulart == "Foerderzentren") %>%
  select(SFZ, lat, lon)
## Differentiate special school types
bavaria_SS_SFZ <- filter(bavaria_SS, SFZ==1)
bavaria_SS_nSFZ <- filter(bavaria_SS, SFZ==0)
## Change coordinate format
bavaria_SS_SFZ <- st_as_sf(bavaria_SS_SFZ, coords = c("lon", "lat"), crs=4326)
bavaria_SS_nSFZ <- st_as_sf(bavaria_SS_nSFZ, coords = c("lon", "lat"), crs=4326)
## Plot the map
plot1 <- ggplot() +
  geom_sf(data = bavaria, fill = "grey95", color = "black") +
  geom_sf(data = bavaria_SS_SFZ, fill="grey", color="black", size = 1, shape=21) +
  geom_sf(data = bavaria_SS_nSFZ, fill="black", color="black", size = 1, shape=21) +
  theme_bw() +
  theme(legend.position = "none")
## Show plot1
plot1
ggsave("Abbildungen/figure1_1.png", plot1, width=7, height=7, units=c("cm"))
# Plot poster
ggplot() +
  geom_sf(data = bavaria, fill = "#F6F3F3", color = "black") +
  geom_sf(data = bavaria_SS_SFZ, fill="#6D6F6F", color="black", size = 2.5, shape=21) +
  geom_sf(data = bavaria_SS_nSFZ, fill="#FAD8DB", color="black", size = 2.5, shape=21) +
  theme_classic() +
  theme(legend.position = "none")

# 4.2 Plot 2: Bavarian Maps of Model 1a
# -------------------------------------
## Points of special schools in Bavaria
bavaria_SS <- model_1a %>% ungroup() %>%
  filter(Schulart == "Foerderzentren") %>%
  select(SFZ, lat, lon)
## Differentiate special school types
bavaria_SS_SFZ <- filter(bavaria_SS, SFZ==1)
bavaria_SS_nSFZ <- filter(bavaria_SS, SFZ==0)
## Change coordinate format
bavaria_SS_nSFZ <- st_as_sf(bavaria_SS_nSFZ, coords = c("lon", "lat"), crs=4326)
## Plot the map
plot2 <- ggplot() +
  geom_sf(data = bavaria, fill = "grey95", color = "black") +
  geom_sf(data = bavaria_SS_nSFZ, fill="black", color="black", size = 1, shape=21) +
  theme_bw() +
  theme(legend.position = "none")
## Show plot2
plot2
ggsave("Abbildungen/figure1_2.png", plot2, width=7, height=7, units=c("cm"))
# Plot poster
ggplot() +
  geom_sf(data = bavaria, fill = "#F6F3F3", color = "black") +
  geom_sf(data = bavaria_SS_nSFZ, fill="#FAD8DB", color="black", size = 2.5, shape=21) +
  theme_classic() +
  theme(legend.position = "none")

# 4.3 Plot 3: Bavarian Maps of Model 1b
# -------------------------------------
## Points of inclusive primary schools in Bavaria
bavaria_IPS <- model_1b %>% ungroup() %>%
  filter(Schulart == "Grundschulen") %>%
  filter(!is.na(n_students_PS_new)) %>%
  select(SFZ, lat, lon)
## Points of special schools in Bavaria
bavaria_SS <- model_1b %>% ungroup() %>%
  filter(Schulart == "Foerderzentren") %>%
  select(SFZ, lat, lon)
## Differentiate special school types
bavaria_SS_nSFZ <- filter(bavaria_SS, SFZ==0)
## Change coordinate format
bavaria_SS_nSFZ <- st_as_sf(bavaria_SS_nSFZ, coords = c("lon", "lat"), crs=4326)
bavaria_IPS <- st_as_sf(bavaria_IPS, coords = c("lon", "lat"), crs=4326)
## Plot the map
plot3 <- ggplot() +
  geom_sf(data = bavaria, fill = "grey95", color = "black") +
  geom_sf(data = bavaria_IPS, color="black", fill="white", size = 1, shape=24) +
  geom_sf(data = bavaria_SS_nSFZ, fill="black", color="black", size = 1, shape=21) +
  theme_minimal() +
  theme(legend.position = "none")
## Show plot3
plot3
ggsave("Abbildungen/figure1_3.png", plot3, width=7, height=7, units=c("cm"))
## Plot poster
ggplot() +
  geom_sf(data = bavaria, fill = "#F6F3F3", color = "black") +
  geom_sf(data = bavaria_IPS, fill="black", color="black",size = 2.5, shape=21) +
  geom_sf(data = bavaria_SS_SFZ, fill="#6D6F6F", color="black", size = 2.5, shape=21) +
  geom_sf(data = bavaria_SS_nSFZ, fill="#FAD8DB", color="black", size = 2.5, shape=21) +
  theme_classic() +
  theme(legend.position = "none")

# 4.4 Plot 4: Bavarian Maps of Model 2a
# -------------------------------------
## Points of special schools in Bavaria
bavaria_SS <- model_2a %>% ungroup() %>%
  filter(Schulart == "Foerderzentren") %>%
  select(SFZ, lat, lon)
## Differentiate special school types
bavaria_SS_SFZ <- filter(bavaria_SS, SFZ==1)
bavaria_SS_nSFZ <- filter(bavaria_SS, SFZ==0)
## Change coordinate format
bavaria_SS_nSFZ <- st_as_sf(bavaria_SS_nSFZ, coords = c("lon", "lat"), crs=4326)
## Plot the map
plot4 <- ggplot() +
  geom_sf(data = bavaria, fill = "grey95", color = "black") +
  geom_sf(data = bavaria_SS_nSFZ, fill="black", color="black", size = 1, shape=21) +
  theme_bw() +
  theme(legend.position = "none")
## Show plot4
plot4
ggsave("Abbildungen/figure1_4.png", plot4, width=7, height=7, units=c("cm"))
## Plot poster
ggplot() +
  geom_sf(data = bavaria, fill = "#F6F3F3", color = "black") +
  geom_sf(data = bavaria_SS_SFZ, fill="#6D6F6F", color="black", size = 2.5, shape=21) +
  geom_sf(data = bavaria_SS_nSFZ, fill="#FAD8DB", color="black", size = 2.5, shape=21) +
  theme_classic() +
  theme(legend.position = "none")

# 4.5 Plot 5: Bavarian Maps of Model 2b
# -------------------------------------
## Points of inclusive primary schools in Bavaria
bavaria_IPS <- model_2b %>% ungroup() %>%
  filter(Schulart == "Grundschulen") %>%
  filter(!is.na(n_students_PS_new)) %>%
  select(SFZ, lat, lon)
## Points of special schools in Bavaria
bavaria_SS <- model_2b %>% ungroup() %>%
  filter(Schulart == "Foerderzentren") %>%
  select(SFZ, lat, lon)
## Differentiate special school types
bavaria_SS_nSFZ <- filter(bavaria_SS, SFZ==0)
## Change coordinate format
bavaria_SS_nSFZ <- st_as_sf(bavaria_SS_nSFZ, coords = c("lon", "lat"), crs=4326)
bavaria_IPS <- st_as_sf(bavaria_IPS, coords = c("lon", "lat"), crs=4326)
## Plot the map
plot5 <- ggplot() +
  geom_sf(data = bavaria, fill = "grey95", color = "black") +
  geom_sf(data = bavaria_IPS, color="black", fill="white", size = 1, shape=24) +
  geom_sf(data = bavaria_SS_nSFZ, fill="black", color="black", size = 1, shape=21) +
  theme_minimal() +
  theme(legend.position = "none")
## Show plot5
plot5
ggsave("Abbildungen/figure1_5.png", plot5, width=7, height=7, units=c("cm"))
## Plot poster
ggplot() +
  geom_sf(data = bavaria, fill = "#F6F3F3", color = "black") +
  geom_sf(data = bavaria_IPS, fill="black", color="black",size = 2.5, shape=21) +
  geom_sf(data = bavaria_SS_SFZ, fill="#6D6F6F", color="black", size = 2.5, shape=21) +
  geom_sf(data = bavaria_SS_nSFZ, fill="#FAD8DB", color="black", size = 2.5, shape=21) +
  theme_classic() +
  theme(legend.position = "none")

#_______________________________________________________________________________
#------------------- 5. FIGURE 2: URBAN AND RURAL MODEL A ----------------------

# 5.1 Plot 6: Munich Map of Model 0
# ---------------------------------
## Map of Munich
munich <- district %>% 
  filter(GEN=="München" & BEZ =="Kreisfreie Stadt") %>% select(GEN)
## Points of primary schools in Munich
munich_PS <- model_0 %>% 
  filter(RS_Kreis == 9162) %>%
  filter(Schulart == "Grundschulen") %>%
  select(lat, lon) %>%
  mutate(lat = str_remove_all(as.character(lat), "\\."),
         lon = str_remove_all(as.character(lon), "\\.")) %>%
  mutate(lat = str_replace(lat, "(^\\d{2})", "\\1."),
         lon = str_replace(lon, "(^\\d{2})", "\\1."))
## Points of special schools in Munich
munich_SS <- model_0 %>% 
  filter(RS_Kreis == 9162) %>% 
  filter(Schulart == "Foerderzentren")
munich_SS_SFZ <- filter(munich_SS, SFZ==1) %>%
  select(lat, lon) %>%
  mutate(lat = str_remove_all(as.character(lat), "\\."),
         lon = str_remove_all(as.character(lon), "\\.")) %>%
  mutate(lat = str_replace(lat, "(^\\d{2})", "\\1."),
         lon = str_replace(lon, "(^\\d{2})", "\\1."))
munich_SS_nSFZ <- filter(munich_SS, SFZ==0) %>%
  select(lat, lon) %>%
  mutate(lat = str_remove_all(as.character(lat), "\\."),
         lon = str_remove_all(as.character(lon), "\\.")) %>%
  mutate(lat = str_replace(lat, "(^\\d{2})", "\\1."),
         lon = str_replace(lon, "(^\\d{2})", "\\1."))
## Change coordinate format
munich_PS <- st_as_sf(munich_PS, coords = c("lon", "lat"), crs=4326)
munich_SS_SFZ <- st_as_sf(munich_SS_SFZ, coords = c("lon", "lat"), crs=4326)
munich_SS_nSFZ <- st_as_sf(munich_SS_nSFZ, coords = c("lon", "lat"), crs=4326)
## Plot the map
plot6 <- ggplot() +
  geom_sf(data = munich, fill = "grey95", color = "black") +
  geom_sf(data = munich_PS, color="grey", fill="white", size = 1, shape=24) +
  geom_sf(data = munich_SS_SFZ, fill="grey", color="black", size = 1, shape=21) +
  geom_sf(data = munich_SS_nSFZ, fill="black", color="black", size = 1, shape=21) +
  theme_bw() +
  theme(legend.position = "none")
## Show plot6
plot6
ggsave("Abbildungen/figure2_1.png", plot6, width=7, height=7, units=c("cm"))

# 5.2 Plot 7: Munich Maps of Model 1a
# -----------------------------------
## Points of primary schools in Munich
munich_PS <- model_1a %>% 
  filter(RS_Kreis == 9162) %>%
  filter(Schulart == "Grundschulen") %>%
  select(lat, lon) %>%
  mutate(lat = str_remove_all(as.character(lat), "\\."),
         lon = str_remove_all(as.character(lon), "\\.")) %>%
  mutate(lat = str_replace(lat, "(^\\d{2})", "\\1."),
         lon = str_replace(lon, "(^\\d{2})", "\\1."))
## Points of special schools in Munich
munich_SS <- model_1a %>% 
  filter(RS_Kreis == 9162) %>% 
  filter(Schulart == "Foerderzentren")
munich_SS_nSFZ <- filter(munich_SS, SFZ==0) %>%
  select(lat, lon) %>%
  mutate(lat = str_remove_all(as.character(lat), "\\."),
         lon = str_remove_all(as.character(lon), "\\.")) %>%
  mutate(lat = str_replace(lat, "(^\\d{2})", "\\1."),
         lon = str_replace(lon, "(^\\d{2})", "\\1."))
## Change coordinate format
munich_PS <- st_as_sf(munich_PS, coords = c("lon", "lat"), crs=4326)
munich_SS_nSFZ <- st_as_sf(munich_SS_nSFZ, coords = c("lon", "lat"), crs=4326)
## Plot the map
plot7 <- ggplot() +
  geom_sf(data = munich, fill = "grey95", color = "black") +
  geom_sf(data = munich_PS, color="grey", fill="white", size = 1, shape=24) +
  geom_sf(data = munich_SS_nSFZ, fill="black", color="black", size = 1, shape=21) +
  theme_bw() +
  theme(legend.position = "none")
## Show plot7
plot7
ggsave("Abbildungen/figure2_2.png", plot7, width=7, height=7, units=c("cm"))

# 4.7 Plot 7: Munich Maps of Model 1b
# -----------------------------------
## Points of primary schools in Munich
munich_PS <- model_1b %>% 
  filter(RS_Kreis == 9162) %>%
  filter(Schulart == "Grundschulen") %>%
  select(lat, lon) %>%
  mutate(lat = str_remove_all(as.character(lat), "\\."),
         lon = str_remove_all(as.character(lon), "\\.")) %>%
  mutate(lat = str_replace(lat, "(^\\d{2})", "\\1."),
         lon = str_replace(lon, "(^\\d{2})", "\\1."))
## Points of inclusive primary schools in Munich
munich_IPS <- model_1b %>% 
  filter(RS_Kreis == 9162) %>%
  filter(Schulart == "Grundschulen") %>%
  filter(!is.na(n_students_PS_new)) %>%
  select(lat, lon) %>%
  mutate(lat = str_remove_all(as.character(lat), "\\."),
         lon = str_remove_all(as.character(lon), "\\.")) %>%
  mutate(lat = str_replace(lat, "(^\\d{2})", "\\1."),
         lon = str_replace(lon, "(^\\d{2})", "\\1."))
## Points of special schools in Munich
munich_SS <- model_1b %>% 
  filter(RS_Kreis == 9162) %>% 
  filter(Schulart == "Foerderzentren")
munich_SS_nSFZ <- filter(munich_SS, SFZ==0) %>%
  select(lat, lon) %>%
  mutate(lat = str_remove_all(as.character(lat), "\\."),
         lon = str_remove_all(as.character(lon), "\\.")) %>%
  mutate(lat = str_replace(lat, "(^\\d{2})", "\\1."),
         lon = str_replace(lon, "(^\\d{2})", "\\1."))
## Change coordinate format
munich_PS <- st_as_sf(munich_PS, coords = c("lon", "lat"), crs=4326)
munich_IPS <- st_as_sf(munich_IPS, coords = c("lon", "lat"), crs=4326)
munich_SS_nSFZ <- st_as_sf(munich_SS_nSFZ, coords = c("lon", "lat"), crs=4326)
## Plot the map
plot8 <- ggplot() +
  geom_sf(data = munich, fill = "grey95", color = "black") +
  geom_sf(data = munich_PS, color="grey", fill="white", size = 1, shape=24) +
  geom_sf(data = munich_IPS, color="black", fill="white", size = 1, shape=24) +
  geom_sf(data = munich_SS_nSFZ, fill="black", color="black", size = 1, shape=21) +
  theme_bw() +
  theme(legend.position = "none")
## Show plot8
plot8
ggsave("Abbildungen/figure2_3.png", plot8, width=7, height=7, units=c("cm"))

# 5.4 Plot 9: Regen Map of Model 0
# --------------------------------
## Map of Regen
regen <- district %>% filter(GEN=="Regen") %>% select(GEN)
## Points of primary schools in Regen
regen_PS <- model_0 %>% 
  filter(RS_Kreis == 9276) %>%
  filter(Schulart == "Grundschulen") %>%
  select(lat, lon) %>%
  mutate(lat = str_remove_all(as.character(lat), "\\."),
         lon = str_remove_all(as.character(lon), "\\.")) %>%
  mutate(lat = str_replace(lat, "(^\\d{2})", "\\1."),
         lon = str_replace(lon, "(^\\d{2})", "\\1."))
## Points of special schools in Regen
regen_SS <- model_0 %>% 
  filter(RS_Kreis == 9276) %>% 
  filter(Schulart == "Foerderzentren")
regen_SS_SFZ <- filter(regen_SS, SFZ==1) %>%
  select(lat, lon) %>%
  mutate(lat = str_remove_all(as.character(lat), "\\."),
         lon = str_remove_all(as.character(lon), "\\.")) %>%
  mutate(lat = str_replace(lat, "(^\\d{2})", "\\1."),
         lon = str_replace(lon, "(^\\d{2})", "\\1."))
regen_SS_nSFZ <- filter(regen_SS, SFZ==0) %>%
  select(lat, lon) %>%
  mutate(lat = str_remove_all(as.character(lat), "\\."),
         lon = str_remove_all(as.character(lon), "\\.")) %>%
  mutate(lat = str_replace(lat, "(^\\d{2})", "\\1."),
         lon = str_replace(lon, "(^\\d{2})", "\\1."))
## Change coordinate format
regen_PS <- st_as_sf(regen_PS, coords = c("lon", "lat"), crs=4326)
regen_SS_SFZ <- st_as_sf(regen_SS_SFZ, coords = c("lon", "lat"), crs=4326)
regen_SS_nSFZ <- st_as_sf(regen_SS_nSFZ, coords = c("lon", "lat"), crs=4326)
## Plot the map
plot9 <- ggplot() +
  geom_sf(data = regen, fill = "grey95", color = "black") +
  geom_sf(data = regen_PS, color="grey", fill="white", size = 1, shape=24) +
  geom_sf(data = regen_SS_SFZ, fill="grey", color="black", size = 1, shape=21) +
  geom_sf(data = regen_SS_nSFZ, fill="black", color="black", size = 1, shape=21) +
  theme_bw() +
  theme(legend.position = "none")
## Show plot9
plot9
ggsave("Abbildungen/figure2_4.png", plot9, width=7, height=7, units=c("cm"))

# 5.5 Plot 10: Regen Map of Model 1a
# ----------------------------------
## Points of primary schools in Regen
regen_PS <- model_1a %>% 
  filter(RS_Kreis == 9276) %>%
  filter(Schulart == "Grundschulen") %>%
  select(lat, lon) %>%
  mutate(lat = str_remove_all(as.character(lat), "\\."),
         lon = str_remove_all(as.character(lon), "\\.")) %>%
  mutate(lat = str_replace(lat, "(^\\d{2})", "\\1."),
         lon = str_replace(lon, "(^\\d{2})", "\\1."))
## Points of special schools in Regen
regen_SS <- model_1a %>% 
  filter(RS_Kreis == 9276) %>% 
  filter(Schulart == "Foerderzentren")
regen_SS_nSFZ <- filter(regen_SS, SFZ==0) %>%
  select(lat, lon) %>%
  mutate(lat = str_remove_all(as.character(lat), "\\."),
         lon = str_remove_all(as.character(lon), "\\.")) %>%
  mutate(lat = str_replace(lat, "(^\\d{2})", "\\1."),
         lon = str_replace(lon, "(^\\d{2})", "\\1."))
## Change coordinate format
regen_PS <- st_as_sf(regen_PS, coords = c("lon", "lat"), crs=4326)
regen_SS_nSFZ <- st_as_sf(regen_SS_nSFZ, coords = c("lon", "lat"), crs=4326)
## Plot the map
plot10 <- ggplot() +
  geom_sf(data = regen, fill = "grey95", color = "black") +
  geom_sf(data = regen_PS, color="grey", fill="white", size = 1, shape=24) +
  geom_sf(data = regen_SS_nSFZ, fill="black", color="black", size = 1, shape=21) +
  theme_bw() +
  theme(legend.position = "none")
## Show plot10
plot10
ggsave("Abbildungen/figure2_5.png", plot10, width=7, height=7, units=c("cm"))

# 5.7 Plot 11: Regen Maps of Model 1b
# -----------------------------------
## Points of primary schools in Regen
regen_PS <- model_1b %>% 
  filter(RS_Kreis == 9276) %>%
  filter(Schulart == "Grundschulen") %>%
  select(lat, lon) %>%
  mutate(lat = str_remove_all(as.character(lat), "\\."),
         lon = str_remove_all(as.character(lon), "\\.")) %>%
  mutate(lat = str_replace(lat, "(^\\d{2})", "\\1."),
         lon = str_replace(lon, "(^\\d{2})", "\\1."))
## Points of inclusive primary schools in Regen
regen_IPS <- model_1b %>% 
  filter(RS_Kreis == 9276) %>%
  filter(Schulart == "Grundschulen") %>%
  filter(!is.na(n_students_PS_new)) %>%
  select(lat, lon) %>%
  mutate(lat = str_remove_all(as.character(lat), "\\."),
         lon = str_remove_all(as.character(lon), "\\.")) %>%
  mutate(lat = str_replace(lat, "(^\\d{2})", "\\1."),
         lon = str_replace(lon, "(^\\d{2})", "\\1."))
## Change coordinate format
regen_PS <- st_as_sf(regen_PS, coords = c("lon", "lat"), crs=4326)
regen_IPS <- st_as_sf(regen_IPS, coords = c("lon", "lat"), crs=4326)
## Plot the map
plot11 <- ggplot() +
  geom_sf(data = regen, fill = "grey95", color = "black") +
  geom_sf(data = regen_PS, color="grey", fill="white", size = 1, shape=24) +
  geom_sf(data = regen_IPS, color="black", fill="white", size = 1, shape=24) +
  theme_bw() +
  theme(legend.position = "none")
## Show plot11
plot11
ggsave("Abbildungen/figure2_6.png", plot11, width=7, height=7, units=c("cm"))

# remove unnecessay objects
rm(bavaria, bavaria_SS, bavaria_SS_nSFZ, bavaria_SS_SFZ, munich, munich_PS,
   munich_SS, munich_SS_nSFZ, munich_SS_SFZ, plot1, plot2, plot3, plot4, plot5,
   plot6, plot7, plot8, regen, regen_PS, regen_SS, regen_SS_nSFZ, regen_SS_SFZ,
   regen_IPS, munich_IPS, plot10, plot11, plot9, legend, district, bavaria_IPS)

#_______________________________________________________________________________
#------------------------------- 6. ANALYSIS -----------------------------------
model_0 <- read.csv("Neue Tabellen/Modelle/model_0.csv")
model_1a <- read.csv("Neue Tabellen/Modelle/model_1a.csv")
model_1b <- read.csv("Neue Tabellen/Modelle/model_1b.csv")
model_2a <- read.csv("Neue Tabellen/Modelle/model_2a.csv")
model_2b <- read.csv("Neue Tabellen/Modelle/model_2b.csv")

# 6.1. Number of schools and districts
# ------------------------------------
## Recoding of district type for further analysis
model_0 <- model_0 %>% mutate(SN_KTYP4_new = case_when(
  SN_KTYP4 %in% c(1, 2) ~ 1, SN_KTYP4 %in% c(3, 4) ~ 3))
model_1a <- model_1a %>% mutate(SN_KTYP4_new = case_when(
  SN_KTYP4 %in% c(1, 2) ~ 1, SN_KTYP4 %in% c(3, 4) ~ 3))
model_1b <- model_1b %>% mutate(SN_KTYP4_new = case_when(
  SN_KTYP4 %in% c(1, 2) ~ 1, SN_KTYP4 %in% c(3, 4) ~ 3))
model_2a <- model_2a %>% mutate(SN_KTYP4_new = case_when(
  SN_KTYP4 %in% c(1, 2) ~ 1, SN_KTYP4 %in% c(3, 4) ~ 3))
model_2b <- model_2b %>% mutate(SN_KTYP4_new = case_when(
  SN_KTYP4 %in% c(1, 2) ~ 1, SN_KTYP4 %in% c(3, 4) ~ 3))
## Number of districts, district types and schools and students
nrow(unique(select(model_0, RS_Kreis)))
table(unique(select(model_0, SN_KTYP4_new, RS_Kreis))$SN_KTYP4_new)
table(unique(select(model_0, SN_KTYP4_new, Schulnummer, Schulart))$SN_KTYP4_new, 
      unique(select(model_0, SN_KTYP4_new, Schulnummer, Schulart))$Schulart)
sum(filter(model_0, SN_KTYP4_new == 1 & Schulart == "Grundschulen")$n_students_school)
sum(filter(model_0, SN_KTYP4_new == 3 & Schulart == "Grundschulen")$n_students_school)
sum(filter(model_0, SN_KTYP4_new == 1 & Schulart == "Foerderzentren")$n_students_school)
sum(filter(model_0, SN_KTYP4_new == 3 & Schulart == "Foerderzentren")$n_students_school)
sum(filter(model_0, SN_KTYP4_new == 1)$n_students_school)
sum(filter(model_0, SN_KTYP4_new == 3)$n_students_school)
## Number of schools in the models
nrow(filter(model_0, Schulart=="Grundschulen"))
nrow(filter(model_1a, Schulart=="Grundschulen"))
nrow(filter(model_2a, Schulart=="Grundschulen"))
nrow(filter(model_0, Schulart=="Foerderzentren"))
nrow(filter(model_1a, Schulart=="Foerderzentren"))
nrow(filter(model_2a, Schulart=="Foerderzentren"))
nrow(filter(model_1b, Schulart=="Grundschulen" & !is.na(n_students_PS_new)))
nrow(filter(model_2b, Schulart=="Grundschulen" & !is.na(n_students_PS_new)))
# Student numbers of inclusive primary schools
x <- filter(model_1b, Schulart=="Grundschulen" & !is.na(n_students_PS_new))
summary(x$prop_support_rate_new)
nrow(filter(x, prop_support_rate_new < 20))
x <- filter(model_2b, Schulart=="Grundschulen" & !is.na(n_students_PS_new))
summary(x$prop_support_rate_new)
nrow(filter(x, prop_support_rate_new < 20))

# 6.2. Preparing data frame for statistical analysis of school size
# -----------------------------------------------------------------
## Selection of new number of students per primary school and number of added
## students per primary school. Joining all models into one data frame. Data
## frame is cleaned as special schools are removed from data frame. 
stat_students <- model_0 %>% ungroup() %>%
  select(Schulnummer, Schulart, SN_KTYP4_new, n_students_school) %>%
  left_join(model_1a %>% ungroup() %>% 
              select(c(Schulnummer, n_students_PS_new, n_students_PS_add)), 
            by="Schulnummer") %>%
  left_join(model_1b %>% ungroup() %>% 
              select(c(Schulnummer, n_students_PS_new, n_new_students_school)), 
            by="Schulnummer") %>%
  left_join(model_2a %>% ungroup() %>% 
              select(c(Schulnummer, n_students_PS_new, n_students_PS_add)), 
            by="Schulnummer") %>%
  left_join(model_2b %>% ungroup() %>% 
              select(c(Schulnummer, n_students_PS_new, n_new_students_school)), 
            by="Schulnummer") %>%
  filter(!Schulart=="Foerderzentren") 
## Renaming the variables based on content and model
names(stat_students) <- c(
  "school", "type", "structure", "n_model_0", "n_model_1a", "n_add_model_1a", 
  "n_model_1b", "n_add_model_1b", "n_model_2a", "n_add_model_2a", "n_model_2b", 
  "n_add_model_2b")
## Recoding the B-variables: NA of absolute student numbers in gets to be the 
## absolute student number of model 0, as there is no difference. NA in the
## variables of absolute student increase gets to be 0, as there is no increase
stat_students <- stat_students %>%
  mutate(n_model_1b = ifelse(is.na(n_model_1b), n_model_0, n_model_1b),
         n_model_2b = ifelse(is.na(n_model_2b), n_model_0, n_model_2b),
         n_add_model_1b = ifelse(is.na(n_add_model_1b), 0, n_add_model_1b),
         n_add_model_2b = ifelse(is.na(n_add_model_2b), 0, n_add_model_2b))
## Calculation of percentual increase in student numbers per model
stat_students <- stat_students %>%
  mutate(p_add_model_1a = (n_add_model_1a/n_model_0)*100,
         p_add_model_1b = (n_add_model_1b/n_model_0)*100,
         p_add_model_2a = (n_add_model_2a/n_model_0)*100,
         p_add_model_2b = (n_add_model_2b/n_model_0)*100)
## Save data frame for student statistics
write.csv(stat_students, "Neue Tabellen/stat_students.csv")

# 6.2. Analysis of school sizes
# -----------------------------
stat_students <- read.csv("Neue Tabellen/stat_students.csv")
# Model 0
summary(stat_students$n_model_0)
sd(stat_students$n_model_0)
# Model 1a
summary(stat_students$n_add_model_1a)
sd(stat_students$n_add_model_1a)
summary(stat_students$p_add_model_1a)
sd(stat_students$p_add_model_1a)
summary(stat_students$n_model_1a)
sd(stat_students$n_model_1a)
# Model 1b
summary(stat_students$n_add_model_1b)
sd(stat_students$n_add_model_1b)
summary(stat_students$p_add_model_1b)
sd(stat_students$p_add_model_1b)
summary(stat_students$n_model_1b)
sd(stat_students$n_model_1b)
# Model 2a
summary(stat_students$n_add_model_2a)
sd(stat_students$n_add_model_2a)
summary(stat_students$p_add_model_2a)
sd(stat_students$p_add_model_2a)
summary(stat_students$n_model_2a)
sd(stat_students$n_model_2a)
# Model 2b
summary(stat_students$n_add_model_2b)
sd(stat_students$n_add_model_2b)
summary(stat_students$p_add_model_2b)
sd(stat_students$p_add_model_2b)
summary(stat_students$n_model_2b)
sd(stat_students$n_model_2b)
# Inclusive Primary Schools
x <- filter(stat_students, n_add_model_1b!=0)
summary(x$n_add_model_1b)
sd(x$n_add_model_1b)
x <- filter(stat_students, n_add_model_2b!=0)
summary(x$n_add_model_2b)
sd(x$n_add_model_2b)

# 6.3. Analysis of Variance for school sizes
# ------------------------------------------
## Are there significant urban - rural differences in the student increase?
### Select relevant variables and convert to long format
x <- stat_students %>% select(structure, starts_with("p_add")) %>% 
  pivot_longer(cols=2:5, names_to ="model", values_to = "p")
### t-test between urban and rural for every model
t.test(p ~ structure, data=filter(x, model=="p_add_model_1a")) # t(1713.2) = 4.926, p < .001
t.test(p ~ structure, data=filter(x, model=="p_add_model_1b")) # no sig.
t.test(p ~ structure, data=filter(x, model=="p_add_model_2a")) # no sig.
t.test(p ~ structure, data=filter(x, model=="p_add_model_2b")) # no sig.

## Are there significant differences in the school sizes between the models?
### Select relevant variables and convert to long format
x <- stat_students %>% select(structure, starts_with("n_m")) %>% 
  pivot_longer(cols=2:6, names_to ="model", values_to = "n")
### ANOVA between models for urban and rural areas
summary(aov(n~model, data=x)) # **
summary(aov(n~model, data=filter(x, structure==1))) # no sig.
summary(aov(n~model, data=filter(x, structure==3))) # no sig.
### Posthoc-Test between models for urban and rural areas
TukeyHSD(aov(n~model, data=x))
TukeyHSD(aov(n~model, data=filter(x, structure==1))) # no sig.
TukeyHSD(aov(n~model, data=filter(x, structure==3))) # no sig.

# 4.4. Preparing data frame for statistical analysis of support rate
# ------------------------------------------------------------------
## Explanation
stat_support <- model_0 %>% ungroup() %>%
  select(Schulnummer, Schulart, SN_KTYP4_new, prop_support_rate) %>%
  left_join(model_1a %>% ungroup() %>% 
              select(c(Schulnummer, prop_support_rate_new)), 
            by="Schulnummer") %>%
  left_join(model_1b %>% ungroup() %>% 
              select(c(Schulnummer, prop_support_rate_new)), 
            by="Schulnummer") %>%
  left_join(model_2a %>% ungroup() %>% 
              select(c(Schulnummer, prop_support_rate_new)), 
            by="Schulnummer") %>%
  left_join(model_2b %>% ungroup() %>% 
              select(c(Schulnummer, prop_support_rate_new)), 
            by="Schulnummer") %>%
  filter(!Schulart=="Foerderzentren") 
## Renaming the variables based on content and model
names(stat_support) <- c("school", "type", "structure", "model_0", "model_1a", 
                          "model_1b", "model_2a", "model_2b")

stat_support <- stat_support %>%
  mutate(p_model_1a = model_1a - model_0,
         p_model_1b = model_1b - model_0,
         p_model_2a = model_2a - model_0,
         p_model_2b = model_2b - model_0)

# Model 0
summary(stat_support$model_0)
sd(stat_support$model_0)
# Model 1a
summary(stat_support$model_1a)
sd(stat_support$model_1a)
# Model 2a
summary(stat_support$model_2a)
sd(stat_support$model_2a)
# Model 1b
summary(stat_support$model_1b)
sd(stat_support$model_1b)
# Model 2b
summary(stat_support$model_2b)
sd(stat_support$model_2b)

# 4.5. Analysis of Variance for support rate
# ------------------------------------------
## Are there significant urban - rural differences in the increase of support?
### Select relevant variables and convert to long format
x <- stat_support %>% select(structure, starts_with("p_m")) %>% 
  pivot_longer(cols=2:5, names_to ="model", values_to = "p")
### t-test between urban and rural for every model
t.test(p ~ structure, data=filter(x, model=="p_model_1a")) #***
t.test(p ~ structure, data=filter(x, model=="p_model_1b")) 
t.test(p ~ structure, data=filter(x, model=="p_model_2a")) 
t.test(p ~ structure, data=filter(x, model=="p_model_2b")) 

## Are there significant differences in the support rate between the models?
### Select relevant variables and convert to long format
x <- stat_support %>% select(structure, starts_with("model")) %>% 
  pivot_longer(cols=2:6, names_to ="model", values_to = "n")
### ANOVA between models for urban and rural areas
summary(aov(n~model, data=x)) # 
summary(aov(n~model, data=filter(x, structure==1))) # 
summary(aov(n~model, data=filter(x, structure==3))) # 
t.test(n ~ structure, data=filter(x, model=="model_0"))
t.test(n ~ structure, data=filter(x, model=="model_1a")) #***
t.test(n ~ structure, data=filter(x, model=="model_1b")) 
t.test(n ~ structure, data=filter(x, model=="model_2a")) 
t.test(n ~ structure, data=filter(x, model=="model_2b")) 
### Posthoc-Test between models for urban and rural areas
TukeyHSD(aov(n~model, data=x)) #
TukeyHSD(aov(n~model, data=filter(x, structure==1))) # 
TukeyHSD(aov(n~model, data=filter(x, structure==3))) # 

# 4.6. Preparing data for travel distance analysis
# ------------------------------------------------
# Read data
travel <- lapply(Sys.glob("Neue Tabellen/Entfernungen/*.csv"), 
                 function(x) read.csv(x))
# Rename data
names(travel) <- c("model_0", "model_1a", "model_1b", "model_2a", "model_2b")

# Create dataframe with all points
travel_all <- travel %>%
  map(~select(.x, -X)) %>%
  bind_rows(.id="column_label") %>%
  group_by(ARS) %>%
  left_join(select(model_0, RS_Kreis, SN_KTYP4_new), 
            by=join_by(ARS == RS_Kreis))
  
# 4.7. Travel distance analysis
# -----------------------------
# Model 0
x <- filter(travel_all, column_label=="model_0")
summary(x$Distance)
sd(x$Distance)
y1 <- filter(x, SN_KTYP4_new==1)
summary(y1$Distance)
sd(y1$Distance)
y2 <- filter(x, SN_KTYP4_new==3)
summary(y2$Distance)
sd(y2$Distance)
# Model 1a
x <- filter(travel_all, column_label=="model_1a")
summary(x$Distance)
sd(x$Distance)
y1 <- filter(x, SN_KTYP4_new==1)
summary(y1$Distance)
sd(y1$Distance)
y2 <- filter(x, SN_KTYP4_new==3)
summary(y2$Distance)
sd(y2$Distance)
# Model 2a
x <- filter(travel_all, column_label=="model_2a")
summary(x$Distance)
sd(x$Distance)
y1 <- filter(x, SN_KTYP4_new==1)
summary(y1$Distance)
sd(y1$Distance)
y2 <- filter(x, SN_KTYP4_new==3)
summary(y2$Distance)
sd(y2$Distance)
# Model 1b
x <- filter(travel_all, column_label=="model_1b")
summary(x$Distance)
sd(x$Distance)
y1 <- filter(x, SN_KTYP4_new==1)
summary(y1$Distance)
sd(y1$Distance)
y2 <- filter(x, SN_KTYP4_new==3)
summary(y2$Distance)
sd(y2$Distance)
# Model 2b
x <- filter(travel_all, column_label=="model_2b")
summary(x$Distance)
sd(x$Distance)
y1 <- filter(x, SN_KTYP4_new==1)
summary(y1$Distance)
sd(y1$Distance)
y2 <- filter(x, SN_KTYP4_new==3)
summary(y2$Distance)
sd(y2$Distance)

#_______________________________________________________________________________
#------------------------ 5. STATISTICAL VISUALIZATION -------------------------

# 5.1 Boxplot of absolute student numbers
# ---------------------------------------
## Preparation of data frame for ggplot: Select number of students and 
## change to long format, recode numeric to character description of district 
## structure and chance factor levels
x <- select(stat_students, c(structure, starts_with("n_m"))) %>%
  pivot_longer(cols=2:6, names_to ="model", values_to = "n") %>%
  mutate(structure = case_when(structure==3~"rural", structure==1~"urban")) %>%
  mutate(structure = factor(structure, levels=c("urban", "rural"))) %>%
  mutate(model = factor(model, levels=c("n_model_0", "n_model_1a", "n_model_1b", 
                                        "n_model_2a", "n_model_2b")))
### Creating grouped boxplot and saving plot as png
plot12 <- x %>% ggplot(aes(x=model, y=n, fill=structure)) + 
  geom_boxplot(outlier.size = 0.5) + 
  theme_bw() + theme(legend.position = "bottom") + ylim(c(-60, 1000)) + 
  labs(x = "inclusion model", y = "n students per primary school", 
       fill = "settlement structure") +
  scale_x_discrete(
    labels = c("model 0", "model 1a","model 1b","model 2a","model 2b")) +
  scale_fill_manual(values = c("urban" = "grey50", "rural" = "grey90"))
plot12
ggsave("Abbildungen/figure3_1.png", plot12, width=11, height=8, units=c("cm"))

# 5.2 Boxplot of percentual student increase
# ------------------------------------------
## Preparation of data frame for ggplot: Select perccentage of added students 
## and change to long format, recode numeric to character description of 
## district structure and chance factor levels
x <- select(stat_students, c(structure, starts_with("p_add"))) %>%
  pivot_longer(cols=2:5, names_to ="model", values_to = "p_add") %>%
  mutate(structure = case_when(structure==3~"rural", structure==1~"urban")) %>%
  mutate(structure = factor(structure, levels=c("urban", "rural"))) %>%
  mutate(model = factor(model, levels=c("p_add_model_1a", "p_add_model_1b", 
                                        "p_add_model_2a", "p_add_model_2b")))
## Creating grouped boxplot and saving plot as png
plot13 <- x %>% ggplot(aes(x=model, y=p_add, fill=structure)) + 
  geom_boxplot(outlier.size = 0.5) + 
  theme_bw() + theme(legend.position = "bottom") + ylim(c(-2, 35)) + 
  labs(x = "inclusion model", y = "% student increase", fill = "settlement structure") +
  scale_x_discrete(
    labels = c("model 1a","model 1b","model 2a","model 2b")) +
  scale_fill_manual(values = c("urban" = "grey50", "rural" = "grey90"))
plot13
ggsave("Abbildungen/figure3_2.png", plot13, width=9, height=8, units=c("cm"))
# Plot poster
x %>% ggplot(aes(x=model, y=p_add, fill=structure)) + 
  geom_boxplot(outlier.size = 2, linewidth=1) + 
  labs(x = "", y = "% Vergrößerung Schule") +
  theme_minimal() + #ylim(c(-2, 35)) +
  theme(axis.ticks.y=element_blank(), legend.position = "bottom") +
  scale_x_discrete(
    labels = c("Modell 1a","Modell 1b","Modell 2a","Modell 2b")) +
  scale_fill_manual(values = c("urban" = "#FAD8DB", "rural" = "#9F3E47"))

# 5.3 Boxplot of support rate
# ---------------------------
x <- select(stat_support, c(structure, starts_with("model"))) %>%
  pivot_longer(cols=2:6, names_to ="model", values_to = "n") %>%
  mutate(structure = case_when(structure==3~"rural", structure==1~"urban")) %>%
  mutate(structure = factor(structure, levels=c("urban", "rural"))) %>%
  mutate(model = factor(model, levels=c("model_0", "model_1a", "model_1b", 
                                        "model_2a", "model_2b")))
### Creating grouped boxplot and saving plot as png
plot14 <- x %>% ggplot(aes(x=model, y=n, fill=structure)) + 
  geom_boxplot(outlier.size = 0.5) + 
  theme_bw() + theme(legend.position = "bottom") + ylim(c(-3, 50)) + 
  labs(x = "inclusion model", y = "support rate per primary school", 
       fill = "settlement structure") +
  scale_x_discrete(
    labels = c("model 0", "model 1a","model 1b","model 2a","model 2b")) +
  scale_fill_manual(values = c("urban" = "grey50", "rural" = "grey90"))
plot14
ggsave("Abbildungen/figure4_1.png", plot14, width=11, height=8, units=c("cm"))

# 5.4 Boxplot of support rate increase
# ------------------------------------
x <- select(stat_support, c(structure, starts_with("p_m"))) %>%
  pivot_longer(cols=2:5, names_to ="model", values_to = "p") %>%
  mutate(structure = case_when(structure==3~"rural", structure==1~"urban")) %>%
  mutate(structure = factor(structure, levels=c("urban", "rural"))) %>%
  mutate(model = factor(model, levels=c("p_model_1a", "p_model_1b", 
                                        "p_model_2a", "p_model_2b")))
## Creating grouped boxplot and saving plot as png
plot15 <- x %>% ggplot(aes(x=model, y=p, fill=structure)) + 
  geom_boxplot(outlier.size = 0.5) + 
  theme_bw() + theme(legend.position = "bottom") + ylim(c(-2,25)) + 
  labs(x = "inclusion model", y = "support rate increase", fill = "settlement structure") +
  scale_x_discrete(
    labels = c("model 1a","model 1b","model 2a","model 2b")) +
  scale_fill_manual(values = c("urban" = "grey50", "rural" = "grey90"))
plot15
ggsave("Abbildungen/figure4_2.png", plot15, width=9, height=8, units=c("cm"))
# Plot poster
x %>% ggplot(aes(x=model, y=p, fill=structure)) + 
  geom_boxplot(outlier.size = 2, linewidth=1) + 
  labs(x = "", y = "% Vergrößerung Förderquote") +
  theme_minimal() + #ylim(c(-2, 35)) +
  theme(axis.ticks.y=element_blank(), legend.position = "bottom") +
  scale_x_discrete(
    labels = c("Modell 1a","Modell 1b","Modell 2a","Modell 2b")) +
  scale_fill_manual(values = c("urban" = "#FAD8DB", "rural" = "#9F3E47"))

# 5.5 Barplot of school numbers
# -----------------------------
a <- select(model_0, Schulart, SFZ) %>% 
  mutate(Schule=case_when(
    Schulart=="Grundschulen" ~ "primary school",
    Schulart=="Foerderzentren" & SFZ==0 ~ "other special school",
    Schulart=="Foerderzentren" & SFZ==1 ~ "SFZ")) %>% count(Schule) %>%
  mutate(model = "model 0")
b <- select(model_1a, Schulart, SFZ) %>% 
  mutate(Schule=case_when(
    Schulart=="Grundschulen" ~ "primary school",
    Schulart=="Foerderzentren" & SFZ==0 ~ "other special school",
    Schulart=="Foerderzentren" & SFZ==1 ~ "SFZ")) %>% count(Schule) %>%
  mutate(model = "model 1a")
c <- select(model_1b, Schulart, SFZ, n_new_students_school) %>%
  mutate(n_new = replace_na(n_new_students_school, 0)) %>%
  mutate(Schule=case_when(
    Schulart=="Grundschulen" & n_new==0 ~ "primary school",
    Schulart=="Grundschulen" & n_new!=0 ~ "inclusive primary school",
    Schulart=="Foerderzentren" & SFZ==0 ~ "other special school",
    Schulart=="Foerderzentren" & SFZ==1 ~ "SFZ")) %>% count(Schule) %>%
  mutate(model = "model 1b")
d <- select(model_2a, Schulart, SFZ) %>% 
  mutate(Schule=case_when(
    Schulart=="Grundschulen" ~ "primary school",
    Schulart=="Foerderzentren" & SFZ==0 ~ "other special school",
    Schulart=="Foerderzentren" & SFZ==1 ~ "SFZ")) %>% count(Schule) %>%
  mutate(model = "model 2a")
e <- select(model_2b, Schulart, SFZ, n_new_students_school) %>%
  mutate(n_new = replace_na(n_new_students_school, 0)) %>%
  mutate(Schule=case_when(
    Schulart=="Grundschulen" & n_new==0 ~ "primary school",
    Schulart=="Grundschulen" & n_new!=0 ~ "inclusive primary school",
    Schulart=="Foerderzentren" & SFZ==0 ~ "other special school",
    Schulart=="Foerderzentren" & SFZ==1 ~ "SFZ")) %>% count(Schule) %>%
  mutate(model = "model 2b")
f <- rbind(a, b, c, d, e) %>% 
  mutate(Schulart = case_when(
    Schule=="SFZ" ~ "special",
    Schule=="other special school" ~ "special",
    Schule=="primary school" ~ "primary",
    Schule=="inclusive primary school" ~ "primary")) %>%
  mutate(Schule = factor(
    Schule, levels=c("primary school","inclusive primary school", 
                     "SFZ", "other special school")))

plot16 <- ggplot(f, aes(x=Schulart, y=n, fill=Schule)) + 
  geom_bar(stat="identity", color="black") +
  facet_grid(~model) +
  theme_bw() + scale_fill_grey(start=1, end=0.3) + theme(legend.position = "bottom")
plot16
ggsave("Abbildungen/figure5.png", plot16, width=20, height=, units=c("cm"))



# 5.6 Violinplot 
# --------------
x <- travel_all %>% ungroup() %>% sample_n(size=6637032)

plot17 <- ggplot(x, aes(x=column_label, y=Distance, 
                        fill=as.factor(SN_KTYP4_new))) + 
  geom_violin(stat="ydensity", color="black") +
  theme_bw() + 
  scale_fill_manual(values = c("1" = "grey50", "3" = "grey90"),
                    name = "settlement structure", 
                    labels = c("urban", "rural")) +
  #scale_fill_grey(start=0.3, end=1, name = "settlement structure", 
  #                labels = c("urban", "rural")) + 
  xlab("inclusion model") + ylab("travel distance (km)") +
  scale_x_discrete(labels=c("model_0" = "model 0", "model_1a" = "model 1a",
                            "model_1b" = "model 1b", "model_2a" = "model 2a",
                            "model_2b" = "model 2b")) +
  theme(legend.position = "bottom")
plot17

ggsave("Abbildungen/figure6.png", plot17, width=20, height=8, units=c("cm"))

# Plot poster
x %>% ggplot(aes(x=column_label, y=Distance, fill=as.factor(SN_KTYP4_new))) + 
  geom_violin(stat="ydensity", color="black", linewidth=1) +
  theme_minimal() +
  theme(axis.ticks.y=element_blank(), legend.position = "bottom") +
  scale_x_discrete(
    labels = c("Modell 1a","Modell 1b","Modell 2a","Modell 2b")) +
  scale_fill_manual(values = c("1" = "#FAD8DB", "3" = "#9F3E47"),
                    name = "settlement structure", 
                    labels = c("urban", "rural"))
  
  scale_fill_manual(values = c("urban" = "", "rural" = ""))

#_______________________________________________________________________________

################################################################################
################## Study 2: Simulating Teacher Destributions ###################
################################################################################

# 0. Setting up ----------------------------------------------------------------

# Packages
packages <- c("tidyverse", "patchwork", "geosphere", "sf", "ggspatial", "khroma")
lapply(packages, library, character.only = T)

# Environment
setwd("H:/Daten/Bayern Daten/Neue Tabellen/Modelle")

# 1. Loading and Reshaping Data ------------------------------------------------

# Function for loading and reshaping
function1 <- function(path) {read.csv(path) %>%
    # Select necessary variables
    select(Schulnummer,RS_Kreis,Schulart,Schultyp,Strasse,PLZ,Ort,lat,lon,
           RS_Gemeinde,KTYP4,SN_KTYP4,n_students_school,n_students_SEN_primary,
           n_students_PS_add,prop_students_PS_add,n_students_PS_new, 
           prop_support_rate_new) %>%
    # Rename columns
    rename(n_students_old = n_students_school,
           n_students_SEN_old = n_students_SEN_primary,
           n_students_SEN_add = n_students_PS_add,
           n_students_new = n_students_PS_new) %>%
    # Generate new column: School size
    mutate(n_students_SEN_new = n_students_SEN_old + n_students_SEN_add)}

# Current Model
data <- read.csv("model_0.csv") 

# Model 1 without Special Schools
data_noSS <- function1("model_1a.csv")

# Model 2 with some Special School Types
data_someSS <- function1("model_2a.csv")

# Geodata of districts
district <- st_read("Z:/2. Forschung/Projekte/Inklusive Schulsysteme (Nikola Habil)/Schulstatistiken/Geodaten/Verwaltungseinheiten Deutschland/vg250_ebenen_0101/VG250_KRS.shp") %>%
  mutate(RS_Kreis = as.numeric(ARS)) %>% select(RS_Kreis, GEN, geometry)

# Geodata of Bavaria
bavaria <- st_read("Z:/2. Forschung/Projekte/Inklusive Schulsysteme (Nikola Habil)/Schulstatistiken/Geodaten/Verwaltungseinheiten Deutschland/Bundesländer_Ohne_Wasser.shp")

# 2. Calculation of Special Education Teacher Hours ----------------------------

# Percentage of subject teachers + religion teachers on FT and HT
9485 # all teachers
5594 # all FT teachers
3793 # all HT teachers
390  # subject teachers in FT
278  # subject teachers in HT
262  # religion teachers in HT

390/5574 # 7 % of FT teachers are subject teachers
(278+262)/3783 # 14.3 % of HT teachers are subject or religion teachers

# Full time hours + half time hours + short time hours + "Referendariat"
110994 +  # Lesson hours FT
  26331 + # Advisory hours FT
  10969 + # Mobile hours FT
59103 +   # Lesson hours HT
  10994 + # Advisory hours HT
  6962 +  # Mobile hours HT
11326 +   # Lesson hours ST
  3537 +  # Advisory hours ST
  2289 +  # Mobile hours ST
8461      # Referendariat hours
# 148294 FT teacher hours, 77059 HT teacher hours, 17152 ST teacher hours   
# --> 250966 teacher hours in total

# Subtracting subject and religion teacher hours
250966 - (148294 * 0.07) - (77059 * 0.143) 
# --> 229566 resulting teacher hours
# Substracting 5 % of teacher hours for hospital schools or other reasons
229566 * 0.95
# --> 218087.7 resulting teacher hours

table(data$Schultyp)
  (30 +          # Behavior Disorder
    10 +         # Learning
    164 +        # SFZ
    7) / 350     # Speaking
table(data$Schulart) # --> 350 Special Schools in Current System
## --> 40% of special schools remain in Model 2

# 3. Data Description ----------------------------------------------------------

# Number of districts in urban / rural areas
data %>% select(RS_Kreis,SN_KTYP4) %>% unique() %>% pull(SN_KTYP4) %>% table()

# Number of School types in urban / rural areas
unique(select(data,Schulnummer,Schulart,SN_KTYP4)) %>% count(Schulart,SN_KTYP4)

# Number of total students
data %>% select(n_students_school, Schulart,SN_KTYP4) %>% unique() %>% 
  summarise(total_students = sum(n_students_school, na.rm = T))

# 4. Simulating special educator distribution ----------------------------------

# Calculation of proportion factors
## Model 1 (50% teacher hours / all students with SEN)
propfac_1 <- (218087.7/2) / sum(data_noSS$n_students_SEN_new, na.rm=T)
## Model 2 (60% of model 1 teacher hours / all students with SEN)
propfac_2 <- (218087.7/2)*0.6 / sum(data_someSS$n_students_SEN_new, na.rm=T)

# Function for Distribution of special educator hours
function2 <- function(d, propfac, sim_number) {
  result <- d %>%
  mutate(teacher_hours = n_students_SEN_new*propfac) %>%
  mutate(n_speced_teachers = teacher_hours/26) %>%
  group_by(RS_Kreis) %>%
  mutate(teacher_hours_Kreis = sum(teacher_hours, na.rm=T)) %>%
  mutate(teachers_Kreis = teacher_hours_Kreis/26) %>%
  mutate(SN_KTYP4 = as.factor(SN_KTYP4)) %>%
  mutate(SN_KTYP4_ENG = case_when(
    SN_KTYP4==1 ~ "large cities", SN_KTYP4==2 ~ "urban districts",
    SN_KTYP4==3 ~ "rural districts", SN_KTYP4==4 ~ "sparsley\npopulated")) %>%
  mutate(SN_KTYP4_ENG = factor(SN_KTYP4_ENG,
    levels=c("large cities", "urban districts", 
             "rural districts", "sparsley\npopulated"))) %>%
  mutate(sim = sim_number) %>% ungroup()
  return(result)}

# Distribution Model 1
model1 <- function2(data_noSS, propfac_1, 1)

# Distribution Model 1
model2 <- function2(data_someSS, propfac_2, 2)

# 5. Matching Schools ----------------------------------------------------------

# Greedy Algorithm per District
function3 <- function(df, model = NULL) {
  # New empty list to save school pairs
  pairs <- list()
  # Loop: Do it for every district!
  for (kreis in unique(df$RS_Kreis)) {
    # Filter the data
    d_kreis <- df %>% filter(RS_Kreis == kreis)
    # Create the coordinates matrix for distance calculation
    coords <- cbind(d_kreis$lon, d_kreis$lat)
    # Compute the distance matrix
    dist_matrix <- distm(coords)
    # Function to calculate the teacher hours difference
    teacher_diff <- function(i, j) {
      abs(d_kreis$teacher_hours[i] - d_kreis$teacher_hours[j])
    }
    # Number of schools, which need matching
    remaining_schools <- 1:nrow(d_kreis)
    # Do this, as long as more than 1 unmatched school remains
    while (length(remaining_schools) > 1) {
      # Create empty variables to save information
      best_pair <- NULL # Pair of school numbers
      best_distance <- Inf # Minimal distance
      best_teacher_diff <- -Inf # Largest teacher hour diff
      best_pair_index <- c() # Indices of best school pair
      # Select the first school to match
      for (i in 1:(length(remaining_schools) - 1)) {
        # Select second school, which matches the first school
        for (j in (i + 1):length(remaining_schools)) {
          # List the two schools to compare
          school_i <- remaining_schools[i]
          school_j <- remaining_schools[j]
          # Calculate the distance between the schools
          distance <- dist_matrix[school_i, school_j]
          # Calculate the teacher hour diff between the schools
          teacher_diff_value <- teacher_diff(school_i, school_j)
          # If distance and teacher diff are better then other options,
          # choose this pair as matching pair
          if (distance < best_distance || (
            distance==best_distance&&teacher_diff_value>best_teacher_diff)) {
            # Save variables of matching pair
            best_pair <- c(school_i, school_j)
            best_distance <- distance
            best_teacher_diff <- teacher_diff_value
            best_pair_index <- c(i, j)
          }
        }
      }
      # Add the best pair to the list
      pairs <- c(pairs, list(c(d_kreis$Schulnummer[best_pair[1]], 
                               d_kreis$Schulnummer[best_pair[2]])))
      # Remove the selected pair from the remaining schools
      remaining_schools <- remaining_schools[-c(best_pair_index)]
    }
  }
  # Create a DataFrame from the list of pairs for a better overview
  paired_schools <- do.call(rbind, pairs) %>%
    # List to dataframe
    as.data.frame() %>%
    # Rename columns
    rename(Schule_A = V1, Schule_B = V2) %>%
    # New column with pair-ID
    mutate(Group = paste0(pmin(Schule_A, Schule_B), "_", 
                          pmax(Schule_A, Schule_B))) %>%
    # Restructure df: Every row is a school
    pivot_longer(c(Schule_A, Schule_B), values_to = "Schulnummer") %>%
    # Remove old variable
    select(-name)
  
  # Join the pairs and match left schools to nearest pair
  if (!is.null(model)) {
    model_matched <- model %>% 
      # Join pairs and remove duplicates
      right_join(paired_schools) %>% unique() %>%
      # Sort distance between schools in a district
      arrange(RS_Kreis, lon, lat) %>% group_by(RS_Kreis) %>%
      # Manually match left schools to nearest district
      fill(Group, .direction = "up") %>% fill(Group, .direction = "down") %>%
      ungroup() %>% group_by(Group) %>%
      # Create new columns
      mutate(dist_km = mean(
        distm(matrix(c(lon, lat), ncol = 2)) / 1000, na.rm = T)) %>%
      mutate(dist_km = na_if(dist_km, 0)) %>%
      mutate(sum_hours = sum(teacher_hours)) %>%
      mutate(sum_teachers = sum_hours / 26)
    return(model_matched)
  }
  return(paired_schools)
}

# Match schools without full-time special educators per district
x1 <- model1 %>% filter(teacher_hours < 26) %>% function3(model=model1)
x2 <- model2 %>% filter(teacher_hours < 26) %>% function3(model=model2)

# Combine all information into one df and calculate new variables
models <- rbind(x1, x2) %>% ungroup() %>% 
  full_join(model1) %>% full_join(model2) %>%
  group_by(RS_Kreis, sim) %>%
  # Proportion of matched schools / all schools in district
  mutate(prop_matched = (sum(is.na(Group)))/n()*100) %>%
  # Fot plots
  mutate(system = case_when(sim==1 ~ "Model 1", sim==2 ~ "Model 2")) %>%
  mutate(sum_hours = case_when(sum_hours > 5000 ~ 5000, TRUE ~ sum_hours)) %>%
  ungroup()

rm(x1, x2) # Remove unused dfs

# 6. Plots ---------------------------------------------------------------------

# Figure 1

## Figure 1a - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- models %>% left_join(district) %>% st_as_sf()
y <- models %>% filter(Schulart == "Foerderzentren" & Schultyp != "Kranke") %>%
  select(system, lat, lon) %>% st_as_sf(coords = c("lon", "lat"), crs=4326)

plot2a <- ggplot() + 
  # Districts filled with teacher hours
  geom_sf(data=x, aes(geometry=geometry, fill=teacher_hours_Kreis)) +
  # Points of Schools
  geom_sf(data=y, aes(geometry=geometry, color="")) +
  # legend, margins and style
  labs(fill = "weekly special education hours", color="special school locations",
       subtitle=bold("A:")~
         "Spatial distribution of weekly special education hours and special school locations") + 
  annotation_scale(location="br") +
  theme_bw() +
  theme(legend.position = "bottom", legend.justification = "left",
        legend.key.width=unit(1, "cm")) +
  facet_grid(cols = vars(system)) +
  # labels and text
  guides(
    fill = guide_colorbar(title.position = "top", order=1, title.hjust = 0.5),
    color = guide_legend(title.position = "top", order=2, title.hjust = 0.5)) +
  # colors
  scale_color_manual(values="black") +
  scale_fill_distiller(
    type="seq",direction=1,palette="Greys",limits=c(100,5000),
    breaks=c(1000, 2000, 3000, 4000, 5000),
    labels=c("1000", "2000", "3000", "4000", "> 5000"))

## Figure 1b - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
plot2b <- models %>%
  ggplot(aes(y=teacher_hours, x=SN_KTYP4_ENG, fill=system)) +
  geom_boxplot()+ theme_bw() + 
  # legend, margins and style
  theme(legend.position = "bottom", legend.justification = "left",
        legend.margin = margin(5,0,0,0),
        legend.box.margin = margin(-10,-10,-10,0),
        axis.title.x = element_text(margin=margin(t=-5)),
        plot.title = element_text(size=12, face="bold")) +
  # labels and text
  ylab("weekly special education hours") + xlab("district categories") + 
  labs(subtitle=bold("B:")~
         "Weekly special education hours") +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) +
  # second y-axis
  scale_y_continuous("weekly special education hours", breaks = seq(0, 350, 50)) +
  # colors & lines
  scale_fill_manual(values=c("grey80", "grey45")) +
  geom_hline(yintercept = 26, linetype = "dashed", color = "black")

## Combining and saving figures - - - - - - - - - - - - - - - - - - - - - - - - 
plot2 <- plot2a/guide_area()/(plot2b | plot2c) + 
  plot_layout(heights=c(1, 0.2, 1), guides="collect") &
  theme(legend.box="horizontal")

plot2

ggsave("Abbildungen/Plot2.png", 
       plot=plot2, width=21, heigh=24, units=c("cm"), dpi=1200)

# Figure 2
plot2c <- models %>%
  mutate(demand = case_when(teacher_hours >=26 ~ "full position(s)", 
                            teacher_hours < 26 ~ "no full position")) %>%
  mutate(system = case_when(system == "Model 1" ~ "1", 
                            system == "Model 2" ~ "2")) %>%
  filter(!is.na(demand)) %>%
  ggplot(aes(x = system, fill = demand)) +
  facet_grid(cols = vars(SN_KTYP4_ENG)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = c(0, 0))  +
  labs(subtitle=bold("C:")~
         "Special education positions") +
  labs(y = "schools with (no) full position demand (%)", x = "Model") +
  theme(legend.position = "bottom",
        axis.title.x = element_text(margin=margin(t=-5))) +
  scale_fill_manual(values=c("grey80", "grey45")) +
  theme_bw()
  
plot2c

# Figure 3

## Preparing variables - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
models_sf <- models %>% # Spatial Points
  st_as_sf(coords = c("lon", "lat"), crs=4326) 
models_lines <- models_sf %>% group_by(Group) %>% # Spatial Lines
  mutate(geometry = st_union(geometry) %>% st_cast("LINESTRING"))

## Munich: Model 1 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
plot3a <- ggplot() +
  # Map of Munich City
  geom_sf(data = district %>% filter(RS_Kreis=="9162") %>% select(geometry), 
          fill = "grey95", color = "black") +
  # Points of Schools
  geom_sf(data=models_sf %>% filter(RS_Kreis == "9162") %>% filter(sim==1) %>%
            select(teacher_hours, geometry), aes(size=teacher_hours), alpha=0.7) +
  # Lines between Partner Schools
  geom_sf(data = models_lines %>% filter(RS_Kreis == "9162") %>% filter(sim==1) %>%
            filter(!is.na(Group)), aes(fill=Group)) +
  # Labs
  labs(subtitle=bold("Model 1:")~"Large city") +
  # Customized Theme
  guides(fill = "none") + theme_bw() + annotation_scale() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks=seq(11, 12, by=0.1)) +
  scale_size_continuous(name = "weekly special education hours", guide = "legend",
                        breaks = c(10, 25, 50, 75, 100), limits=c(0, 200))

## Tirschenreuth: Model 1 - - - - - - - - - - - - - - - - - - - - - - - - - - -     
plot3b <- ggplot() +
  # Map of Munich City
  geom_sf(data = district %>% filter(RS_Kreis=="9377") %>% select(geometry), 
          fill = "grey95", color = "black") +
  # Points of Schools
  geom_sf(data=models_sf %>% filter(RS_Kreis == "9377") %>% filter(sim==1) %>%
            select(teacher_hours, geometry), aes(size=teacher_hours), alpha=0.7) +
  # Lines between Partner Schools
  geom_sf(data = models_lines %>% filter(RS_Kreis == "9377") %>% filter(sim==1) %>%
            filter(!is.na(Group)), aes(fill=Group)) +
  # Labs
  labs(subtitle=bold("Model 1:")~"Sparsely populated district") +
  # Customized Theme
  guides(fill = "none") + theme_bw() + annotation_scale() +
  theme(legend.position = "bottom") +
  scale_size_continuous(name = "weekly special education hours", guide = "legend",
                        breaks = c(10, 25, 50, 75, 100), limits=c(0, 200))

## Munich: Model 2 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
plot3c <- ggplot() +
  # Map of Munich City
  geom_sf(data = district %>% filter(RS_Kreis=="9162") %>% select(geometry), 
          fill = "grey95", color = "black") +
  # Points of Schools
  geom_sf(data=models_sf %>% filter(RS_Kreis == "9162") %>% filter(sim==2) %>%
            select(teacher_hours, geometry), aes(size=teacher_hours), alpha=0.7) +
  # Lines between Partner Schools
  geom_sf(data = models_lines %>% filter(RS_Kreis == "9162") %>% filter(sim==2) %>%
            filter(!is.na(Group)), aes(fill=Group)) +
  # Labs
  labs(subtitle=bold("Model 2:")~"Large city") +
  # Customized Theme
  guides(fill = "none") + theme_bw() + annotation_scale() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks=seq(11, 12, by=0.1)) +
  scale_size_continuous(name = "weekly special education hours", guide = "legend",
                        breaks = c(10, 25, 50, 75, 100), limits=c(0, 200))

## Tirschenreuth: Model 2 - - - - - - - - - - - - - - - - - - - - - - - - - - -     
plot3d <- ggplot() +
  # Map of Munich City
  geom_sf(data = district %>% filter(RS_Kreis=="9377") %>% select(geometry), 
          fill = "grey95", color = "black") +
  # Points of Schools
  geom_sf(data=models_sf %>% filter(RS_Kreis == "9377") %>% filter(sim==2) %>%
            select(teacher_hours, geometry), aes(size=teacher_hours), alpha=0.7) +
  # Lines between Partner Schools
  geom_sf(data = models_lines %>% filter(RS_Kreis == "9377") %>% filter(sim==2) %>%
            filter(!is.na(Group)), aes(fill=Group)) +
  # Labs
  labs(subtitle=bold("Model 2:")~"Sparsely populated district") +
  # Customized Theme
  guides(fill = "none") + theme_bw() + annotation_scale() +
  theme(legend.position = "bottom") +
  scale_size_continuous(name = "weekly special education hours", guide = "legend",
                        breaks = c(10, 25, 50, 75, 100), limits=c(0, 200))

plot3 <- plot3a+plot3b+plot3c+plot3d+plot_layout(guides = "collect", nrow=2) & 
  theme(legend.position='bottom')

ggsave("Abbildungen/Plot3.png", 
       plot=plot3, width=25, heigh=18, units=c("cm"), dpi=1200)

# Figure 4

## Figure 4a - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
plot4a <- models %>% 
  ggplot(aes(fill=SN_KTYP4_ENG, y = sum_hours, x=as.factor(sim))) + 
  geom_boxplot(color="black", alpha=0.8) +
  theme_bw() + theme(legend.position = "bottom") +
  scale_fill_manual(values=c("grey90", "grey70", "grey50", "grey30")) +
  labs(x="model", y="SEH per school pair", fill="district categories")

## Figure 4b - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
plot4b <- models %>% 
  ggplot(aes(fill=SN_KTYP4_ENG, y = dist_km, x=as.factor(sim))) + 
  geom_boxplot(alpha=0.8) +
  theme_bw() + theme(legend.position = "none") +
  scale_fill_manual(values=c("grey90", "grey70", "grey50", "grey30")) +
  labs(x="model", y="distance between school pairs (km)", 
       fill="district categories") +
  scale_y_continuous(position="right", breaks=seq(0, 30, 5),
                     limits = c(0, 20))

## Combining and saving figures - - - - - - - - - - - - - - - - - - - - - - - - 
plot4 <- (plot4a + plot4b)/guide_area() + 
  plot_layout(guides="collect", heights=c(6, 1)) +
  plot_annotation(subtitle=bold("Model comparison:")~
                    "Mobile special educator hour demand and school pair distances")
plot4

ggsave("Plot4.png", 
       plot=plot4, width=20, heigh=12, units=c("cm"), dpi=1200)

## 7. Analysis -----------------------------------------------------------------

names(models)

# Percentage of primary schools with > 1 special educator
nrow(models %>% filter(sim == 1) %>% filter(n_speced_teachers >= 1)) / 
  nrow(models %>% filter(sim == 1))*100
nrow(models %>% filter(sim == 2) %>% filter(n_speced_teachers >= 1)) / 
  nrow(models %>% filter(sim == 2))*100

# Percentage of primary schools with < 1 special educator
100-(nrow(models %>% filter(sim == 1) %>% filter(n_speced_teachers >= 1)) / 
  nrow(models %>% filter(sim == 1))*100)
100-(nrow(models %>% filter(sim == 2) %>% filter(n_speced_teachers >= 1)) / 
  nrow(models %>% filter(sim == 2))*100)

# Number of teacher hours and teachers per primary school
summary(models %>% filter(sim == 1) %>% pull(teacher_hours))
summary(models %>% filter(sim == 1) %>% pull(n_speced_teachers))
summary(models %>% filter(sim == 2) %>% pull(teacher_hours))
summary(models %>% filter(sim == 2) %>% pull(n_speced_teachers))

# Average demand of spec ed hours per district type
x <- models %>% group_by(RS_Kreis, sim) %>% filter(sim==1) %>%
  mutate(mean_hours = mean(teacher_hours, na.rm=T), 
         mean_teachers = mean(teachers, na.rm=T))

mean(x %>% filter(SN_KTYP4_ENG=="large cities") %>% pull(mean_hours))
mean(x %>% filter(SN_KTYP4_ENG=="urban districts") %>% pull(mean_hours))
mean(x %>% filter(SN_KTYP4_ENG=="rural districts") %>% pull(mean_hours))
mean(x %>% filter(SN_KTYP4_ENG=="sparsley\npopulated") %>% pull(mean_hours))

summary(aov(data=x, mean_hours~SN_KTYP4_ENG))
pairwise.t.test(x$mean_hours, x$SN_KTYP4_ENG)

x <- models %>% group_by(RS_Kreis, sim) %>% filter(sim==2) %>%
  mutate(mean_hours = mean(teacher_hours, na.rm=T), 
         mean_teachers = mean(teachers, na.rm=T))

mean(x %>% filter(SN_KTYP4_ENG=="large cities") %>% pull(mean_hours))
mean(x %>% filter(SN_KTYP4_ENG=="urban districts") %>% pull(mean_hours))
mean(x %>% filter(SN_KTYP4_ENG=="rural districts") %>% pull(mean_hours))
mean(x %>% filter(SN_KTYP4_ENG=="sparsley\npopulated") %>% pull(mean_hours))

summary(aov(data=x, mean_hours~SN_KTYP4_ENG))
pairwise.t.test(x$mean_hours, x$SN_KTYP4_ENG)

# Difference in hours demand between sim 1 and 2
x <- models %>% group_by(RS_Kreis, sim) %>%filter(SN_KTYP4_ENG=="large cities") %>%
  mutate(mean_hours = mean(teacher_hours, na.rm=T))
t.test(data=x, mean_hours~sim)
x <- models %>% group_by(RS_Kreis, sim) %>%filter(SN_KTYP4_ENG=="urban districts") %>%
  mutate(mean_hours = mean(teacher_hours, na.rm=T))
t.test(data=x, mean_hours~sim)
x <- models %>% group_by(RS_Kreis, sim) %>%filter(SN_KTYP4_ENG=="rural districts") %>%
  mutate(mean_hours = mean(teacher_hours, na.rm=T))
t.test(data=x, mean_hours~sim)
x <- models %>% group_by(RS_Kreis, sim) %>%filter(SN_KTYP4_ENG=="sparsley\npopulated") %>%
  mutate(mean_hours = mean(teacher_hours, na.rm=T))
t.test(data=x, mean_hours~sim)

x <- filter(models, Ort == "Muenchen", sim=="2")

# Mobile special educator hours demand of district categories
models %>% 
  select(SN_KTYP4_ENG, sim, sum_hours) %>% na.omit() %>%
  count(sim, SN_KTYP4_ENG)

# Hours demand of mobile schools
summary(models %>% filter(sim == 1) %>% pull(sum_hours))
summary(models %>% filter(sim == 2) %>% pull(sum_hours))
sd(models %>% filter(sim == 1) %>% pull(sum_hours), na.rm=T)
sd(models %>% filter(sim == 2) %>% pull(sum_hours), na.rm=T)

# Distance between school pairs (km)
summary(models %>% filter(sim == 1) %>% pull(dist_km))
sd(models %>% filter(sim == 1) %>% pull(dist_km), na.rm=T)
summary(models %>% filter(sim == 1 & SN_KTYP4_ENG == "urban districts") %>% pull(dist_km))
summary(models %>% filter(sim == 1 & SN_KTYP4_ENG == "rural districts") %>% pull(dist_km))
summary(models %>% filter(sim == 1 & SN_KTYP4_ENG == "sparsley\npopulated") %>% pull(dist_km))

summary(models %>% filter(sim == 2) %>% pull(dist_km))
sd(models %>% filter(sim == 2) %>% pull(dist_km), na.rm=T)
summary(models %>% filter(sim == 2 & SN_KTYP4_ENG == "large cities") %>% pull(dist_km))
summary(models %>% filter(sim == 2 & SN_KTYP4_ENG == "urban districts") %>% pull(dist_km))
summary(models %>% filter(sim == 2 & SN_KTYP4_ENG == "rural districts") %>% pull(dist_km))
summary(models %>% filter(sim == 2 & SN_KTYP4_ENG == "sparsley\npopulated") %>% pull(dist_km))

# Tirschenreuth Example

x <- models %>% filter(RS_Kreis=="9377") %>% filter(system=="Model 1") 

# 6. Maps ----------------------------------------------------------------------

## Preparing variables - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
x <- models %>% left_join(select(district, RS_Kreis, GEN)) %>%
  select(RS_Kreis, GEN, KTYP4) %>% unique()

models_plot <- models %>% 
  left_join(select(district, RS_Kreis, GEN)) %>%
  mutate(studentsSEN_plot =case_when(Schulart=="Grundschulen" ~ n_students_SEN_new,
                                     Schulart=="Foerderzentren" ~ n_students_old))
models_sf <- models_plot %>% # Spatial Points
  mutate(Schultyp = case_when(
    Schultyp == "Grund- u. Mittelschule" ~ "Primary School",
    Schultyp == "GE" ~ "Special School - Intellectual Disability",
    Schultyp == "Sehen" ~ "Special School - Blindness",
    Schultyp == "Hoeren" ~ "Special School - Hearing",
    Schultyp == "KME" ~ "Special School - Physical Disability",
    Schultyp == "Kranke" ~ "Hospital School")) %>%
  st_as_sf(coords = c("lon", "lat"), crs=4326) 
models_lines <- models_sf %>% group_by(Group) %>% # Spatial Lines
  mutate(geometry = st_union(geometry) %>% st_cast("LINESTRING"))
  
## List of all districts
districts_unique <- unique(models_sf$RS_Kreis)

## Colors for mapping
farben <- colour("vibrant")(7)
school_colors <- c(
  "Primary School" = "black",
  "Special School - Intellectual Disability"     = farben[1], # orange
  "Special School - Blindness"  = farben[2], # dunkelblau
  "Special School - Hearing" = farben[3], # hellblau
  "Special School - Physical Disability"    = farben[5], # rot
  "Hospital School" = farben[6]) # dunkelgrün

## Plots of Model 1 for every district
for (kreis in districts_unique) {
  # Name des Kreises
  kreisname <- models_plot %>%
  filter(RS_Kreis == kreis) %>%
    distinct(GEN) %>%
    pull(GEN)
  # Filtere die Daten für den aktuellen Kreis
  kreis_geom <- district %>% filter(RS_Kreis == kreis) %>% select(geometry)
  schools <- models_sf %>% filter(RS_Kreis == kreis, sim == 1) %>% 
    select(studentsSEN_plot, Schultyp, geometry)
  # Plot
  p <- ggplot() +
    geom_sf(data = kreis_geom, fill = "grey95", color = "black") +
    geom_sf(data = schools, aes(size = studentsSEN_plot, color = Schultyp), alpha = 0.9) +
    scale_color_manual(values = school_colors) +
    labs(subtitle = paste0(kreisname, ": Model 1"),
         size="N students with SEN", color="School type") +
    guides(fill = "none") + 
    theme_bw() + annotation_scale() + theme(legend.position = "right")
  # Save plot
  ggsave(filename = paste0("Karten alle Landkreise/Modell 1/", kreisname, "-", kreis, ".png"),
         plot = p, width = 20, height = 15, units=c("cm"), dpi = 500)
}

## Plots of Model 2 for every district
for (kreis in districts_unique) {
  # Name des Kreises
  kreisname <- models_plot %>%
    filter(RS_Kreis == kreis) %>%
    distinct(GEN) %>%
    pull(GEN)
  # Filtere die Daten für den aktuellen Kreis
  kreis_geom <- district %>% filter(RS_Kreis == kreis) %>% select(geometry)
  schools <- models_sf %>% filter(RS_Kreis == kreis, sim == 2) %>% 
    select(studentsSEN_plot, Schultyp, geometry)
  # Plot
  p <- ggplot() +
    geom_sf(data = kreis_geom, fill = "grey95", color = "black") +
    geom_sf(data = schools, aes(size = studentsSEN_plot, color = Schultyp), alpha = 0.9) +
    scale_color_manual(values = school_colors) +
    labs(subtitle = paste0(kreisname, ": Model 2"),
         size="N students with SEN", color="School type") +
    guides(fill = "none") + 
    theme_bw() + annotation_scale() + theme(legend.position = "right") +
  # Save plot
  ggsave(filename = paste0("Karten alle Landkreise/Modell 2/", kreisname, "-", kreis, ".png"),
         plot = p, width = 20, height = 15, units=c("cm"), dpi = 500)
}

