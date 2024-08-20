

#packages

library(tidyverse)
library(lingtypology)
library(writexl)

#language coordinates
genlangpoints <- read_tsv("data/genlangpoints.csv")

#color cheat sheet:
# "#3b8bc6" - Avar Blue
# "#35a432" - Other Green
# "#f78716" - Azerbaijani Orange


# Good morning ------------------------------------------------------------

#load good morning dataset
morninggen <- read_tsv("data/morning_greetings.tsv") %>%
  mutate(map = case_when(id == 34 ~ 'no', #manually change values for Lezgian
                         id == 35 ~ 'yes',
                         TRUE ~ map)) %>%
  mutate(genlang_point = case_when(lang == 'Dargwa' ~ 'yes',
                                   id == 34 ~ 'no',
                                   id == 35 ~ 'yes',
                                   TRUE ~ genlang_point)) %>%
  filter(map == 'yes') %>%
  filter(genlang_point == 'yes') %>%
  mutate(lang = case_when(lang == 'Dargwa' ~ idiom,
                          TRUE ~ lang))

#add coordinates etc to good morning data
morninggenlang <- left_join(morninggen, genlangpoints, by='lang') %>%
  filter(!is.na(gltc_lang))

#combine dagatlas features to show zones more clearly
morninggenlang <- morninggenlang %>%
  mutate(mapfeature = case_when(value1 == "Did you wake up?" ~ "Did you wake up?",
                                value2 == "Azerbaijani" ~ "Sabah Xair",
                                TRUE ~ "Other"))

#single feature map
map.feature(lang.gltc(morninggenlang$gltc_lang),
            features = morninggenlang$mapfeature,
            latitude = morninggenlang$lat,
            longitude = morninggenlang$lon,
            color = c("#3b8bc6","#35a432","#f78716"),
            width = 10,
            tile = 'Esri.WorldGrayCanvas',
            title = morninggenlang$value1_name[1])


# Farewell ----------------------------------------------------------------

#load farewell dataset
farewell <- read_tsv("data/farewell.tsv") %>%
  mutate(map = case_when(id == 48 ~ 'yes',
                         id == 19 ~ 'yes',
                         TRUE ~ map)) %>%
  mutate(genlang_point = case_when(id == 48 ~ 'yes',
                         id == 19 ~ 'yes',
                         idiom == 'Kaitag' ~ 'yes',
                         TRUE ~ genlang_point)) %>%
  filter(map == 'yes') %>%
  filter(genlang_point == 'yes') %>%
  mutate(lang = case_when(lang == 'Dargwa' ~ idiom,
                          TRUE ~ lang)) %>%
  mutate(idiom = case_when(lang == "Tsakhur" ~ "Tsakhur",
                         TRUE ~ idiom))


#add coordinates etc to farewell data
farewelllang  <- left_join(farewell, genlangpoints, by='lang')

#combine dagatlas features to show zones more clearly
farewelllang <- farewelllang %>%
  mutate(mapfeature = case_when(value2 == "straight way" ~ "Straight way",
                                value2 == "jaʁur/juʁur/uʁur/uɣur" ~ "jaʁur/juʁur/uʁur/uɣur" , 
                                TRUE ~ "Other"))

#single feature map
map.feature(lang.gltc(farewelllang$gltc_lang),
            features = farewelllang$mapfeature,
            latitude = farewelllang$lat,
            longitude = farewelllang$lon,
            color=c("#f78716","#35a432", "#3b8bc6"),
            width=10,
            tile = 'Esri.WorldGrayCanvas',
            title = "Type of farewell")


# Commemorative -----------------------------------------------------------

#load commemorative dataset
commemorative <- read_tsv("data/commemorative.tsv") %>%
  mutate(map = case_when(idiom == 'Burkikhan' ~ 'no', #switch Agul
                         form == 'raħmat xaǯe' ~ 'yes', #switch idr what
                         form == 'Daala gechdojla (cunna)' ~ 'yes', #enable Chechen
                         form == 'рагьмат этгир' ~ 'yes', #enable Kumyk
                         TRUE ~ map)) %>%
  mutate(genlang_point = case_when(form == 'Daala gechdojla (cunna)' ~ 'yes',
                                   idiom == 'Kaitag' ~ 'yes', #enable Kaitag
                         form == 'рагьмат этгир' ~ 'yes',
                         example_as_in_source == 'Василий гебюрю нур пур гердю' ~ 'yes', #enable Tat
                         TRUE ~ genlang_point)) %>%
  filter(map == 'yes') %>%
  filter(genlang_point == 'yes') %>%
  mutate(lang = case_when(lang == 'Dargwa' ~ idiom,
                          TRUE ~ lang))

#add coordinates etc to commemorative data
commlang <- left_join(commemorative, genlangpoints, by='lang')

#combine dagatlas features to show zones more clearly
commlang <- commlang %>%
  mutate(mapfeature = case_when(value2 == "yes" ~ "May the sins be washed away",
                                value1 == "yes" ~ "Rahmat",
                                TRUE ~ "Other")) %>%
  filter(!is.na(gltc_lang)) %>% #remove Ossetic
  mutate(idiom = case_when(lang == "Tsez" ~ "Tsez", #overwrite NA idiom
                           TRUE ~ idiom)) %>%
  filter(!is.na(idiom)) #remove unknown idioms

#single feature map
map.feature(lang.gltc(commlang$gltc_lang),
            features = commlang$mapfeature,
            color=c("#3b8bc6","#35a432","#f78716"),
            width = 10,
            latitude = commlang$lat,
            longitude = commlang$lon,
            tile = 'Esri.WorldGrayCanvas',
            title = "Commemorative")

# All features ------------------------------------------------------------

# intersection of language sets

morninglangs <- morninggenlang %>%
  select(gltc_lang) %>%
  mutate(set = 'morning') %>%
  distinct()

farewelllangs <- farewelllang %>%
  select(gltc_lang) %>%
  mutate(set = 'farewell') %>%
  distinct()

commlangs <- commlang %>%
  select(gltc_lang) %>%
  mutate(set = 'comm') %>%
  distinct()

mf <- full_join(morninglangs, farewelllangs, by = "gltc_lang")
mf2 <- full_join(mf, commlangs, by = "gltc_lang") %>%
  filter(is.na(set.x) | is.na(set.y) | is.na(set)) %>%
  mutate(lang = lang.gltc(gltc_lang))


# make counts per feature per zone

#morning
morningzone <- morninggenlang %>%
  mutate(morningavar = case_when(value1 == "Did you wake up?" ~ 1,
                          TRUE ~ NA)) %>%
  mutate(morningazerbaijani = case_when(value1 == "Good morning" & value2 == "Azerbaijani" ~ 1,
                          TRUE ~ NA)) %>%
  select(lang, idiom, gltc_lang, lat, lon, morningavar, morningazerbaijani)

#farewell
farewellzone <- farewelllang %>%
  mutate(farewellavar = case_when(value2 == "straight way" ~ 1,
                                  TRUE ~ NA))%>%
  mutate(farewellazerbaijani = case_when(value2 == "jaʁur/juʁur/uʁur/uɣur" ~ 1,    
                                         TRUE ~ NA)) %>%
  select(lang, idiom, gltc_lang, lat, lon, farewellavar, farewellazerbaijani)

#commemorative
commzone <- commlang %>%
  mutate(commavar = case_when(value2 == "yes" ~ 1,
                              TRUE ~ NA)) %>%
  mutate(commazerbaijani = case_when(value1 == "yes" ~ 1,
                                     TRUE ~ NA)) %>%
  select(lang, idiom, gltc_lang, lat, lon, commavar, commazerbaijani) %>%
  filter(!is.na(gltc_lang)) %>%
  mutate(idiom = case_when(lang == "Tsez" ~ "Tsez", 
                           TRUE ~ idiom)) %>%
  filter(!is.na(idiom))

#combine first two zones
zones <- full_join(morningzone, farewellzone, by = "idiom") %>%
  mutate(lang = coalesce(lang.x, lang.y)) %>%
  mutate(gltc_lang = coalesce(gltc_lang.x, gltc_lang.y)) %>%
  mutate(lat = coalesce(lat.x, lat.y)) %>%
  mutate(lon = coalesce(lon.x, lon.y)) %>%
  select(lang, idiom, gltc_lang, lat, lon,
         morningavar, morningazerbaijani, farewellavar, farewellazerbaijani)

#add third zone and count scores
zones3 <- full_join(zones, commzone, by = "idiom") %>%
  mutate(lang = coalesce(lang.x, lang.y)) %>%
  mutate(gltc_lang = coalesce(gltc_lang.x, gltc_lang.y)) %>%
  mutate(lat = coalesce(lat.x, lat.y)) %>%
  mutate(lon = coalesce(lon.x, lon.y)) %>%
  select(lang, gltc_lang, lat, lon,
         morningavar, morningazerbaijani, farewellavar, farewellazerbaijani,
         commavar, commazerbaijani)%>%
  group_by(lang, gltc_lang, lat, lon)%>%
  summarise(across(everything(), .f = sum, na.rm = TRUE)) %>%
  mutate(avartotal = rowSums(across(c(morningavar, farewellavar, commavar)), na.rm=TRUE)) %>%
  mutate(azerbaijanitotal = rowSums(across(c(morningazerbaijani, farewellazerbaijani, commazerbaijani)), na.rm=TRUE)) %>%
  mutate(influencetotal = rowSums(across(c(avartotal, azerbaijanitotal)), na.rm=TRUE))

#write_xlsx(zones3, "mapsummary_20240816.xlsx")

#avar zone
map.feature(lang.gltc(zones3$gltc_lang),
            features = zones3$avartotal,
            color = c("white", "#3b8bc6"),
            width = 10,
            legend = FALSE,
            density.estimation = as.character(zones3$avartotal), #not working for some weird reason
            density.width = 0.5,
            density.estimation.color = c("white", "#3b8bc6"),
            density.title = "Avar zone",
            latitude = zones3$lat,
            longitude = zones3$lon,
            tile = 'Esri.WorldGrayCanvas')

#azerbaijani zone
map.feature(lang.gltc(zones3$gltc_lang),
            features = zones3$azerbaijanitotal,
            color = c("white", "#f78716"),
            width = 10,
            legend = FALSE,
            density.estimation = as.character(zones3$azerbaijanitotal), #not working for some weird reason
            density.width = 0.5,
            density.estimation.color = c("white", "#f78716"),
            density.title = "Azerbaijani zone",
            latitude = zones3$lat,
            longitude = zones3$lon,
            tile = 'Esri.WorldGrayCanvas')

#influence zone
map.feature(lang.gltc(zones3$gltc_lang),
            features = zones3$influencetotal,
            color = c("#35a432", "white"),
            width = 10,
            legend = FALSE,
            density.estimation = as.character(zones3$influencetotal), #not working for some weird reason
            density.width = 0.5,
            density.estimation.color = c("#35a432", "white"),
            density.title = "Other zone",
            latitude = zones3$lat,
            longitude = zones3$lon,
            tile = 'Esri.WorldGrayCanvas')

