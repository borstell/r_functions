
# Load packages -----------------------------------------------------------

library(tidyverse)



# Sign language data ------------------------------------------------------

# Attempt to classify micro communities (not a straightforward task!)
micro <- 
  tibble(
    Name = c("Albarradas Sign Language", "Alipur Sign Language", "Al-Sayyid Bedouin Sign Language",
             "Amami O Shima Sign Language", "Angami Naga Sign Language", "Arab El-Naim Sign Language",
             "Ban Khor Sign Language", "Bay Islands Sign Language", "Berbey Sign Language",
             "Bribri Sign Language", "Brunca Sign Language", "Bura Sign Language",
             "Cena", "Central Taurus Sign Language", "Dadhkai Village Sign Language", 
             "Douentza Sign Language", "Ein Mahal Sign Language", "Ein Mahal Sign Language", 
             "Extreme North Cameroon Sign Language", "Far North Queensland Indigenous Sign Language", "Ghandruk Sign Language",
             "Ghardaia Sign Language", "Hawai'i Sign Language", "Ibokun Sign Language", 
             "Inuit Sign Language", "Isyarat Lama Asli Bali", "Jamaican Country Sign Language", 
             "Jhyankot Sign Language", "Jumla Sign Language", "Kafr Qasem Sign Language",
             "Kailge Sign Language", "Kajana Sign Language", "Kata Kolok",
             "Keresan Pueblo Indian Sign Language", "Langue des Signes de Bouakako", "Língua de sinais de Caiçara",
             "Língua de sinais do Uiramutã", "Língua de sinais Fortalezinha", "Macushi Sign Language", 
             "Magajingari Sign Language", "Marajo Sign Language", "Mardin Sign Language",
             "Maunabudhuk and Bodhe Local Sign", "Mavea Sign Language", "Maxakali Sign Language",
             "Mbour Sign Language", "Meemul Ch'aab'al", "Yucatec Maya Sign Language",
             "Mehek Sign Language", "Miriwoong Sign Language", "Miyakubo Sign Language",
             "Mofu-Gudur Sign Language", "Mount Avejaha Sign Language", "Nanabin Sign Language",
             "Nebaj Shared Homesign Systems", "Nueva Vida Maijuna Sign Language", "Martha's Vineyard Sign Language",
             "Old Kentish Sign Language", "Chiangmai Sign Language", "Old Bangkok Sign Language",
             "Papiu Yanomama Sign Language", "Old Cayman Sign Language", "Providencia Sign Language",
             "San Juan Quiahije Chatino Sign Language", "Sao Tome and Principe Sign Language", "Sinasina Sign Language",
             "Sivia Sign Language", "South Rupununi Sign", "Tebul Sign Language",
             "Terena Sign Language", "Urubú-Kaapor Sign Language", "Wanib Sign Language",
             "Zinacantec family homesign"),
    Category = "Micro")

# Unclear cases
unclear <- 
  tibble(
    Name = c("Douz Sign Language", "Isyarat Lama Cicendo", "Langue des Signes Zairoise", 
             "Wimbum Sign Language", "Yolngu Sign Language"),
    Category = "Unclear")

# Glottolog category
auxiliary <- 
  tibble(
    Name = c("Australian Aborigines Sign Language", "Baraninsky Armenian Sign Language", "Monastic Sign Language",
             "Plains Indian Sign Language"),
    Category = "Auxiliary"
  )

# Glottolog category
pidgin <- 
  tibble(Name = c("International Sign"),
         Category = "Pidgin")

# Combine all categorizations
sl_classification <- 
  bind_rows(micro, unclear, auxiliary, pidgin)

# Combine with Glottolog data
sl_glottolog <- 
  read_csv("https://raw.githubusercontent.com/glottolog/glottolog-cldf/master/cldf/values.csv") |> 
  filter(Value == "Sign_Language") |> 
  select(Language_ID) |> 
  left_join(
    read_csv("https://raw.githubusercontent.com/glottolog/glottolog-cldf/master/cldf/languages.csv"), by = join_by("Language_ID" == "ID")
  ) |> 
  left_join(sl_classification, by = join_by("Name" == "Name")) |> 
  mutate(Longitude = case_when(
    Longitude < -30 ~ Longitude + 360,
    .default = Longitude
  )) |> 
  
  # Assume Macro for any others (may be incorrect!)
  mutate(Category = case_when(
    is.na(Category) ~ "Macro",
    .default = Category
  ))



# Plot map ----------------------------------------------------------------

# Map data without Antarctica, and moved for Pacific-centered projection
world <- map_data("world") |> 
  filter(region != "Antarctica") |> 
  mutate(long = case_when(
    long < -30 | region == "Greenland" ~ long + 360,
    .default = long
  ))



# Plot map with sign language data (some coordinates missing)
ggplot() +
  geom_polygon(data = world, 
               aes(long, lat, group = group), 
               fill = "grey90") +
  geom_point(data = sl_glottolog |> filter(Category %in% c("Micro", "Macro")),
             aes(Longitude, Latitude, color = Category),
             alpha = .8) +
  coord_sf() +
  labs(color = NULL, title = "Macro and micro community sign languages",
       caption = "Data: Glottolog 5.0") +
  scale_color_manual(values = c("dodgerblue3", "orange2")) +
  theme_void(base_size = 15) +
  theme(legend.position = "bottom",
        plot.title.position = "plot")
