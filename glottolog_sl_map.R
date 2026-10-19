
# Load packages -----------------------------------------------------------

library(tidyverse)
library(rnaturalearth)
library(sf)



# Read and process data ---------------------------------------------------

# Read and combine Glottolog sign language data
sl_glottolog <- 
  read_csv("https://raw.githubusercontent.com/glottolog/glottolog-cldf/master/cldf/values.csv") |> 
  filter(Value == "Sign_Language") |> 
  select(Language_ID) |> 
  left_join(
    read_csv("https://raw.githubusercontent.com/glottolog/glottolog-cldf/master/cldf/languages.csv"), by = join_by("Language_ID" == "ID")
  )

# Convert Glottolog data to SF object, remove points missing coordinates
sl_glottolog_sf <- 
  sl_glottolog |> 
  filter(!is.na(Longitude), !is.na(Latitude)) |> 
  mutate(geometry = map2_vec(Longitude,
                             Latitude,
                             \(x, y) st_geometry(st_point(c(x, y))))) |> 
  st_as_sf(crs = 4326)

# Map data without Antarctica
world <- 
  ne_countries(scale = "medium") |> 
  filter(subregion != "Antarctica")



# Plot map ----------------------------------------------------------------

# Plot Equal Earth map with sign language data
ggplot() +
  geom_sf(data = world, 
          color = "grey80", fill = "grey80", linewidth = .1) +
  geom_sf(data = sl_glottolog_sf, 
          alpha = .5, shape = 21, stroke = 1, size = .75) + 
  coord_sf(crs = "+proj=eqearth") +
  theme_void(paper = "white")



# Save plot ---------------------------------------------------------------

ggsave("Downloads/glottolog_sl_map.png", width = 9, height = 4, units = "in", dpi = 600)
