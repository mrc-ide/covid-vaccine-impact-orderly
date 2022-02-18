##GEOJSON map
#download map
worldsf_raw <- geojson_sf(
  "https://datahub.io/core/geo-countries/r/countries.geojson"
)
worldsf <- rename(worldsf_raw, iso3c = ISO_A3, country = ADMIN) %>% select(iso3c, geometry, country)
#split France in to France and French Guyana + remove Anarctica

##French Guiana:
#currently part of France so we need to access that
#francePoly <- st_geometry(filter(worldsf, iso3c == "FRA"))
#by looking at the various polygons this is GUF
#frenchGuianaPoly <- st_sfc(st_polygon(francePoly[[1]][[4]]))
#remove Guiana from France
#francePoly[[1]][[4]] <- NULL
#combine new countries
#newCountries <- st_sf(iso3c = c("FRA","GUF"), geometry = c(francePoly, frenchGuianaPoly),
#                      country = c("France", "French Guiana"))
#add back in
worldsf_mod <- worldsf %>%
  filter(iso3c != "ATA")#%>%
  # filter(iso3c != "FRA" & iso3c != "ATA") %>%
  # rbind(newCountries)
#store as rds file
saveRDS(
  worldsf_mod,
  "worldsf.Rds"
)
