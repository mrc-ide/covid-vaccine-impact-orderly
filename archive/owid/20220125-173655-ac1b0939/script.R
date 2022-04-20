owid <- read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
#Only need a few columns
owid <- owid %>%
  rename(obsDate = date, iso3c = iso_code) %>% #rename date to avoid conflicts
  filter(obsDate <= date & nchar(iso3c)==3) %>% #only interested in data on or before this date
  select(obsDate, iso3c, total_deaths, total_cases, total_tests,
         total_vaccinations, people_vaccinated) #only want deaths, vaccine doses and people with atleast one dose

#this is missing data for french Guiana so we get that from the French Gov website
#update link at https://www.data.gouv.fr/fr/datasets/donnees-relatives-aux-personnes-vaccinees-contre-la-covid-19-1/
#if needed
GUF <- read.csv(
  "https://www.data.gouv.fr/fr/datasets/r/375b7a34-9ae4-472a-9566-8eb7108c9add",
  sep = ";"
) %>% #only get guana data:
  filter(reg == 3 & jour <= date) %>%
  mutate(iso3c = "GUF", total_vaccinations = n_cum_dose1 + n_cum_complet,
         total_deaths = NA, total_cases = NA, total_tests = NA) %>%
  rename(obsDate = jour, people_vaccinated = n_cum_dose1) %>%
  select(iso3c, obsDate, total_deaths, total_vaccinations, people_vaccinated, total_cases, total_tests)
#add to OWID
owid <- rbind(owid, GUF)

#store this in derived since it has been modified
saveRDS(
  owid,
  "owid.Rds"
)
