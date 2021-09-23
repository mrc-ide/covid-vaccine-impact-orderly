
#regions by ISO3 code
AFR <- countrycode(
  c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi",
    "Cabo Verde", "Cameroon", "Central African Republic", "Chad", "Comoros",
    "Congo", "Côte d’Ivoire", "Democratic Republic of the Congo",
    "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia",
    "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia",
    "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Mozambique",
    "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", "Senegal",
    "Seychelles", "Sierra Leone", "South Africa", "South Sudan", "Togo",
    "Uganda", "United Republic of Tanzania", "Zambia", "Zimbabwe"),
  origin = "country.name", destination = "iso3c"
)
AMR <- c(
  countrycode(
    stringr::str_split("Antigua and Barbuda, Argentina, Bahamas, Barbados, Belize, Bolivia (Plurinational State of), Brazil, Canada, Chile, Colombia, Costa Rica, Cuba, Dominica, Dominican Republic, Ecuador, El Salvador, Grenada, Guatemala, Guyana, Haiti, Honduras, Jamaica, Mexico, Nicaragua, Panama, Paraguay, Peru, Saint Kitts and Nevis, Saint Lucia, Saint Vincent and the Grenadines, Suriname, Trinidad and Tobago, United States of America, Uruguay, Venezuela (Bolivarian Republic of)",
                       ", ")[[1]],
    origin = "country.name", destination = "iso3c"
  ),
  "GUF" #add french guiana to region
)
SEAR <- countrycode(
  stringr::str_split("Bangladesh, Bhutan, Democratic People’s Republic of Korea, India, Indonesia, Maldives, Myanmar, Nepal, Sri Lanka, Thailand, Timor-Leste",
                     ", ")[[1]],
  origin = "country.name", destination = "iso3c"
)
EUR <- countrycode(
  stringr::str_split("Albania, Andorra, Armenia, Austria, Azerbaijan, Belarus, Belgium, Bosnia and Herzegovina, Bulgaria, Croatia, Cyprus, Czechia, Denmark, Estonia, Finland, France, Georgia, Germany, Greece, Hungary, Iceland, Ireland, Israel, Italy, Kazakhstan, Kyrgyzstan, Latvia, Lithuania, Luxembourg, Malta, Monaco, Montenegro, Netherlands, North Macedonia, Norway, Poland, Portugal, Republic of Moldova, Romania, Russian Federation, San Marino, Serbia, Slovakia, Slovenia, Spain, Sweden, Switzerland, Tajikistan, Turkey, Turkmenistan, Ukraine, United Kingdom of Great Britain and Northern Ireland, Uzbekistan",
                     ", ")[[1]],
  origin = "country.name", destination = "iso3c"
)
EMR <- c(
  countrycode(
    stringr::str_split("Afghanistan, Bahrain, Djibouti, Egypt, Iran (Islamic Republic of), Iraq, Jordan, Kuwait, Lebanon, Libya, Morocco, Oman, Pakistan, Qatar, Saudi Arabia, Somalia, Sudan, Syrian Arab Republic, Tunisia, United Arab Emirates, Yemen",
                       ", ")[[1]],
    origin = "country.name", destination = "iso3c"
  ),
  "PSE"
)
WPR <- c(
  countrycode(
    stringr::str_split("Australia, Brunei Darussalam, Cambodia, China, Cook Islands, Fiji, Japan, Kiribati, Lao People’s Democratic Republic, Malaysia, Marshall Islands, Micronesia (Federated States of), Mongolia, Nauru, New Zealand, Niue, Palau, Papua New Guinea, Philippines, Republic of Korea, Samoa, Singapore, Solomon Islands, Tonga, Tuvalu, Vanuatu, Viet Nam",
                       ", ")[[1]],
    origin = "country.name", destination = "iso3c"
  ),
  "HKG", "TWN"
)
#create dataframe
whoRegion <- data.frame(
  iso3c = unique(
    c(AFR, AMR, SEAR, EUR, EMR, WPR)
  )
) %>%
  mutate(who_region = case_when(
    iso3c %in% AFR ~ "AFR",
    iso3c %in% AMR ~ "AMR",
    iso3c %in% SEAR ~ "SEAR",
    iso3c %in% EUR ~ "EUR",
    iso3c %in% EMR ~ "EMR",
    iso3c %in% WPR ~ "WPR",
  ))
saveRDS(
  whoRegion,
  "who_region.Rds"
)
