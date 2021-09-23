
#set up income group data
LIC <- countrycode(
  c("Afghanistan", "Guinea-Bissau", "Somalia", "Burkina Faso", "Korea, Dem. People's Rep", "South Sudan", "Burundi", "Liberia", "Sudan", "Central African Republic", "Madagascar", "Syrian Arab Republic", "Chad", "Malawi", "Togo", "Congo, Dem. Rep", "Mali", "Uganda", "Eritrea", "Mozambique", "Yemen, Rep.", "Ethiopia", "Niger", "Gambia, The", "Rwanda", "Guinea", "Sierra Leone"),
  origin = "country.name", destination = "iso3c")
LMIC <- countrycode(
  c("Angola", "Honduras", "Philippines", "Algeria", "India", "Samoa", "Bangladesh", "Indonesia", "São Tomé and Principe", "Belize", "Iran, Islamic Rep", "Senegal", "Benin", "Kenya", "Solomon Islands", "Bhutan", "Kiribati", "Sri Lanka", "Bolivia", "Kyrgyz Republic", "Tanzania", "Cabo Verde", "Lao PDR", "Tajikistan", "Cambodia", "Lesotho", "Timor-Leste", "Cameroon", "Mauritania", "Tunisia", "Comoros", "Micronesia, Fed. Sts.", "Ukraine", "Congo, Rep.", "Mongolia", "Uzbekistan", "Côte d'Ivoire", "Morocco", "Vanuatu", "Djibouti", "Myanmar", "Vietnam", "Egypt, Arab Rep.", "Nepal", "West Bank and Gaza", "El Salvador", "Nicaragua", "Zambia", "Eswatini", "Nigeria", "Zimbabwe", "Ghana", "Pakistan", "Haiti", "Papua New Guinea"),
  origin = "country.name", destination = "iso3c"
)
UMIC <- c(
  countrycode(
    c("Albania", "Gabon", "Namibia", "American Samoa", "Georgia", "North Macedonia", "Argentina", "Grenada", "Panama", "Armenia", "Guatemala", "Paraguay", "Azerbaijan", "Guyana", "Peru", "Belarus", "Iraq", "Romania", "Bosnia and Herzegovina", "Jamaica", "Russian Federation", "Botswana", "Jordan", "Serbia", "Brazil", "Kazakhstan", "South Africa", "Bulgaria", "St. Lucia", "China", "Lebanon", "St. Vincent and the Grenadines", "Colombia", "Libya", "Suriname", "Costa Rica", "Malaysia", "Thailand", "Cuba", "Maldives", "Tonga", "Dominica", "Marshall Islands", "Turkey", "Dominican Republic", "Mauritius", "Turkmenistan", "Equatorial Guinea", "Mexico", "Tuvalu", "Ecuador", "Moldova", "Fiji", "Montenegro"),
    origin = "country.name", destination = "iso3c"
  ),
  "VEN" #give venuzela its old income group as its NA atm
)
HIC <- c(
  countrycode(
    c("Andorra", "Greece", "Poland", "Antigua and Barbuda", "Greenland", "Portugal", "Aruba", "Guam", "Puerto Rico", "Australia", "Hong Kong SAR, China", "Qatar", "Austria", "Hungary", "San Marino", "Bahamas, The", "Iceland", "Saudi Arabia", "Bahrain", "Ireland", "Seychelles", "Barbados", "Isle of Man", "Singapore", "Belgium", "Israel", "Sint Maarten", "Bermuda", "Italy", "Slovak Republic", "British Virgin Islands", "Japan", "Slovenia", "Brunei Darussalam", "Korea, Rep", "Spain", "Canada", "Kuwait", "St. Kitts and Nevis", "Cayman Islands", "Latvia", "Liechtenstein", "Sweden", "Chile", "Lithuania", "Switzerland", "Croatia", "Luxembourg", "Taiwan, China", "Curaçao", "Macao SAR, China", "Trinidad and Tobago", "Cyprus", "Malta", "Turks and Caicos Islands", "Czech Republic", "Monaco", "United Arab Emirates", "Denmark", "Nauru", "United Kingdom", "Estonia", "Netherlands", "United States", "Faroe Islands", "New Caledonia", "Uruguay", "Finland", "New Zealand", "Virgin Islands (U.S.)", "France", "Northern Mariana Islands", "French Polynesia", "Norway", "Germany", "Oman", "Gibraltar", "Palau"),
    origin = "country.name", destination = "iso3c"
  ),
  "GUF" # French Guiana HIC as part of France
)
income_group <- data.frame(iso3c = unique(c(
  LIC, LMIC, UMIC, HIC
))) %>%
  mutate(income_group = case_when(
    iso3c %in% LIC ~ 4,
    iso3c %in% LMIC ~ 3,
    iso3c %in% UMIC ~ 2,
    iso3c %in% HIC ~ 1
  ),
  income_group = factor(income_group,
                        levels = seq(4),
                        labels = c(
                          "HIC",
                          "UMIC",
                          "LMIC",
                          "LIC"
                        ), ordered = T)
  )
#save data set
saveRDS(
  income_group,
  "income_group.Rds"
)
