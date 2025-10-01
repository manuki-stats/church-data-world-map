# ---- Load Required Libraries ----
# All packages are pre-installed in the Docker image
library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(sf)
library(DT)
library(shinythemes)
library(lwgeom)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)
library(webshot)
library(writexl)
library(plotly)
library(shinyjs)
library(viridisLite)
library(ggplot2)
library(htmlwidgets)
library(purrr)

useShinyjs()

# ---- Docker Instructions ----
# Set Shiny app to listen on all interfaces and a specific port for Docker compatibility.

options(shiny.host = "0.0.0.0")
options(shiny.port = 3838) # Also 8180 is a valid option


# ---- Load the Data ----
# Update paths for Docker environment
if (file.exists("/srv/shiny-server/final_geo_table.csv")) {
  # Docker environment
  setwd("/srv/shiny-server")
  data <- read.csv("final_geo_table.csv", check.names = FALSE)
  abbreviations_file <- "variable_abbreviations.csv"
} else {
  # Local development environment
  path_outputs <- "C:/Users/schia/Documents/GitHub/Consulting_Catholic_Church"
  #path_outputs <- "C:/Users/soffi/Documents/Consulting_Catholic_Church"
  setwd(path_outputs)
  data <- read.csv("final_geo_table.csv", check.names = FALSE)
  abbreviations_file <- file.path(path_outputs, "variable_abbreviations.csv")
}

# ---- Define Variable Abbreviations ----
if (!file.exists(abbreviations_file)) {
  stop("Variable abbreviations CSV file not found at: ", abbreviations_file)
}
abbreviations_df <- read.csv(abbreviations_file, stringsAsFactors = FALSE, check.names = FALSE)
variable_abbreviations <- setNames(abbreviations_df$abbreviation, abbreviations_df$variable_name)

# ---- Data Filtering Functions ----
# Function to check if a column has non-NA data at the country level.

has_country_data <- function(col_data, region_type) {
  country_data <- col_data[region_type == "Country"]
  return(sum(!is.na(country_data)) > 0)
}


# ---- Identify and Filter Columns with Country Data ----
# Apply the function to identify columns with country-level data.

cols_with_country_data <- sapply(names(data), function(col_name) {
  if (is.numeric(data[[col_name]])) {
    has_country_data(data[[col_name]], data$`Region type`)
  } else {
    TRUE
  }
})

# Filter the dataset to retain only columns with country data.
data_filtered <- data[, cols_with_country_data]


# ---- Separate Data into Macroregions and Countries ----
# Extract macroregion data and rename the region column for consistency.

data_macroregions <- data_filtered %>%
  filter(`Region type` == "Macroregion") %>%
  rename(macroregion = Region)


# ---- Safe Division Helper Function ----
# Helper function to perform division safely, avoiding NA or division by zero.

safe_div <- function(num, den, scale = 1) {
  ifelse(is.na(num) | is.na(den) | den == 0, NA_real_, (num / den) * scale)
}

# ---- Updated Macroregion Mapping Function ----
assign_macroregion <- function(country_names) {
  # Africa
  africa <- c(
    "Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde", 
    "Cameroon", "Central African Rep.", "Chad", "Comoros", "Congo", "Dem. Rep. Congo", 
    "Côte d'Ivoire", "Djibouti", "Egypt", "Eritrea", "eSwatini", "Ethiopia", "Gabon", 
    "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Eq. Guinea", "Kenya", "Lesotho", 
    "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", 
    "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Réunion", "Rwanda", 
    "W. Sahara", "Saint Helena", "São Tomé and Principe", "Senegal", "Seychelles", 
    "Sierra Leone", "Somalia", "South Africa", "S. Sudan", "Sudan", "Tanzania", 
    "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe"
  )
  
  # North America
  north_america <- c(
    "Bermuda", "Canada", "Greenland", "St. Pierre and Miquelon", "United States of America"
  )
  
  # Central America (mainland + Antilles / Caribbean)
  central_america <- c(
    # Mainland
    "Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Mexico", 
    "Nicaragua", "Panama",
    # Antilles / Caribbean islands
    "Anguilla", "Antigua and Barbuda", "Aruba", "Bahamas", "Barbados", "Cayman Is.", 
    "Cuba", "Dominica", "Dominican Rep.", "Grenada", "Guadeloupe", "Haiti", "Jamaica", 
    "Martinique", "Montserrat", "Netherlands Antilles", "Puerto Rico", "St. Kitts and Nevis", 
    "Saint Lucia", "St. Vin. and Gren.", "Trinidad and Tobago", "Turks and Caicos Is.", 
    "British Virgin Is.", "U.S. Virgin Is."
  )
  
  # South America
  south_america <- c(
    "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Falkland Is.", 
    "French Guiana", "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela"
  )
  
  # Middle East Asia
  middle_east_asia <- c(
    "Afghanistan", "Cyprus", "Iran", "Iraq", "Israel", "Jordan", "Lebanon", "Syria", "Turkey"
  )
  
  # South and Far East Asia
  south_far_east_asia <- c(
    "Bahrain", "Bangladesh", "Bhutan", "Brunei", "Cambodia", "China", "Hong Kong", 
    "Macao", "Taiwan", "India", "Indonesia", "Japan", "Kazakhstan", "North Korea", 
    "South Korea", "Kuwait", "Kyrgyzstan", "Laos", "Malaysia", "Maldives", "Mongolia", 
    "Myanmar", "Nepal", "Oman", "Pakistan", "Philippines", "Qatar", "Russia", 
    "Saudi Arabia", "Singapore", "Sri Lanka", "Tajikistan", "Thailand", "Timor-Leste", 
    "Turkmenistan", "United Arab Emirates", "Uzbekistan", "Vietnam", "Yemen"
  )
  
  # Europe
  europe <- c(
    "Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", "Belgium", 
    "Fed. of Bos. & Herz.", "Bulgaria", "Croatia", "Czechia", "Denmark", "Estonia", 
    "Faeroe Is.", "Finland", "France", "Georgia", "Germany", "Great Britain", "Greece", 
    "Hungary", "Iceland", "Ireland", "Italy", "Kosovo", "Latvia", "Liechtenstein", 
    "Lithuania", "Luxembourg", "North Macedonia", "Malta", "Moldova", "Monaco", 
    "Montenegro", "Netherlands", "Norway", "Poland", "Portugal", "Romania", 
    "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", "Svalbard Is.", 
    "Sweden", "Switzerland", "Ukraine"
  )
  
  # Oceania
  oceania <- c(
    "Australia", "Cook Is.", "Fiji", "Guam", "Kiribati", "Marshall Is.", "Micronesia", 
    "N. Mariana Is.", "Nauru", "New Caledonia", "New Zealand", "Niue", "Palau", 
    "Papua New Guinea", "Fr. Polynesia", "Samoa", "American Samoa", "Solomon Is.", 
    "Tokelau", "Tonga", "Tuvalu", "Vanuatu", "Wallis and Futuna Is."
  )
  
  dplyr::case_when(
    country_names %in% africa ~ "Africa",
    country_names %in% north_america ~ "North America",
    country_names %in% central_america ~ "Central America",
    country_names %in% south_america ~ "South America",
    country_names %in% middle_east_asia ~ "Middle East Asia",
    country_names %in% south_far_east_asia ~ "South and Far East Asia",
    country_names %in% europe ~ "Europe",
    country_names %in% oceania ~ "Oceania",
    TRUE ~ NA_character_
  )
}

# ---- Process and Merge Macroregions ----
# Standardize names, merge split regions, and filter out aggregates
data_macroregions <- data_macroregions %>%
  mutate(macroregion = case_when(
    macroregion %in% c("Central America (Mainland)", "Central America (Antilles)") ~ "Central America",
    macroregion %in% c("South East and Far East Asia", "South & Far East Asia") ~ "South and Far East Asia",
    macroregion == "Middle East" ~ "Middle East Asia",
    TRUE ~ macroregion
  )) %>%
  filter(!macroregion %in% c("World", "America", "Asia")) %>%
  mutate(Year = suppressWarnings(as.integer(Year))) %>%  # Ensure Year is integer, matching country processing
  group_by(macroregion, Year) %>%
  summarise(
    across(
      where(is.numeric),
      ~ if (all(is.na(.))) NA_real_ else sum(., na.rm = TRUE)
    ),
    .groups = "drop"
  )

# ---- Recompute Derived Variables for Macroregions ----
# Shorthand for data_macroregions to simplify recomputations.
dm <- data_macroregions
# Recompute density and rates, rounding to 2 decimal places.
if (all(c("Inhabitants per km^2", "Inhabitants in thousands", "Area in km^2") %in% names(dm))) {
  dm[["Inhabitants per km^2"]] <- round(safe_div(dm[["Inhabitants in thousands"]] * 1000, dm[["Area in km^2"]]), 2)
}
if (all(c("Catholics per 100 inhabitants", "Catholics in thousands", "Inhabitants in thousands") %in% names(dm))) {
  dm[["Catholics per 100 inhabitants"]] <- round(safe_div(dm[["Catholics in thousands"]], dm[["Inhabitants in thousands"]], 100), 2)
}
if (all(c("Inhabitants per pastoral centre", "Inhabitants in thousands", "Pastoral centres (total)") %in% names(dm))) {
  dm[["Inhabitants per pastoral centre"]] <- round(safe_div(dm[["Inhabitants in thousands"]] * 1000, dm[["Pastoral centres (total)"]]), 2)
}
if (all(c("Catholics per pastoral centre", "Catholics in thousands", "Pastoral centres (total)") %in% names(dm))) {
  dm[["Catholics per pastoral centre"]] <- round(safe_div(dm[["Catholics in thousands"]] * 1000, dm[["Pastoral centres (total)"]]), 2)
}
if (all(c("Pastoral centres per diocese", "Pastoral centres (total)", "Ecclesiastical territories (total)") %in% names(dm))) {
  dm[["Pastoral centres per diocese"]] <- round(safe_div(dm[["Pastoral centres (total)"]], dm[["Ecclesiastical territories (total)"]]), 2)
}
if (all(c("Parishes as share of total pastoral centres", "Parishes (total)", "Pastoral centres (total)") %in% names(dm))) {
  dm[["Parishes as share of total pastoral centres"]] <- round(safe_div(dm[["Parishes (total)"]], dm[["Pastoral centres (total)"]]), 2)
}
# Recompute mission stations share.
if (all(c("Mission stations with resident priest",
          "Mission stations without resident priest",
          "Pastoral centres (total)") %in% names(dm))) {
  dm[["Mission stations as share of total pastoral centres"]] <-
    round(safe_div(dm[["Mission stations with resident priest"]] +
                     dm[["Mission stations without resident priest"]],
                   dm[["Pastoral centres (total)"]]), 2)
}
if (all(c("Number of other pastoral centres as share of total pastoral centres", "Other pastoral centres", "Pastoral centres (total)") %in% names(dm))) {
  dm[["Number of other pastoral centres as share of total pastoral centres"]] <- round(safe_div(dm[["Other pastoral centres"]], dm[["Pastoral centres (total)"]]), 2)
}
# Compute total priests for burdens.
priests_total <- if ("Priests (diocesan and religious)" %in% names(dm)) {
  dm[["Priests (diocesan and religious)"]]
} else if (all(c("Diocesan priests (total)", "Religious priests") %in% names(dm))) {
  dm[["Diocesan priests (total)"]] + dm[["Religious priests"]]
} else {
  NA_real_
}
if ("Inhabitants per priest" %in% names(dm) && !all(is.na(priests_total))) {
  dm[["Inhabitants per priest"]] <- round(safe_div(dm[["Inhabitants in thousands"]] * 1000, priests_total), 2)
}
if ("Catholics per priest" %in% names(dm) && !all(is.na(priests_total))) {
  dm[["Catholics per priest"]] <- round(safe_div(dm[["Catholics in thousands"]] * 1000, priests_total), 2)
}
# Recompute sacraments per 1000 Catholics.
if (all(c("Infant baptisms (people up to 7 years old) per 1000 Catholics",
          "Infant baptisms (people up to 7 years old)", "Catholics in thousands") %in% names(dm))) {
  dm[["Infant baptisms (people up to 7 years old) per 1000 Catholics"]] <-
    round(safe_div(dm[["Infant baptisms (people up to 7 years old)"]], dm[["Catholics in thousands"]] * 1000, 1000), 2)
}
if (all(c("Marriages per 1000 Catholics", "Marriages", "Catholics in thousands") %in% names(dm))) {
  dm[["Marriages per 1000 Catholics"]] <- round(safe_div(dm[["Marriages"]], dm[["Catholics in thousands"]] * 1000, 1000), 2)
}
if (all(c("Confirmations per 1000 Catholics", "Confirmations", "Catholics in thousands") %in% names(dm))) {
  dm[["Confirmations per 1000 Catholics"]] <- round(safe_div(dm[["Confirmations"]], dm[["Catholics in thousands"]] * 1000, 1000), 2)
}
if (all(c("First Communions per 1000 Catholics", "First Communions", "Catholics in thousands") %in% names(dm))) {
  dm[["First Communions per 1000 Catholics"]] <- round(safe_div(dm[["First Communions"]], dm[["Catholics in thousands"]] * 1000, 1000), 2)
}
# Recompute shares.
if (all(c("Share of adult baptisms (people over 7 years old)", "Adult baptisms (people over 7 years old)", "Baptisms") %in% names(dm))) {
  dm[["Share of adult baptisms (people over 7 years old)"]] <- round(safe_div(dm[["Adult baptisms (people over 7 years old)"]], dm[["Baptisms"]]), 2)
}
if (all(c("Share of mixed marriages (among those celebrated with ecclesiastical rite)", "Mixed marriages", "Marriages") %in% names(dm))) {
  dm[["Share of mixed marriages (among those celebrated with ecclesiastical rite)"]] <- round(safe_div(dm[["Mixed marriages"]], dm[["Marriages"]]), 2)
}
# Recompute vocation/ordination/departure rates.
if (all(c("Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand inhabitants",
          "Candidates for diocesan and religious clergy in philosophy+theology centres", "Inhabitants in thousands") %in% names(dm))) {
  dm[["Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand inhabitants"]] <-
    round(safe_div(dm[["Candidates for diocesan and religious clergy in philosophy+theology centres"]],
                   dm[["Inhabitants in thousands"]] * 1000, 100000), 2)
}
if (all(c("Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand Catholics",
          "Candidates for diocesan and religious clergy in philosophy+theology centres", "Catholics in thousands") %in% names(dm))) {
  dm[["Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand Catholics"]] <-
    round(safe_div(dm[["Candidates for diocesan and religious clergy in philosophy+theology centres"]],
                   dm[["Catholics in thousands"]] * 1000, 100000), 2)
}
if (all(c("Ordination rate - Yearly ordinations per 100 philosophy+theology students for diocesan priesthood",
          "Yearly ordinations of diocesan priests", "Candidates for diocesan clergy in philosophy+theology centres") %in% names(dm))) {
  dm[["Ordination rate - Yearly ordinations per 100 philosophy+theology students for diocesan priesthood"]] <-
    round(safe_div(dm[["Yearly ordinations of diocesan priests"]],
                   dm[["Candidates for diocesan clergy in philosophy+theology centres"]], 100), 2)
}
if (all(c("Departures per 100 enrolled students in philosophy+theology centres for diocesan clergy",
          "Students in philosophy+theology centres for diocesan clergy who left seminary",
          "Candidates for diocesan clergy in philosophy+theology centres") %in% names(dm))) {
  dm[["Departures per 100 enrolled students in philosophy+theology centres for diocesan clergy"]] <-
    round(safe_div(dm[["Students in philosophy+theology centres for diocesan clergy who left seminary"]],
                   dm[["Candidates for diocesan clergy in philosophy+theology centres"]], 100), 2)
}
# Recompute philosophy+theology candidates per 100 priests.
if (all(c("Philosophy+theology candidates for diocesan and religious clergy per 100 priests",
          "Candidates for diocesan and religious clergy in philosophy+theology centres") %in% names(dm)) && !all(is.na(priests_total))) {
  dm[["Philosophy+theology candidates for diocesan and religious clergy per 100 priests"]] <-
    round(safe_div(dm[["Candidates for diocesan and religious clergy in philosophy+theology centres"]], priests_total, 100), 2)
}
# Compute lagged values for diocesan priest shares.
dm <- dm %>%
  arrange(macroregion, Year) %>%
  group_by(macroregion) %>%
  mutate(
    prev_inc_priests = lag(`Incardinated diocesan priests on January 1`, n = 1)
  ) %>%
  ungroup()
if (all(c("Yearly ordinations of diocesan priests as share of those incardinated on January 1",
          "Yearly ordinations of diocesan priests") %in% names(dm))) {
  dm[["Yearly ordinations of diocesan priests as share of those incardinated on January 1"]] <-
    round(safe_div(dm[["Yearly ordinations of diocesan priests"]], dm[["prev_inc_priests"]], 100), 2)
}
if (all(c("Yearly deaths of diocesan priests as share of those incardinated on January 1",
          "Yearly deaths of diocesan priests") %in% names(dm))) {
  dm[["Yearly deaths of diocesan priests as share of those incardinated on January 1"]] <-
    round(safe_div(dm[["Yearly deaths of diocesan priests"]], dm[["prev_inc_priests"]], 100), 2)
}
if (all(c("Yearly defections of diocesan priests as share of those incardinated at January 1",
          "Yearly defections of diocesan priests") %in% names(dm))) {
  dm[["Yearly defections of diocesan priests as share of those incardinated at January 1"]] <-
    round(safe_div(dm[["Yearly defections of diocesan priests"]], dm[["prev_inc_priests"]], 100), 2)
}
if (all(c("Yearly ordinations minus deaths and defections of diocesan priests as share of those incardinated on January 1",
          "Yearly ordinations of diocesan priests", "Yearly deaths of diocesan priests", "Yearly defections of diocesan priests") %in% names(dm))) {
  net_ord <- dm[["Yearly ordinations of diocesan priests"]] - dm[["Yearly deaths of diocesan priests"]] - dm[["Yearly defections of diocesan priests"]]
  dm[["Yearly ordinations minus deaths and defections of diocesan priests as share of those incardinated on January 1"]] <-
    round(safe_div(net_ord, dm[["prev_inc_priests"]], 100), 2)
}
# Recompute weighted averages.
if (all(c("Average area of ecclesiastical territories in km^2", "Area in km^2", "Ecclesiastical territories (total)") %in% names(dm))) {
  dm[["Average area of ecclesiastical territories in km^2"]] <-
    round(safe_div(dm[["Area in km^2"]], dm[["Ecclesiastical territories (total)"]]), 2)
}
if (all(c("Average diocesan area (in km^2)", "Area in km^2", "Ecclesiastical territories (total)") %in% names(dm))) {
  dm[["Average diocesan area (in km^2)"]] <-
    round(safe_div(dm[["Area in km^2"]], dm[["Ecclesiastical territories (total)"]]), 2)
}
# Recompute apostolic workforce share.
if (all(c("Priests and bishops as share of apostolic workforce",
          "Bishops (total)", "Catechists", "Lay missionaries") %in% names(dm)) &&
    ("Priests (diocesan and religious)" %in% names(dm) ||
     all(c("Diocesan priests (total)", "Religious priests") %in% names(dm)))) {
  priests_total <- if ("Priests (diocesan and religious)" %in% names(dm)) {
    dm[["Priests (diocesan and religious)"]]
  } else {
    dm[["Diocesan priests (total)"]] + dm[["Religious priests"]]
  }
  
  workforce <- priests_total + dm[["Bishops (total)"]] + dm[["Catechists"]] + dm[["Lay missionaries"]]
  dm[["Priests and bishops as share of apostolic workforce"]] <-
    round(safe_div(priests_total + dm[["Bishops (total)"]], workforce), 2)
}
# Commit all recomputed values back to data_macroregions.
data_macroregions <- dm

# Extract country data.
data_countries <- data_filtered %>%
  filter(`Region type` == "Country")
# ---- Load and Prepare World Map ----
# Load world map data from Natural Earth as an sf object.

world <- ne_countries(scale = "medium", type = "map_units", returnclass = "sf")

# Unify Somalia by merging Somalia and Somaliland geometries.
somalia_unified <- world %>%
  filter(admin %in% c("Somalia", "Somaliland")) %>%
  summarise(admin = "Somalia", name = "Somalia", geometry = st_union(geometry))

# Update the world map by removing original entries and adding the unified Somalia.
world <- world %>%
  filter(!admin %in% c("Somalia", "Somaliland")) %>%
  bind_rows(somalia_unified)

# --- Handle composites by merging map_units up to your country names ---

# Map Natural Earth sub-units -> your desired country labels
bridge_overrides <- tribble(
  ~name,                  ~Region_bridge,
  # Great Britain (merge UK home nations and territories)
  "England",              "Great Britain",
  "Scotland",             "Great Britain", 
  "Wales",                "Great Britain",
  "N. Ireland",           "Great Britain",
  "Guernsey",             "Great Britain",
  "Jersey",               "Great Britain", 
  "Isle of Man",          "Great Britain",
  
  # Belgium (merge language regions + Brussels)
  "Flemish",              "Belgium",
  "Walloon",              "Belgium",
  "Brussels",             "Belgium",
  
  # Antigua and Barbuda (merge the two islands)
  "Antigua",              "Antigua and Barbuda",
  "Barbuda",              "Antigua and Barbuda",
  
  # Netherlands Antilles (historical; merge successor units)
  "Curaçao",              "Netherlands Antilles",
  "Caribbean Netherlands","Netherlands Antilles",
  "Sint Maarten",         "Netherlands Antilles",
  "St-Martin",            "Netherlands Antilles", 
  "St-Barthélemy",        "Netherlands Antilles",
  
  # Bosnia and Herzegovina (merge all parts)
  "Bosnia and Herzegovina", "Fed. of Bos. & Herz.",
  "Republic of Srpska",   "Fed. of Bos. & Herz.",
  "Republika Srpska",     "Fed. of Bos. & Herz.",
  "Rep. Srpska",          "Fed. of Bos. & Herz.",
  
  # Serbia (merge with autonomous provinces)
  "Vojvodina",            "Serbia",
  
  # Tanzania (merge with Zanzibar)
  "Zanzibar",             "Tanzania",
  
  # Finland (merge with Åland)
  "Åland",                "Finland",
  
  # Portugal (merge with autonomous regions)
  "Azores",               "Portugal",
  "Madeira",              "Portugal",
  
  # France (merge with overseas territories)
  "Mayotte",              "France",
  
  # Papua New Guinea (merge with Bougainville)
  "Bougainville",         "Papua New Guinea",
  
  # Australia (merge with external territories)
  "Christmas I.",         "Australia",
  "Cocos Is.",            "Australia", 
  "Norfolk Island",       "Australia",
  "Ashmore and Cartier Is.", "Australia",
  "Heard I. and McDonald Is.", "Australia",
  
  # Norway (merge with dependencies)
  "Svalbard Is.",         "Norway",
  "Jan Mayen I.",         "Norway",
  
  # Palestine (if you have Palestine data - merge territories)
  "Gaza",                 "Palestine",
  "West Bank",            "Palestine",
  
  # Vatican (typically reports with Italy for statistical purposes)
  "Vatican",              "Italy"
)

# Add the bridge, defaulting to the map_unit's own name when not overridden
world_bridge <- world %>%
  left_join(bridge_overrides, by = "name") %>%
  mutate(Region_bridge = coalesce(Region_bridge, name))

# Dissolve geometries by Region_bridge so they match your data's 'country' labels
world_custom <- world_bridge %>%
  group_by(Region_bridge) %>%
  summarise(geometry = sf::st_union(geometry), .groups = "drop") %>%
  mutate(geometry = st_make_valid(geometry)) %>%  # Validate after union
  mutate(geometry = st_wrap_dateline(geometry, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=20")))  # Wrap after validation


# ---- Define Excluded Variables for Map Tab ----
# List variables to exclude from the "Map" tab's variable selection menu.

excluded_vars <- c(
  "Area in km^2",
  "prev_inc_priests",
  "Yearly ordinations of diocesan priests as share of those incardinated on January 1",
  "Yearly deaths of diocesan priests as share of those incardinated on January 1",
  "Yearly defections of diocesan priests as share of those incardinated at January 1",
  "Yearly ordinations minus deaths and defections of diocesan priests as share of those incardinated on January 1",
  "Ecclesiastical territories (total) - index numbers (base 2013 = 100)",
  "Pastoral centres (total) - index numbers (base 2013 = 100)",
  "Parishes (total) - index numbers (base 2013 = 100)",
  "Priests (diocesan and religious) - index numbers (base 2013 = 100)",
  "Diocesan priests (total) - index numbers (base 2013 = 100)",
  "Incardinated diocesan priests - index numbers (base 2013 = 100)",
  "Religious priests - index numbers (base 2013 = 100)",
  "Permanent deacons (diocesan and religious) - index numbers (base 2013 = 100)",
  "Non-priest religious men (with temporary or perpetual vows) - index numbers (base 2013 = 100)",
  "Religious women (with temporary or perpetual vows) - index numbers (base 2013 = 100)",
  "Candidates for diocesan and religious clergy in philosophy+theology centres - index numbers (base 2013 = 100)",
  "Students for diocesan and religious clergy in secondary schools - index numbers (base 2013 = 100)"
)


# ---- Create Macroregion Map Data ----
# Assign macroregions to world map using the corrected world_custom
world_with_macroregions <- world_custom %>%
  mutate(macroregion = assign_macroregion(Region_bridge)) %>%
  filter(!is.na(macroregion))

# Create dissolved macroregion polygons with proper geometry handling
macroregion_polygons <- world_with_macroregions %>%
  group_by(macroregion) %>%
  summarise(
    geometry = st_union(geometry),
    .groups = "drop"
  ) %>%
  mutate(geometry = st_make_valid(geometry)) %>%  # Validate after union
  mutate(geometry = st_wrap_dateline(geometry, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=20")))  # Wrap after validation

# Merge with macroregion data
map_data_macroregions <- left_join(
  macroregion_polygons,
  data_macroregions,
  by = "macroregion"
)

# Update allowed variables for macroregions (same logic as countries)
allowed_variables_macroregions <- setdiff(
  names(data_macroregions)[sapply(data_macroregions, is.numeric) & names(data_macroregions) != "Year"],
  excluded_vars
)

# Time series variables for macroregions
time_series_vars_macroregions <- allowed_variables_macroregions[
  sapply(allowed_variables_macroregions, function(var) {
    years <- data_macroregions %>% 
      filter(!is.na(.data[[var]])) %>% 
      pull(Year) %>% 
      unique()
    length(years) > 1
  })
]

# ---- Analyze Unmatched Country Names ----
# Identify countries in data that do not match the world map.

unmatched_in_data <- anti_join(data_countries, world, by = c("Region" = "name"))
#print(unmatched_in_data$Region)


# ---- Manual Country Name Corrections ----
# Define a vector of corrections for mismatched country names.



name_corrections <- c(
  "Bolivia (Plurinational State of)" = "Bolivia",
  "Bosnia and Herzegovina"           = "Fed. of Bos. & Herz.",
  "Brunei Darussalam"                = "Brunei",
  "Cayman Islands"                   = "Cayman Is.",
  "Central African Republic"         = "Central African Rep.",
  "China (Mainland)"                 = "China",
  "China (Taiwan)"                   = "Taiwan",
  "Cook Islands"                     = "Cook Is.",
  "Cote d'Ivoire"                    = "Côte d'Ivoire",
  "Dem. Peoples Rep. Of Korea"       = "North Korea",
  "Dem. Rep. of the Congo"           = "Dem. Rep. Congo",
  "Dominican Republic"               = "Dominican Rep.",
  "East Timor"                       = "Timor-Leste",
  "Equatorial Guinea"                = "Eq. Guinea",
  "Eswatini"                         = "eSwatini",
  "Faeroe Islands"                   = "Faeroe Is.",
  "Falkland Islands (Malvinas)"      = "Falkland Is.",
  "Fed. S. Micronesia"               = "Micronesia",
  "French Polynesia"                 = "Fr. Polynesia",
  "Gibraltar"                        = "Great Britain",
  "Hong Kong SAR"                    = "Hong Kong",
  "Iran (Islamic Rep. of)"           = "Iran",
  "Kazakchstan"                      = "Kazakhstan",
  "Macao SAR"                        = "Macao",
  "Macedonia (Rep. of North)"        = "North Macedonia",
  "Marshall Islands"                 = "Marshall Is.",
  "Montenegro (Rep. of)"             = "Montenegro",
  "N. Mariana Islands"               = "N. Mariana Is.",
  "Peoples Dem. Rep. Lao"            = "Laos",
  "Republic of Korea"                = "South Korea",
  "Republic of Moldova"              = "Moldova",
  "Reunion"                          = "Réunion",
  "Russian Fed. (in Europe)"         = "Russia",
  "Russian Federation (in Asia)"     = "Russia",
  "Saint Kitts and Nevis"            = "St. Kitts and Nevis",
  "Sao Tome and Principe"            = "São Tomé and Principe",
  "Solomon Islands"                  = "Solomon Is.",
  "South Sudan"                      = "S. Sudan",
  "St. Vincent and the Grenadines"   = "St. Vin. and Gren.",
  "Svalbard and Jan Mayen Is."       = "Svalbard Is.",   
  "Syrian Arab Republic"             = "Syria",
  "Turkiye"                          = "Turkey",
  "Turks and Caicos Islands"         = "Turks and Caicos Is.",
  "United Republic of Tanzania"      = "Tanzania",
  "United States"                    = "United States of America",
  "Venezuela (The Bolivar Rep. of)"  = "Venezuela",
  "Viet Nam"                         = "Vietnam",
  "Virgin Islands (Great. Brit.)"    = "British Virgin Is.",
  "Virgin Islands (U.S.A.)"          = "U.S. Virgin Is.",
  "Wallis and Futuna Islands"        = "Wallis and Futuna Is.",
  "Western Sahara"                   = "W. Sahara"
)

# ---- Complete Country Name Standardization ----
# First, apply name corrections to create the 'country' column
data_countries <- data_countries %>%
  mutate(
    country = case_when(
      Region %in% names(name_corrections) ~ name_corrections[Region],
      TRUE ~ Region
    )
  )

# Then apply bridge corrections to the newly created 'country' column
bridge_corrections <- setNames(bridge_overrides$Region_bridge, bridge_overrides$name)

data_countries <- data_countries %>%
  mutate(
    country = case_when(
      country %in% names(bridge_corrections) ~ bridge_corrections[country],
      TRUE ~ country
    )
  )

# Check for remaining unmatched countries
unmatched_in_data <- anti_join(
  data_countries,
  world_custom %>% st_drop_geometry(),
  by = c("country" = "Region_bridge")
)

#print(unique(unmatched_in_data$country))


# ---- Aggregate Numeric Values by Country and Year ----
# Ensure 'Year' column exists and is converted to integer.

stopifnot("Year" %in% names(data_countries))
data_countries <- data_countries %>%
  mutate(Year = suppressWarnings(as.integer(Year)))

# Identify numeric columns excluding 'Year'.
num_cols <- names(data_countries)[sapply(data_countries, is.numeric)]
num_cols <- setdiff(num_cols, "Year")

# Aggregate by summing numeric values for each country-year pair.
data_countries <- data_countries %>%
  group_by(country, Year) %>%
  summarise(
    across(
      all_of(num_cols),
      ~ if (all(is.na(.))) NA_real_ else sum(., na.rm = TRUE)
    ),
    .groups = "drop"
  )

dc <- data_countries

# ---- Define Allowed Variables for Map Tab ----
# Select numeric variables excluding 'Year' and those in excluded_vars.

allowed_variables <- setdiff(
  names(data_countries)[sapply(data_countries, is.numeric) & names(data_countries) != "Year"],
  excluded_vars
)


# ---- Build Coverage Matrix ----
build_coverage_matrix <- function(data, variables, countries, years) {
  coverage <- expand.grid(
    variable = variables,
    country = countries,
    year = years,
    stringsAsFactors = FALSE
  ) %>%
    left_join(data, by = c("country", "year" = "Year")) %>%
    mutate(
      has_data =purrr::map2_lgl(variable, seq_len(nrow(.)), function(var, idx) {
        value <- .[[var]][idx]
        !is.na(value)
      })
    ) %>%
    select(variable, country, year, has_data)
  
  return(coverage)
}

coverage_matrix <- build_coverage_matrix(
  data_countries, 
  allowed_variables, 
  unique(data_countries$country), 
  unique(data_countries$Year)
)

# Recompute density and rates, rounding to 2 decimal places.
if (all(c("Inhabitants per km^2", "Inhabitants in thousands", "Area in km^2") %in% names(dc))) {
  dc[["Inhabitants per km^2"]] <- round(safe_div(dc[["Inhabitants in thousands"]] * 1000, dc[["Area in km^2"]]), 2)
}
if (all(c("Catholics per 100 inhabitants", "Catholics in thousands", "Inhabitants in thousands") %in% names(dc))) {
  dc[["Catholics per 100 inhabitants"]] <- round(safe_div(dc[["Catholics in thousands"]], dc[["Inhabitants in thousands"]], 100), 2)
}
if (all(c("Inhabitants per pastoral centre", "Inhabitants in thousands", "Pastoral centres (total)") %in% names(dc))) {
  dc[["Inhabitants per pastoral centre"]] <- round(safe_div(dc[["Inhabitants in thousands"]] * 1000, dc[["Pastoral centres (total)"]]), 2)
}
if (all(c("Catholics per pastoral centre", "Catholics in thousands", "Pastoral centres (total)") %in% names(dc))) {
  dc[["Catholics per pastoral centre"]] <- round(safe_div(dc[["Catholics in thousands"]] * 1000, dc[["Pastoral centres (total)"]]), 2)
}
if (all(c("Pastoral centres per diocese", "Pastoral centres (total)", "Ecclesiastical territories (total)") %in% names(dc))) {
  dc[["Pastoral centres per diocese"]] <- round(safe_div(dc[["Pastoral centres (total)"]], dc[["Ecclesiastical territories (total)"]]), 2)
}
if (all(c("Parishes as share of total pastoral centres", "Parishes (total)", "Pastoral centres (total)") %in% names(dc))) {
  dc[["Parishes as share of total pastoral centres"]] <- round(safe_div(dc[["Parishes (total)"]], dc[["Pastoral centres (total)"]]), 2)
}

# Recompute mission stations share.
if (all(c("Mission stations with resident priest",
          "Mission stations without resident priest",
          "Pastoral centres (total)") %in% names(dc))) {
  dc[["Mission stations as share of total pastoral centres"]] <-
    round(safe_div(dc[["Mission stations with resident priest"]] +
                     dc[["Mission stations without resident priest"]],
                   dc[["Pastoral centres (total)"]]), 2)
}
if (all(c("Number of other pastoral centres as share of total pastoral centres", "Other pastoral centres", "Pastoral centres (total)") %in% names(dc))) {
  dc[["Number of other pastoral centres as share of total pastoral centres"]] <- round(safe_div(dc[["Other pastoral centres"]], dc[["Pastoral centres (total)"]]), 2)
}

# Compute total priests for burdens.
priests_total <- if ("Priests (diocesan and religious)" %in% names(dc)) {
  dc[["Priests (diocesan and religious)"]]
} else if (all(c("Diocesan priests (total)", "Religious priests") %in% names(dc))) {
  dc[["Diocesan priests (total)"]] + dc[["Religious priests"]]
} else {
  NA_real_
}
if ("Inhabitants per priest" %in% names(dc) && !all(is.na(priests_total))) {
  dc[["Inhabitants per priest"]] <- round(safe_div(dc[["Inhabitants in thousands"]] * 1000, priests_total), 2)
}
if ("Catholics per priest" %in% names(dc) && !all(is.na(priests_total))) {
  dc[["Catholics per priest"]] <- round(safe_div(dc[["Catholics in thousands"]] * 1000, priests_total), 2)
}

# Recompute sacraments per 1000 Catholics.
if (all(c("Infant baptisms (people up to 7 years old) per 1000 Catholics",
          "Infant baptisms (people up to 7 years old)", "Catholics in thousands") %in% names(dc))) {
  dc[["Infant baptisms (people up to 7 years old) per 1000 Catholics"]] <-
    round(safe_div(dc[["Infant baptisms (people up to 7 years old)"]], dc[["Catholics in thousands"]] * 1000, 1000), 2)
}
if (all(c("Marriages per 1000 Catholics", "Marriages", "Catholics in thousands") %in% names(dc))) {
  dc[["Marriages per 1000 Catholics"]] <- round(safe_div(dc[["Marriages"]], dc[["Catholics in thousands"]] * 1000, 1000), 2)
}
if (all(c("Confirmations per 1000 Catholics", "Confirmations", "Catholics in thousands") %in% names(dc))) {
  dc[["Confirmations per 1000 Catholics"]] <- round(safe_div(dc[["Confirmations"]], dc[["Catholics in thousands"]] * 1000, 1000), 2)
}
if (all(c("First Communions per 1000 Catholics", "First Communions", "Catholics in thousands") %in% names(dc))) {
  dc[["First Communions per 1000 Catholics"]] <- round(safe_div(dc[["First Communions"]], dc[["Catholics in thousands"]] * 1000, 1000), 2)
}

# Recompute shares.
if (all(c("Share of adult baptisms (people over 7 years old)", "Adult baptisms (people over 7 years old)", "Baptisms") %in% names(dc))) {
  dc[["Share of adult baptisms (people over 7 years old)"]] <- round(safe_div(dc[["Adult baptisms (people over 7 years old)"]], dc[["Baptisms"]]), 2)
}
if (all(c("Share of mixed marriages (among those celebrated with ecclesiastical rite)", "Mixed marriages", "Marriages") %in% names(dc))) {
  dc[["Share of mixed marriages (among those celebrated with ecclesiastical rite)"]] <- round(safe_div(dc[["Mixed marriages"]], dc[["Marriages"]]), 2)
}

# Recompute vocation/ordination/departure rates.
if (all(c("Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand inhabitants",
          "Candidates for diocesan and religious clergy in philosophy+theology centres", "Inhabitants in thousands") %in% names(dc))) {
  dc[["Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand inhabitants"]] <-
    round(safe_div(dc[["Candidates for diocesan and religious clergy in philosophy+theology centres"]],
                   dc[["Inhabitants in thousands"]] * 1000, 100000), 2)
}
if (all(c("Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand Catholics",
          "Candidates for diocesan and religious clergy in philosophy+theology centres", "Catholics in thousands") %in% names(dc))) {
  dc[["Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand Catholics"]] <-
    round(safe_div(dc[["Candidates for diocesan and religious clergy in philosophy+theology centres"]],
                   dc[["Catholics in thousands"]] * 1000, 100000), 2)
}
if (all(c("Ordination rate - Yearly ordinations per 100 philosophy+theology students for diocesan priesthood",
          "Yearly ordinations of diocesan priests", "Candidates for diocesan clergy in philosophy+theology centres") %in% names(dc))) {
  dc[["Ordination rate - Yearly ordinations per 100 philosophy+theology students for diocesan priesthood"]] <-
    round(safe_div(dc[["Yearly ordinations of diocesan priests"]],
                   dc[["Candidates for diocesan clergy in philosophy+theology centres"]], 100), 2)
}
if (all(c("Departures per 100 enrolled students in philosophy+theology centres for diocesan clergy",
          "Students in philosophy+theology centres for diocesan clergy who left seminary",
          "Candidates for diocesan clergy in philosophy+theology centres") %in% names(dc))) {
  dc[["Departures per 100 enrolled students in philosophy+theology centres for diocesan clergy"]] <-
    round(safe_div(dc[["Students in philosophy+theology centres for diocesan clergy who left seminary"]],
                   dc[["Candidates for diocesan clergy in philosophy+theology centres"]], 100), 2)
}

# Recompute philosophy+theology candidates per 100 priests.
if (all(c("Philosophy+theology candidates for diocesan and religious clergy per 100 priests",
          "Candidates for diocesan and religious clergy in philosophy+theology centres") %in% names(dc)) && !all(is.na(priests_total))) {
  dc[["Philosophy+theology candidates for diocesan and religious clergy per 100 priests"]] <-
    round(safe_div(dc[["Candidates for diocesan and religious clergy in philosophy+theology centres"]], priests_total, 100), 2)
}

# Compute lagged values for diocesan priest shares.
dc <- dc %>%
  arrange(country, Year) %>%
  group_by(country) %>%
  mutate(
    prev_inc_priests = lag(`Incardinated diocesan priests on January 1`, n = 1)
  ) %>%
  ungroup()
if (all(c("Yearly ordinations of diocesan priests as share of those incardinated on January 1",
          "Yearly ordinations of diocesan priests") %in% names(dc))) {
  dc[["Yearly ordinations of diocesan priests as share of those incardinated on January 1"]] <-
    round(safe_div(dc[["Yearly ordinations of diocesan priests"]], dc[["prev_inc_priests"]], 100), 2)
}
if (all(c("Yearly deaths of diocesan priests as share of those incardinated on January 1",
          "Yearly deaths of diocesan priests") %in% names(dc))) {
  dc[["Yearly deaths of diocesan priests as share of those incardinated on January 1"]] <-
    round(safe_div(dc[["Yearly deaths of diocesan priests"]], dc[["prev_inc_priests"]], 100), 2)
}
if (all(c("Yearly defections of diocesan priests as share of those incardinated at January 1",
          "Yearly defections of diocesan priests") %in% names(dc))) {
  dc[["Yearly defections of diocesan priests as share of those incardinated at January 1"]] <-
    round(safe_div(dc[["Yearly defections of diocesan priests"]], dc[["prev_inc_priests"]], 100), 2)
}
if (all(c("Yearly ordinations minus deaths and defections of diocesan priests as share of those incardinated on January 1",
          "Yearly ordinations of diocesan priests", "Yearly deaths of diocesan priests", "Yearly defections of diocesan priests") %in% names(dc))) {
  net_ord <- dc[["Yearly ordinations of diocesan priests"]] - dc[["Yearly deaths of diocesan priests"]] - dc[["Yearly defections of diocesan priests"]]
  dc[["Yearly ordinations minus deaths and defections of diocesan priests as share of those incardinated on January 1"]] <-
    round(safe_div(net_ord, dc[["prev_inc_priests"]], 100), 2)
}

# Recompute weighted averages.
if (all(c("Average area of ecclesiastical territories in km^2", "Area in km^2", "Ecclesiastical territories (total)") %in% names(dc))) {
  dc[["Average area of ecclesiastical territories in km^2"]] <-
    round(safe_div(dc[["Area in km^2"]], dc[["Ecclesiastical territories (total)"]]), 2)
}
if (all(c("Average diocesan area (in km^2)", "Area in km^2", "Ecclesiastical territories (total)") %in% names(dc))) {
  dc[["Average diocesan area (in km^2)"]] <-
    round(safe_div(dc[["Area in km^2"]], dc[["Ecclesiastical territories (total)"]]), 2)
}

# Recompute apostolic workforce share.
if (all(c("Priests and bishops as share of apostolic workforce",
          "Bishops (total)", "Catechists", "Lay missionaries") %in% names(dc)) &&
    ("Priests (diocesan and religious)" %in% names(dc) ||
     all(c("Diocesan priests (total)", "Religious priests") %in% names(dc)))) {
  priests_total <- if ("Priests (diocesan and religious)" %in% names(dc)) {
    dc[["Priests (diocesan and religious)"]]
  } else {
    dc[["Diocesan priests (total)"]] + dc[["Religious priests"]]
  }
  
  workforce <- priests_total + dc[["Bishops (total)"]] + dc[["Catechists"]] + dc[["Lay missionaries"]]
  dc[["Priests and bishops as share of apostolic workforce"]] <-
    round(safe_div(priests_total + dc[["Bishops (total)"]], workforce), 2)
}

# Commit all recomputed values back to data_countries.
data_countries <- dc


# ---- Final Map Data Rematching ----
# Re-merge the world map with corrected country data.

map_data <- world_custom %>%
  left_join(data_countries, by = c("Region_bridge" = "country")) %>%
  rename(name = Region_bridge)

# Check for any remaining unmatched countries and print them.
unmatched_after_fix <- anti_join(
  data_countries,
  world_custom %>% st_drop_geometry(),
  by = c("country" = "Region_bridge")
)
#print(unique(unmatched_after_fix$country))


# ---- Identify Time Series Variables ----
# Filter variables that have data for more than one year for time series use.

time_series_vars <- allowed_variables[
  sapply(allowed_variables, function(var) {
    years <- data_countries %>%
      filter(!is.na(.data[[var]])) %>%
      pull(Year) %>%
      unique()
    length(years) > 1
  })
]

# ---- UI Layout ----
# Prepare country choices for the search input.

all_countries <- sort(unique(data_countries$country))
all_countries <- all_countries[!is.na(all_countries) & all_countries != ""]
country_choices_list <- as.list(all_countries)
names(country_choices_list) <- all_countries
final_country_dropdown_choices <- c("Type to search..." = "", country_choices_list)

# ---- Helper Functions for UI ----
# Helper function to create select inputs for variables, years, or countries.
create_select_input <- function(id, label, choices, selected = NULL, multiple = FALSE, placeholder = NULL) {
  if (!is.null(placeholder)) {
    selectizeInput(
      inputId = id,
      label = label,
      choices = choices,
      selected = selected,
      multiple = multiple,
      options = list(placeholder = placeholder)
    )
  } else {
    selectInput(
      inputId = id,
      label = label,
      choices = choices,
      selected = selected,
      multiple = multiple
    )
  }
}

# Helper function to create download buttons for CSV and Excel.
create_download_buttons <- function() {
  div(
    style = "margin-top: 10px;",
    downloadButton("download_csv", "CSV", class = "btn btn-sm btn-success"),
    downloadButton("download_excel", "Excel", class = "btn btn-sm btn-info")
  )
}

# ---- Helper Functions for Server Logic ----
# Helper function to format values for display (e.g., map hover labels, country info).
format_value <- function(value, mode) {
  ifelse(mode != "absolute" & value == 0,
         "<0.01",
         formatC(round(as.numeric(value), ifelse(mode == "absolute", 0, 2)), format = "f", digits = ifelse(mode == "absolute", 0, 2), big.mark = ","))
}

# Helper function to create color palette for map visualizations.
create_pal <- function(values) {
  valid_values <- values[!is.na(values)]
  if (length(valid_values) > 0) {
    colorNumeric(palette = viridisLite::plasma(256), domain = valid_values, na.color = "transparent")
  } else {
    colorNumeric(palette = viridisLite::plasma(256), domain = c(0, 1), na.color = "transparent")
  }
}

# Helper function to create download data for CSV/Excel handlers.
create_download_data <- function(data, year, variable, selected_country) {
  filtered <- data %>%
    filter(Year == year) %>%
    select(country, Year, all_of(variable))
  if (!is.null(selected_country) && selected_country %in% filtered$country) {
    filtered <- filtered %>% filter(country == selected_country)
  }
  filtered
}

# Define the Shiny UI with custom styles and layout.
ui <- tagList(
  tags$head(
    includeCSS("styles.css"),
    useShinyjs()
  ),
  
  navbarPage("Annuarium Statisticum Ecclesiae", id = "navbar", theme = shinytheme("flatly"),
             
             tabPanel("Map",
                      div(
                        leafletOutput("map", height = "100vh", width = "100%"),
                        absolutePanel(
                          id = "controls", class = "panel panel-default", fixed = TRUE,
                          draggable = TRUE, top = 60, left = 0, right = "auto", bottom = "auto",
                          width = 300, height = "auto",
                          style = "background-color: rgba(255,255,255,0.8); padding: 10px; border-radius: 10px; overflow-y: auto; max-height: 90vh;",
                          create_select_input("variable", "Select variable to display:", allowed_variables),
                          radioButtons("geographic_level", "Geographic Level:",
                                       choices = list("Countries" = "countries", "Macroregions" = "macroregions"),
                                       selected = "countries"),
                          create_select_input("year", "Select year:", sort(unique(data_countries$Year)), selected = max(data_countries$Year)),
                          radioButtons("display_mode", "Display mode:",
                                       choices = list("Absolute values" = "absolute",
                                                      "Per thousand inhabitants" = "per_capita",
                                                      "Per thousand Catholics" = "per_catholic"),
                                       selected = "absolute"),
                          create_select_input("country_search", "Search for a country:", final_country_dropdown_choices, selected = "", multiple = FALSE, placeholder = "Type to search..."),
                          plotOutput("varPlot", height = 150),
                          hr(),
                          div(style = "margin-bottom: 15px;", htmlOutput("country_info")),
                          actionButton("reset_map", "Reset View", icon = icon("undo")),
                          div(style = "margin-top: 15px;",
                              downloadButton("download_map", "Download Map", class = "btn btn-primary")
                          )
                        )
                      )
             ),
             
             # DATA TABLE TAB
             tabPanel("Data Explorer",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          tags$div(
                            style = "background-color: #f8f9fa; border-radius: 8px; padding: 15px; border: 1px solid #dee2e6; font-size: 14px;",
                            # Add geographic level selector
                            radioButtons("explorer_geographic_level", "Geographic Level:",
                                         choices = list("Countries" = "countries", "Macroregions" = "macroregions"),
                                         selected = "countries"),
                            create_select_input("explorer_variable", "Select variable:", c("Select a variable..." = "", allowed_variables)),                            create_select_input("explorer_year", "Select year:", sort(unique(data_countries$Year))),
                            # Add coverage info display
                            htmlOutput("explorer_coverage_info"),
                            create_download_buttons(),
                            br(),
                            actionButton("reset_table", "Reset Filters", icon = icon("redo"), class = "btn btn-sm btn-secondary")
                          )
                        ),
                        mainPanel(
                          class = "data-explorer-main",
                          width = 9,
                          DTOutput("table"),
                          br()
                        )
                      )
             ),
             
             # Time Series TAB
             tabPanel(
               "Time Series",
               tags$style(HTML("
    .ts-container {
      min-height: calc(100vh - 50px); /* Adjust 50px based on navbar height */
      display: flex;
      align-items: stretch;
      margin: 0;
      padding: 0;
    }
    .ts-sidebar {
      height:calc(100vh - 100px);
      overflow-y: auto;
      background-color: #f8f9fa;
      padding: 15px;
      border-radius: 8px;
      border: 1px solid #dee2e6;
      min-height: 600px;
    }
    .ts-main {
      height: 100%;
      display: flex;
      flex-direction: column;
      overflow: hidden;
    }
    .ts-plot {
      flex: 1 1 auto;
      height: 100%;
    }
  ")),
               fluidRow(
                 class = "ts-container",
                 column(
                   width = 3,
                   class = "ts-sidebar",
                   create_select_input("ts_variable", "Select variable:", time_series_vars, selected = time_series_vars[1]),
                   radioButtons("ts_level", "Geographic Level:",
                                choices = c("Countries" = "countries", "Macroregions" = "macroregions"),
                                selected = "macroregions"),
                   uiOutput("ts_region_selector"),
                   div(
                     style = "margin-top: 10px;",
                     downloadButton("download_ts_plot", "Download Plot", class = "btn btn-sm btn-primary"),
                     actionButton("reset_ts", "Reset", icon = icon("undo"), class = "btn btn-sm btn-secondary")
                   )
                 ),
                 column(
                   width = 9,
                   class = "ts-main",
                   div(
                     class = "ts-plot",
                     plotlyOutput("ts_plot", height = "100%")
                   )
                 )
               )
             ),
             
             tabPanel(
               "Credits",
               tags$div(
                 style = "padding: 20px; max-width: 800px; margin: 0 auto; font-size: 16px; line-height: 1.6;",
                 tags$h3("Credits"),
                 tags$p(
                   "The data presented in this web application was extracted using Optical Character Recognition (OCR) by the University Library at the University of Mannheim from the 2022 edition of the ",
                   tags$i("Annuarium Statisticum Ecclesiae."),
                   "The ",
                   tags$i("Annuarium Statisticum Ecclesiae"),
                   " is compiled annually by the Central Office of Church Statistics of the Holy See's Secretariat of State and published by the Vatican Publishing House."
                 ),
                 tags$p(
                   "The data was then transformed and edited by Felicitas Hörl, student assistant for Prof. Dr. Andreas Wollbold. Additional preprocessing steps and development of the web apps were carried out by Claudia Schiavetti and Manuel Soffici. Claudia Schiavetti and Manuel Soffici worked on the project as Master students in Statistics and Data Science at LMU Munich as part of the Consulting Project module."
                 ),
                 tags$p(
                   "The initial idea for the project was developed and supervised by Dr. Anna-Carolina Haensch (Institute of Statistics) and supported by Prof. Dr. Andreas Wollbold and Prof. Dr. Jean-Olivier Nke Ongono (Faculty of Catholic Theology)."
                 ),
                 tags$p(
                   "For further information or inquiries, please contact Dr. Haensch at C.Haensch[at]lmu.de."
                 ),
                 tags$p(
                   "This work is licensed under a ",
                   tags$a(
                     href = "https://creativecommons.org/licenses/by-nc/4.0/",
                     target = "_blank",
                     "Creative Commons Attribution-NonCommercial (CC BY-NC) License."
                   ), 
                 )
               )
             )
             
  )
)


# ---- Server Logic ----
# Helper function to standardize macroregion names for consistency in plotting.

TARGET_REGIONS <- c(
  "South and Far East Asia",
  "Africa",
  "Europe",
  "South America",
  "North America",
  "Middle East Asia",
  "Central America",
  "Oceania"
)
standardize_macro <- function(df, col = "macroregion") {
  df %>%
    filter(!.data[[col]] %in% c("World", "America", "Asia")) %>% # drop aggregates
    mutate(
      macro_simplified = dplyr::case_when(
        .data[[col]] %in% c("Central America (Mainland)", "Central America (Antilles)") ~ "Central America",
        .data[[col]] %in% c("South East and Far East Asia", "South & Far East Asia") ~ "South and Far East Asia",
        .data[[col]] == "Middle East" ~ "Middle East Asia",
        TRUE ~ .data[[col]]
      )
    ) %>%
    filter(macro_simplified %in% TARGET_REGIONS) %>%
    mutate(macro_simplified = factor(macro_simplified, levels = TARGET_REGIONS))
}

# Define the server function for the Shiny app.
server <- function(input, output, session) {
  
  # ---- Initialize Reactive Values ----
  # Reactive value for selected country
  selected_country <- reactiveVal(NULL)
  selected_macroregion <- reactiveVal(NULL)
  # Reactive values for synchronizing selections across tabs.
  selections <- reactiveValues(
    variable = NULL,
    year = NULL,
    from_tab = NULL # Track which tab triggered the change
  )
  
  # Reactive data based on geographic level
  current_data <- reactive({
    if (input$geographic_level == "countries") {
      data_countries
    } else {
      data_macroregions
    }
  })
  
  # Reactive map data based on geographic level  
  current_map_data <- reactive({
    if (input$geographic_level == "countries") {
      map_data
    } else {
      map_data_macroregions
    }
  })
  
  # Reactive allowed variables based on geographic level
  current_allowed_variables <- reactive({
    if (input$geographic_level == "countries") {
      allowed_variables
    } else {
      allowed_variables_macroregions
    }
  })
  
  #Reactive explorer tab based on geographic level 
  current_explorer_data <- reactive({
    if (input$explorer_geographic_level == "countries") {
      data_countries
    } else {
      data_macroregions
    }
  })
  
  # Reactive allowed variables in the explorer tab based on geographic level
  current_explorer_allowed_variables <- reactive({
    if (input$explorer_geographic_level == "countries") {
      allowed_variables
    } else {
      allowed_variables_macroregions
    }
  })
  
  # Update variable choices when geographic level changes
  observe({
    updateSelectInput(
      session,
      "variable",
      choices = current_allowed_variables(),
      selected = if (input$variable %in% current_allowed_variables()) input$variable else current_allowed_variables()[1]
    )
  })
  
  # Reactive for display mode label.
  mode_label <- reactive({
    if (as.integer(input$year) == 2022) input$display_mode else "absolute"
  })
  
  # Reactive for filtered map data
  # Replace the filtered_map_data reactive with this corrected structure:
  
  filtered_map_data <- reactive({
    req(input$variable, input$year, input$geographic_level)
    
    if (input$geographic_level == "countries") {
      data <- current_map_data() %>% filter(Year == input$year)
    } else {
      # Use full macroregion polygons (no holes) - revert to original approach
      data <- map_data_macroregions %>% filter(Year == input$year)
      
      # But recalculate aggregations using only countries with data for this variable
      countries_with_data <- coverage_matrix %>%
        filter(variable == input$variable, year == input$year, has_data == TRUE) %>%
        pull(country)
      
      # Add this check for empty results
      if (length(countries_with_data) == 0) {
        # Get total countries per macroregion from actual data
        total_by_region <- data_countries %>%
          filter(Year == input$year) %>%
          mutate(macroregion = assign_macroregion(country)) %>%
          filter(!is.na(macroregion)) %>%
          group_by(macroregion) %>%
          summarise(total_countries = n(), .groups = "drop")
        
        # Return all regions with zero coverage and 0 values (not NA)
        data <- map_data_macroregions %>% 
          filter(Year == input$year) %>%
          left_join(total_by_region, by = "macroregion") %>%
          mutate(
            countries_with_data = 0,
            coverage_pct = 0,
            !!input$variable := 0  # Change from NA_real_ to 0
          )
        return(data)
      }
      
      # Create coverage statistics
      coverage_stats <- data_countries %>%
        filter(Year == input$year) %>%
        mutate(macroregion = assign_macroregion(country)) %>%
        filter(!is.na(macroregion)) %>%
        group_by(macroregion) %>%
        summarise(
          total_countries = n(),
          countries_with_data = sum(country %in% countries_with_data),
          total_population = sum(`Inhabitants in thousands`, na.rm = TRUE),
          # Only count population from countries that have data for this variable
          covered_population = sum(ifelse(country %in% countries_with_data, 
                                          `Inhabitants in thousands`, 0), na.rm = TRUE),
          # Fix the percentage calculation
          coverage_pct = ifelse(total_population > 0, 
                                round(100 * covered_population / total_population, 0), 
                                0),
          .groups = "drop"
        )
      
      # Recalculate aggregations using only countries with data
      filtered_aggregation <- data_countries %>%
        filter(Year == input$year, country %in% countries_with_data) %>%
        mutate(macroregion = assign_macroregion(country)) %>%
        filter(!is.na(macroregion)) %>%
        group_by(macroregion) %>%
        summarise(
          across(where(is.numeric), ~ if (all(is.na(.))) NA_real_ else sum(., na.rm = TRUE)),
          .groups = "drop"
        )
      
      # Merge with coverage stats and update the data
      data <- data %>%
        select(-all_of(intersect(names(filtered_aggregation)[-1], names(data)))) %>%
        left_join(filtered_aggregation, by = "macroregion") %>%
        left_join(coverage_stats, by = "macroregion") %>%
        # if no countries contribute, show 0 (not NA) for the selected variable
        mutate(
          !!input$variable := dplyr::if_else(
            is.na(.data[[input$variable]]) & (countries_with_data == 0),
            0, .data[[input$variable]]
          )
        )
    }
    
    # Apply display mode transformations (per capita, per catholic)
    if (as.integer(input$year) == 2022) {
      if (input$display_mode == "per_capita") {
        if (input$geographic_level == "countries") {
          # For countries, simple division
          data[[input$variable]] <- dplyr::case_when(
            !is.na(data[["Inhabitants in thousands"]]) & data[["Inhabitants in thousands"]] > 0 ~
              data[[input$variable]] / data[["Inhabitants in thousands"]],
            TRUE ~ NA_real_
          )
        } else {
          # For macroregions, check coverage and use data$countries_with_data (not countries_with_data vector)
          data[[input$variable]] <- dplyr::case_when(
            !is.na(data$countries_with_data) & data$countries_with_data == 0 ~ 0,
            !is.na(data[["Inhabitants in thousands"]]) & data[["Inhabitants in thousands"]] > 0 ~
              data[[input$variable]] / data[["Inhabitants in thousands"]],
            TRUE ~ NA_real_
          )
        }
      } else if (input$display_mode == "per_catholic") {
        if (input$geographic_level == "countries") {
          # For countries, simple division
          data[[input$variable]] <- dplyr::case_when(
            !is.na(data[["Catholics in thousands"]]) & data[["Catholics in thousands"]] > 0 ~
              data[[input$variable]] / data[["Catholics in thousands"]],
            TRUE ~ NA_real_
          )
        } else {
          # For macroregions, check coverage and use data$countries_with_data
          data[[input$variable]] <- dplyr::case_when(
            !is.na(data$countries_with_data) & data$countries_with_data == 0 ~ 0,
            !is.na(data[["Catholics in thousands"]]) & data[["Catholics in thousands"]] > 0 ~
              data[[input$variable]] / data[["Catholics in thousands"]],
            TRUE ~ NA_real_
          )
        }
      }
    }
    
    data
  })
  
  # Helper function to update time series for a selected country.
  update_time_series_for_country <- function(country) {
    req(country)
    if (country %in% data_countries$country) {
      updateRadioButtons(session, "ts_level", selected = "countries")
      shinyjs::delay(500, {
        updateSelectInput(session, "ts_regions", selected = country)
      })
    }
  }
  
  # Update time series for a selected macroregion
  update_time_series_for_macroregion <- function(macroregion) {
    req(macroregion)
    # Standardize the macroregion name for time series
    standardized_name <- case_when(
      macroregion %in% c("Central America (Mainland)", "Central America (Antilles)") ~ "Central America",
      macroregion == "South East and Far East Asia" ~ "South and Far East Asia",
      macroregion == "Middle East" ~ "Middle East Asia", 
      TRUE ~ macroregion
    )
    
    if (standardized_name %in% TARGET_REGIONS) {
      updateRadioButtons(session, "ts_level", selected = "macroregions")
      shinyjs::delay(500, {
        updateSelectInput(session, "ts_regions", selected = standardized_name)
      })
    }
  }
  
  
  # ---- Synchronize Variable and Year Selections ----
  # Update from Map tab.
  observeEvent(input$variable, {
    if (is.null(selections$from_tab) || selections$from_tab != "explorer") {
      selections$variable <- input$variable
      selections$from_tab <- "map"
      updateSelectInput(session, "explorer_variable", selected = input$variable)
      if (input$variable %in% time_series_vars) {
        updateSelectInput(session, "ts_variable", selected = input$variable)
      } else {
        updateSelectInput(session, "ts_variable", selected = time_series_vars[1])
      }
    }
  })
  
  # Update from Data Explorer tab.
  observeEvent(input$explorer_variable, {
    req(input$explorer_variable)
    if (input$explorer_variable != "" && (is.null(selections$from_tab) || selections$from_tab != "map")) {
      selections$variable <- input$explorer_variable
      selections$from_tab <- "explorer"
      updateSelectInput(session, "variable", selected = input$explorer_variable)
      if (input$explorer_variable %in% time_series_vars) {
        updateSelectInput(session, "ts_variable", selected = input$explorer_variable)
      } else {
        updateSelectInput(session, "ts_variable", selected = time_series_vars[1])
      }
    }
  })
  
  # Update from Time Series tab.
  observeEvent(input$ts_variable, {
    if (is.null(selections$from_tab) || selections$from_tab != "map") {
      selections$variable <- input$ts_variable
      selections$from_tab <- "time_series"
      updateSelectInput(session, "variable", selected = input$ts_variable)
      updateSelectInput(session, "explorer_variable", selected = input$ts_variable)
    }
  })
  
  # Synchronize years from Map tab.
  observeEvent(input$year, {
    if (is.null(selections$from_tab) || selections$from_tab != "explorer") {
      selections$year <- input$year
      updateSelectInput(session, "explorer_year", selected = input$year)
    }
  })
  
  # Synchronize years from Data Explorer tab.
  observeEvent(input$explorer_year, {
    if (is.null(selections$from_tab) || selections$from_tab != "map") {
      selections$year <- input$explorer_year
      updateSelectInput(session, "year", selected = input$explorer_year)
    }
  })
  
  # Synchronize the geographic level between the map tab, data explorer tab and the ts tab
  observeEvent(input$geographic_level, {
    updateRadioButtons(session, "explorer_geographic_level", selected = input$geographic_level)
    if (input$geographic_level == "macroregions") {
      selected_country(NULL)
      selected_macroregion(NULL)
      updateSelectInput(session, "country_search", selected = "")
      leafletProxy("map") %>% clearGroup("highlight")
    }
  })
  
  observeEvent(input$explorer_geographic_level, {
    updateRadioButtons(session, "geographic_level", selected = input$explorer_geographic_level)
  })
  
  # ---- Update Available Years Based on Variable ----
  # Dynamically update year choices based on data availability for the selected variable.
  observeEvent(input$variable, {
    available_years <- sort(unique(current_data() %>%
                                     filter(!is.na(.data[[input$variable]])) %>%
                                     pull(Year)))
    updateSelectInput(session, "year", choices = available_years, selected = max(available_years))
  })
  
  # ---- Limit Display Modes to 2022 ----
  # Restrict per capita/per Catholic modes to 2022 data only.
  observeEvent(input$year, {
    if (as.integer(input$year) == 2022) {
      updateRadioButtons(
        session, "display_mode",
        choices = list("Absolute values" = "absolute",
                       "Per thousand inhabitants" = "per_capita",
                       "Per thousand Catholics" = "per_catholic"),
        selected = if (input$display_mode %in% c("absolute", "per_capita", "per_catholic")) input$display_mode else "absolute"
      )
    } else {
      updateRadioButtons(
        session, "display_mode",
        choices = list("Absolute values" = "absolute"),
        selected = "absolute"
      )
    }
  })
  
  
  # ---- Render Interactive World Map ----
  # Create the Leaflet map with selected variable data.
  output$map <- renderLeaflet({
    req(input$variable, input$year, input$geographic_level)
    ml <- mode_label()
    filtered_data <- filtered_map_data()
    
    # Ensure valid values for color palette
    pal <- create_pal(filtered_data[[input$variable]])
    
    # Create labels based on geographic level
    if (input$geographic_level == "countries") {
      region_name <- filtered_data$name
      labels <- ~lapply(paste0("<strong>", region_name, "</strong><br/>",
                               switch(ml,
                                      "absolute" = variable_abbreviations[input$variable],
                                      "per_capita" = paste(variable_abbreviations[input$variable], "per 1000 Pop."),
                                      "per_catholic" = paste(variable_abbreviations[input$variable], "per 1000 Cath.")),
                               ": ",
                               format_value(filtered_data[[input$variable]], ml)), htmltools::HTML)
    } else {
      region_name <- filtered_data$macroregion
      # Get total countries per macroregion
      total_countries <- world_with_macroregions %>%
        group_by(macroregion) %>%
        summarise(total = n(), .groups = "drop")
      
      # Create enhanced coverage labels
      labels <- ~lapply(paste0("<strong>", region_name, "</strong><br/>",
                               "Coverage: ",
                               ifelse(is.na(filtered_data$countries_with_data), "?", filtered_data$countries_with_data), "/",
                               ifelse(is.na(filtered_data$total_countries), "?", filtered_data$total_countries),
                               " countries<br/>",
                               switch(ml,
                                      "absolute" = variable_abbreviations[input$variable],
                                      "per_capita" = paste(variable_abbreviations[input$variable], "per 1000 Pop."),
                                      "per_catholic" = paste(variable_abbreviations[input$variable], "per 1000 Cath.")),
                               ": ",
                               format_value(filtered_data[[input$variable]], ml)), htmltools::HTML)
    }
    
    leaflet(filtered_data, options = leafletOptions(
      maxBounds = list(c(-120, -240), c(120, 240)),
      maxBoundsViscosity = 1,
      zoomControl = FALSE
    )) %>%
      addProviderTiles("CartoDB.Voyager", options = providerTileOptions(noWrap = TRUE)) %>%
      setView(lng = 0, lat = 30, zoom = 3) %>%
      htmlwidgets::onRender("
    function(el, x) {
      var map = this;
      L.control.zoom({ position: 'topright' }).addTo(map);
    }
  ") %>%
      addPolygons(
        fillColor = ~pal(filtered_data[[input$variable]]),
        weight = 1,
        opacity = 0.45,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.6,
        layerId = ~region_name,
        label = labels,
        highlight = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.8, bringToFront = TRUE)
      ) %>%
      addLegend(pal = pal, values = filtered_data[[input$variable]],
                title = paste(switch(ml,
                                     "absolute" = variable_abbreviations[input$variable],
                                     "per_capita" = paste(variable_abbreviations[input$variable], "per 1000 Pop."),
                                     "per_catholic" = paste(variable_abbreviations[input$variable], "per 1000 Cath.")),
                              "in", input$year),
                position = "bottomright")
  })
  
  # ---- Handle Map Download ----
  # Generate filename and content for downloading the map as interactive HTML.
  output$download_map <- downloadHandler(
    filename = function() {
      ml <- mode_label()
      paste0("map_export_", input$variable, "_", input$year,
             switch(ml,
                    "absolute" = "",
                    "per_capita" = "_per_capita",
                    "per_catholic" = "_per_catholic"), ".html")
    },
    content = function(file) {
      ml <- mode_label()
      filtered_data <- filtered_map_data()
      pal <- create_pal(filtered_data[[input$variable]])
      leaflet_obj <- leaflet(filtered_data) %>%
        addProviderTiles("CartoDB.Positron") %>%
        fitBounds(-110, -40, 120, 65) %>%
        addPolygons(
          fillColor = ~pal(filtered_data[[input$variable]]),
          color = "white", weight = 1, opacity = 0.45, fillOpacity = 0.6,
          label = if(input$geographic_level == "countries") ~name else ~macroregion
        ) %>%
        addLegend(pal = pal, values = filtered_data[[input$variable]],
                  title = paste(switch(ml,
                                       "absolute" = variable_abbreviations[input$variable],
                                       "per_capita" = paste(variable_abbreviations[input$variable], "per 1000 Pop."),
                                       "per_catholic" = paste(variable_abbreviations[input$variable], "per 1000 Cath.")),
                                "in", input$year),
                  position = "bottomleft") %>%
        addControl(
          html = paste0("<div style='font-size:20px; font-weight:bold; background-color:rgba(255,255,255,0.7);
                padding:6px 12px; border-radius:6px;'>",
                        switch(ml,
                               "absolute" = input$variable,
                               "per_capita" = paste(input$variable, "per thousand inhabitants"),
                               "per_catholic" = paste(input$variable, "per thousand Catholics")),
                        " - ", input$year, "</div>"),
          position = "topright"
        ) %>%
        addControl(
          html = "<div style='font-size:13px; background-color:rgba(255,255,255,0.6); padding:4px 10px;
          border-radius:5px;'>Source: Annuarium Statisticum Ecclesiae</div>",
          position = "bottomright"
        ) %>%
        htmlwidgets::onRender("
      function(el, x) {
        var style = document.createElement('style');
        style.innerHTML = `
          .small-legend {
            font-size: 10px !important;
          }
          .small-legend .legend {
            line-height: 14px !important;
            font-size: 10px !important;
          }
          .small-legend .legend i {
            width: 10px !important;
            height: 10px !important;
            margin-right: 3px !important;
          }
          .small-legend .legend .legend-title {
            font-size: 11px !important;
            font-weight: bold !important;
            margin-bottom: 3px !important;
          }
        `;
        document.head.appendChild(style);
      }
    ")
      
      htmlwidgets::saveWidget(leaflet_obj, file, selfcontained = TRUE)
    }
  )
  
  # ---- Handle Map Click Events ----
  # Highlight clicked country and update selections.
  observeEvent(input$map_shape_click, {
    req(input$map_shape_click$id, input$year)
    
    if (input$geographic_level == "macroregions") {
      clicked_macroregion <- input$map_shape_click$id
      selected_macroregion(clicked_macroregion)
      
      # Get the clicked macroregion data for highlighting
      clicked_data <- filtered_map_data() %>% filter(macroregion == clicked_macroregion)
      
      if (nrow(clicked_data) == 0) {
        showNotification("No data available for this macroregion.", type = "warning")
        return()
      }
      
      # Highlight selected macroregion
      leafletProxy("map") %>%
        clearGroup("highlight") %>%
        addPolygons(
          data = clicked_data,
          fill = FALSE, color = "red", weight = 3, opacity = 1, group = "highlight"
        )
      
      # Update Data Explorer to show this macroregion
      updateRadioButtons(session, "explorer_geographic_level", selected = "macroregions")
      
      # Update time series if applicable
      if (input$variable %in% time_series_vars) {
        update_time_series_for_macroregion(clicked_macroregion)
        showNotification(
          paste("Time series updated to show", clicked_macroregion),
          type = "message",
          duration = 3
        )
      }
      
      showNotification(paste("Selected macroregion:", clicked_macroregion), type = "message", duration = 3)
      return()
    }
    
    clicked_country <- input$map_shape_click$id
    filtered_data <- current_map_data() %>% filter(name == clicked_country, Year == input$year)
    
    if (nrow(filtered_data) == 0 || is.na(filtered_data$geometry[1])) {
      showNotification("No data available for this country in the selected year.", type = "warning")
      return()
    }
    
    selected_country(clicked_country)
    updateSelectInput(session, "country_search", selected = clicked_country)
    # Update Data Explorer to show this country
    updateRadioButtons(session, "explorer_geographic_level", selected = "countries")
    
    # Update time series only if variable is valid.
    if (input$variable %in% time_series_vars) {
      update_time_series_for_country(clicked_country)
      showNotification(
        paste("Time series updated to show", clicked_country),
        type = "message",
        duration = 3
      )
    }
    
    leafletProxy("map") %>%
      clearGroup("highlight") %>%
      addPolygons(
        data = filtered_data,
        fill = FALSE, color = "red", weight = 3, opacity = 1, group = "highlight"
      ) %>%
      setView(
        lng = tryCatch(
          st_coordinates(st_centroid(st_union(filtered_data$geometry)))[1],
          error = function(e) 0
        ),
        lat = tryCatch(
          st_coordinates(st_centroid(st_union(filtered_data$geometry)))[2],
          error = function(e) 30
        ),
        zoom = 4
      )
  })
  
  # Add coverage information for Data Explorer
  output$explorer_coverage_info <- renderUI({
    req(input$explorer_variable, input$explorer_year)
    if (input$explorer_variable == "" || input$explorer_geographic_level == "countries") return(NULL)
    
    countries_with_data <- coverage_matrix %>%
      filter(variable == input$explorer_variable, year == input$explorer_year, has_data == TRUE) %>%
      pull(country)
    
    total_countries <- length(unique(data_countries$country))
    coverage_count <- length(countries_with_data)
    
    div(
      style = "margin: 10px 0; padding: 8px; background-color: #e9ecef; border-radius: 4px; font-size: 12px;",
      HTML(paste0("<strong>Coverage:</strong> ", coverage_count, "/", total_countries, " countries have data for this variable"))
    )
  })
  
  # ---- Handle Country Search Selection ----
  # Highlight selected country from search and update view.
  observeEvent(input$country_search, {
    req(input$country_search, input$year)
    selected_country(input$country_search)
    
    # Update time series only if variable is valid.
    if (input$variable %in% time_series_vars) {
      update_time_series_for_country(input$country_search)
    }
    
    leafletProxy("map") %>%
      clearGroup("highlight") %>%
      addPolygons(
        data = map_data %>% filter(name == input$country_search, Year == input$year),
        fill = FALSE, color = "red", weight = 3, opacity = 1, group = "highlight") %>%
      setView(
        lng = tryCatch(
          st_coordinates(st_centroid(st_union(map_data %>% filter(name == input$country_search))))[1],
          error = function(e) 0
        ),
        lat = tryCatch(
          st_coordinates(st_centroid(st_union(map_data %>% filter(name == input$country_search))))[2],
          error = function(e) 30
        ),
        zoom = 4
      )
  })
  
  # ---- Sync Selections on Tab Switch to Time Series ----
  # Ensure time series tab reflects current selections when activated.
  observeEvent(input$navbar, {
    if (input$navbar == "Time Series") {
      # Ensure variable is valid for time series.
      valid_variable <- if (!is.null(input$variable) && input$variable %in% time_series_vars) {
        input$variable
      } else {
        time_series_vars[1] # Fallback to first valid time series variable
      }
      
      # Update variable and level.
      updateSelectInput(session, "ts_variable", selected = valid_variable)
      if (!is.null(selected_country()) && selected_country() %in% data_countries$country) {
        updateRadioButtons(session, "ts_level", selected = "countries")
        shinyjs::delay(500, {
          updateSelectInput(session, "ts_regions", selected = selected_country())
        })
      } else if (!is.null(selected_macroregion()) && selected_macroregion() %in% data_macroregions$macroregion) {
        update_time_series_for_macroregion(selected_macroregion())
      } else {
        updateRadioButtons(session, "ts_level", selected = "macroregions")
        shinyjs::delay(500, {
          updateSelectInput(session, "ts_regions", selected = TARGET_REGIONS)
        })
      }
    }
  })
  
  
  # ---- Reset Map View and Selections ----
  # Clear highlights and reset view on reset button click.
  observeEvent(input$reset_map, {
    selected_country(NULL)
    selected_macroregion(NULL)
    leafletProxy("map") %>%
      clearGroup("highlight") %>%
      setView(lng = 0, lat = 30, zoom = 3)
    updateSelectInput(session, "country_search", selected = "")
    updateRadioButtons(session, "display_mode", selected = "absolute")
    
    # Reset time series to default.
    updateSelectInput(session, "ts_variable", selected = time_series_vars[1])
    updateRadioButtons(session, "ts_level", selected = "macroregions")
    updateSelectInput(session, "ts_regions", selected = TARGET_REGIONS)
    
    # Reset data explorer.
    updateSelectInput(session, "explorer_variable", selected = "")
    updateSelectInput(session, "explorer_year", selected = max(data_countries$Year))
  })
  
  # ---- Display Country-Specific Information ----
  # Render HTML with country info based on selection.
  output$country_info <- renderUI({
    ml <- mode_label()
    
    if (!is.null(selected_country())) {
      info <- filtered_map_data() %>% filter(name == selected_country())
      if (nrow(info) == 0 || is.na(info[[input$variable]][1])) {
        HTML(paste0("<strong>", selected_country(), "</strong><br/>No data available"))
      } else {
        HTML(paste0("<strong>", selected_country(), "</strong><br/>",
                    switch(ml,
                           "absolute" = input$variable,
                           "per_capita" = paste(input$variable, "per thousand inhabitants"),
                           "per_catholic" = paste(input$variable, "per thousand Catholics")),
                    " in ", input$year, ": ", format_value(info[[input$variable]][1], ml)))
      }
    } else if (!is.null(selected_macroregion())) {
      info <- filtered_map_data() %>% filter(macroregion == selected_macroregion())
      if (nrow(info) == 0 || is.na(info[[input$variable]][1])) {
        HTML(paste0("<strong>", selected_macroregion(), "</strong><br/>No data available"))
      } else {
        coverage_text <- if (!is.na(info$countries_with_data[1]) && !is.na(info$total_countries[1])) {
          paste0("<br/><small>Coverage: ", info$countries_with_data[1], "/", info$total_countries[1], " countries</small>")
        } else ""
        
        HTML(paste0("<strong>", selected_macroregion(), "</strong>", coverage_text, "<br/>",
                    switch(ml,
                           "absolute" = input$variable,
                           "per_capita" = paste(input$variable, "per thousand inhabitants"),
                           "per_catholic" = paste(input$variable, "per thousand Catholics")),
                    " in ", input$year, ": ", format_value(info[[input$variable]][1], ml)))
      }
    } else {
      HTML("")
    }
  })
  # ---- Render Macroregion Histogram ----
  output$varPlot <- renderPlot({
    
    ml <- mode_label()
    req(input$variable, input$year)
    
    # Get countries with data for this variable (same logic as map)
    countries_with_data <- coverage_matrix %>%
      filter(variable == input$variable, year == input$year, has_data == TRUE) %>%
      pull(country)
    
    # Get total number of countries
    total_countries <- length(unique(data_countries$country))
    
    # Use filtered aggregation for consistency with map
    filtered_macro <- data_countries %>%
      filter(Year == input$year, country %in% countries_with_data) %>%
      mutate(macroregion = assign_macroregion(country)) %>%
      filter(!is.na(macroregion)) %>%
      mutate(macroregion = case_when(
        macroregion %in% c("Central America (Mainland)", "Central America (Antilles)") ~ "Central America",
        macroregion == "South East and Far East Asia" ~ "South & Far East Asia",
        TRUE ~ macroregion
      )) %>%
      filter(!macroregion %in% c("World", "America", "Asia")) %>%
      group_by(macroregion) %>%
      summarise(
        # Sum the raw values first
        variable_sum = sum(.data[[input$variable]], na.rm = TRUE),
        inhabitants_sum = sum(`Inhabitants in thousands`, na.rm = TRUE),
        catholics_sum = sum(`Catholics in thousands`, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        # Then calculate the display values
        value = switch(ml,
                       "absolute" = variable_sum,
                       "per_capita" = ifelse(
                         inhabitants_sum > 0,
                         variable_sum / inhabitants_sum,
                         NA_real_
                       ),
                       "per_catholic" = ifelse(
                         catholics_sum > 0,
                         variable_sum / catholics_sum,
                         NA_real_
                       ))
      ) %>%
      filter(!is.na(value))
    
    if (nrow(filtered_macro) == 0 || all(is.na(filtered_macro$value))) {
      plot.new()
      text(0.5, 0.5, "No data available for any continent", cex = 1.2)
      return()
    }
    
    ggplot(filtered_macro, aes(x = reorder(macroregion, value), y = value)) +
      geom_col(fill = "#f7f7f7", color = "gray80", linewidth = 0.3, alpha = 1) +
      geom_text(
        aes(label = ifelse(ml != "absolute" & value == 0, "<0.01",
                           scales::comma(value, accuracy = ifelse(ml == "absolute", 1, 0.01)))),
        hjust = -0.05, size = 2.9
      ) +
      coord_flip(clip = "off") +
      labs(
        x = "Continents",
        y = NULL,
        title = paste("Continent-level distribution", "in", input$year),
        subtitle = if(length(countries_with_data) > 0) 
          paste("(Based on", length(countries_with_data), "out of", total_countries, "countries with data)") else NULL,
        caption = switch(ml,
                         "absolute" = variable_abbreviations[input$variable],
                         "per_capita" = paste(variable_abbreviations[input$variable], "per 1000 Pop."),
                         "per_catholic" = paste(variable_abbreviations[input$variable], "per 1000 Cath."))
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold", margin = margin(b = 5)),
        plot.subtitle = element_text(hjust = 0.5, size = 8, color = "gray60", margin = margin(b = 10)),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#CCCCCC1A"),
        panel.grid.minor = element_line(colour = "#CCCCCC1A"),
        axis.text.y = element_text(size = 8)
      )
  })
  
  # ---- Render Time Series Plot ----
  # Generate Plotly line chart for time series data.
  output$ts_plot <- renderPlotly({
    req(input$ts_variable, input$ts_regions, input$ts_level)
    
    data_source <- if (input$ts_level == "countries") data_countries else data_macroregions
    region_col <- if (input$ts_level == "countries") "country" else "macroregion"
    
    plot_data <- data_source %>%
      filter(.data[[region_col]] %in% input$ts_regions) %>%
      select(Year, !!sym(region_col), !!sym(input$ts_variable)) %>%
      rename(region = !!sym(region_col), value = !!sym(input$ts_variable)) %>%
      filter(!is.na(value))
    if (input$ts_level == "macroregions") {
      plot_data <- data_macroregions %>%
        standardize_macro("macroregion") %>%
        select(Year, macro_simplified, !!sym(input$ts_variable)) %>%
        rename(region = macro_simplified, value = !!sym(input$ts_variable)) %>%
        filter(region %in% input$ts_regions) %>%
        group_by(Year, region) %>%
        summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
        filter(!is.na(value))
    }
    
    if (nrow(plot_data) == 0) {
      return(
        plot_ly() %>%
          add_trace(x = numeric(0), y = numeric(0),
                    type = "scatter", mode = "lines+markers",
                    showlegend = FALSE, hoverinfo = "skip") %>%
          layout(title = "No data available for the selected variable and region(s)")
      )
    }
    
    plot_ly(
      data = plot_data,
      x = ~Year,
      y = ~value,
      color = ~region,
      colors = RColorBrewer::brewer.pal(max(3, length(unique(plot_data$region))), "Set2"),
      type = "scatter",
      mode = "lines+markers",
      hoverinfo = "text",
      text = ~paste0(
        "<b>", region, "</b><br>",
        "Year: ", Year, "<br>",
        "Value: ", round(value, 2)
      ),
      line = list(width = 2),
      marker = list(size = 6, opacity = 0.8)
    ) %>%
      layout(
        title = paste("Time Series of", variable_abbreviations[input$ts_variable]),
        hovermode = "closest",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Absolute Value"),
        legend = list(title = list(text = ifelse(input$ts_level == "countries", "Countries", "Macroregions")))
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })
  
  # ---- Time Series Reset Button ----
  # Reset time series selections to defaults.
  observeEvent(input$reset_ts, {
    updateSelectInput(session, "ts_variable", selected = time_series_vars[1])
    updateRadioButtons(session, "ts_level", selected = "macroregions")
    updateSelectInput(session, "ts_regions", selected = TARGET_REGIONS)
    
    # Clear country selection.
    selected_country(NULL)
    leafletProxy("map") %>% clearGroup("highlight")
    updateSelectInput(session, "country_search", selected = "")
    # Reset data explorer.
    updateSelectInput(session, "explorer_variable", selected = "")
    updateSelectInput(session, "explorer_year", selected = max(data_countries$Year))
  })
  
  # ---- Time Series Download Button ----
  # Create a static ggplot for downloading the time series plot.
  plot_ts_static <- reactive({
    req(input$ts_variable, input$ts_regions, input$ts_level)
    
    data_source <- if (input$ts_level == "countries") data_countries else data_macroregions
    region_col <- if (input$ts_level == "countries") "country" else "macroregion"
    
    plot_data <- data_source %>%
      filter(.data[[region_col]] %in% input$ts_regions) %>%
      select(Year, !!sym(region_col), !!sym(input$ts_variable)) %>%
      rename(region = !!sym(region_col), value = !!sym(input$ts_variable))
    
    if (input$ts_level == "macroregions") {
      plot_data <- data_macroregions %>%
        standardize_macro("macroregion") %>%
        select(Year, macro_simplified, !!sym(input$ts_variable)) %>%
        rename(region = macro_simplified, value = !!sym(input$ts_variable)) %>%
        filter(region %in% input$ts_regions) %>%
        group_by(Year, region) %>%
        summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
    }
    
    ggplot(plot_data, aes(x = Year, y = value, color = region)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(title = paste("Time Series of", input$ts_variable),
           x = "Year", y = "Absolute Value", color = ifelse(input$ts_level == "countries", "Countries", "Macroregions")) +
      theme_minimal(base_size = 13) +
      theme(plot.background = element_rect(fill = "white", colour = "white"),
            panel.background = element_rect(fill = "white", colour = "white")) +
      theme(legend.position = "bottom")
  })
  
  output$download_ts_plot <- downloadHandler(
    filename = function() {
      paste0("time_series_", input$ts_variable, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = plot_ts_static(), width = 10, height = 6, dpi = 300)
    }
  )
  
  
  # ---- Render Data Table for Explorer Tab ----
  # Display data table with optional per capita calculations for 2022.
  
  output$table <- renderDT({
    if (is.null(input$explorer_variable) || input$explorer_variable == "") {
      return(datatable(data.frame(Message = "Please select a variable to explore.")))
    }
    req(input$explorer_year)
    
    if (input$explorer_geographic_level == "countries") {
      if (as.integer(input$explorer_year) == 2022) {
        # For 2022: show absolute + per capita columns (but not population columns in table)
        filtered <- data_countries %>%
          filter(Year == input$explorer_year) %>%
          select(country, Year, !!input$explorer_variable, `Inhabitants in thousands`, `Catholics in thousands`) %>%
          mutate(
            `Per 1000 Inhabitants` = ifelse(
              !is.na(`Inhabitants in thousands`) & `Inhabitants in thousands` > 0,
              round(.data[[input$explorer_variable]] / `Inhabitants in thousands`, 3),
              NA_real_
            ),
            `Per 1000 Catholics` = ifelse(
              !is.na(`Catholics in thousands`) & `Catholics in thousands` > 0,
              round(.data[[input$explorer_variable]] / `Catholics in thousands`, 3),
              NA_real_
            )
          ) %>%
          select(country, Year, all_of(input$explorer_variable), `Per 1000 Inhabitants`, `Per 1000 Catholics`)
      } else {
        # For non-2022: show only absolute values
        filtered <- data_countries %>%
          filter(Year == input$explorer_year) %>%
          select(country, Year, all_of(input$explorer_variable))
      }
      
      if (!is.null(selected_country()) && selected_country() %in% filtered$country) {
        filtered <- filtered %>% filter(country == selected_country())
      }
      # Capitalize "country" header and handle dynamic variable
      col_names <- c("Country" = "country", "Year" = "Year")
      col_names <- c(col_names, setNames(input$explorer_variable, input$explorer_variable))
      if (as.integer(input$explorer_year) == 2022) {
        col_names <- c(col_names, "Per 1000 Inhabitants" = "Per 1000 Inhabitants", "Per 1000 Catholics" = "Per 1000 Catholics")
      }
      datatable(filtered, options = list(pageLength = 20), colnames = col_names)
    } else {
      # Macroregion data table - use filtered aggregation like the map
      
      # Get countries with data for this variable
      countries_with_data <- coverage_matrix %>%
        filter(variable == input$explorer_variable, year == input$explorer_year, has_data == TRUE) %>%
        pull(country)
      
      if (length(countries_with_data) == 0) {
        # No countries have data - return empty or zero values
        filtered <- data.frame(
          macroregion = character(0),
          Year = integer(0),
          variable = numeric(0)
        )
        names(filtered)[3] <- input$explorer_variable
      } else {
        # Aggregate only countries with data
        aggregated_data <- data_countries %>%
          filter(Year == input$explorer_year, country %in% countries_with_data) %>%
          mutate(macroregion = assign_macroregion(country)) %>%
          filter(!is.na(macroregion)) %>%
          group_by(macroregion) %>%
          summarise(
            Year = first(Year), # Keep the original year value
            across(where(is.numeric) & !Year, ~ if (all(is.na(.))) NA_real_ else sum(., na.rm = TRUE)),
            .groups = "drop"
          )
        
        if (as.integer(input$explorer_year) == 2022) {
          # For 2022: show absolute + per capita columns
          filtered <- aggregated_data %>%
            select(macroregion, Year, all_of(input$explorer_variable), `Inhabitants in thousands`, `Catholics in thousands`) %>%
            mutate(
              `Per 1000 Inhabitants` = ifelse(
                !is.na(`Inhabitants in thousands`) & `Inhabitants in thousands` > 0,
                round(.data[[input$explorer_variable]] / `Inhabitants in thousands`, 3),
                NA_real_
              ),
              `Per 1000 Catholics` = ifelse(
                !is.na(`Catholics in thousands`) & `Catholics in thousands` > 0,
                round(.data[[input$explorer_variable]] / `Catholics in thousands`, 3),
                NA_real_
              )
            ) %>%
            select(macroregion, Year, all_of(input$explorer_variable), `Per 1000 Inhabitants`, `Per 1000 Catholics`)
        } else {
          # For non-2022: show only absolute values
          filtered <- aggregated_data %>%
            select(macroregion, Year, all_of(input$explorer_variable))
        }
      }
      
      if (!is.null(selected_macroregion()) && selected_macroregion() %in% filtered$macroregion) {
        filtered <- filtered %>% filter(macroregion == selected_macroregion())
      }
      # Capitalize "macroregion" header and handle dynamic variable
      col_names <- c("Macroregion" = "macroregion", "Year" = "Year")
      col_names <- c(col_names, setNames(input$explorer_variable, input$explorer_variable))
      if (as.integer(input$explorer_year) == 2022) {
        col_names <- c(col_names, "Per 1000 Inhabitants" = "Per 1000 Inhabitants", "Per 1000 Catholics" = "Per 1000 Catholics")
      }
      datatable(filtered, options = list(pageLength = 20), colnames = col_names)
    }
  })
  
  # ---- Update Available Years for Explorer Tab ----
  # Dynamically update years and variables based on variable data availability.
  observeEvent(input$explorer_variable, {
    req(input$explorer_variable)
    available_years <- sort(unique(current_explorer_data() %>%
                                     filter(!is.na(.data[[input$explorer_variable]])) %>%
                                     pull(Year)))
    updateSelectInput(session, "explorer_year", choices = available_years, selected = max(available_years))
  })
  
  
  # ---- Reset Table Filters ----
  # Clear selections in data explorer tab.
  observeEvent(input$reset_table, {
    updateSelectInput(session, "explorer_variable", selected = "")
    updateSelectInput(session, "explorer_year", selected = max(data_countries$Year))
    selected_country(NULL)
    
    # Clear map highlight.
    leafletProxy("map") %>% clearGroup("highlight")
    updateSelectInput(session, "country_search", selected = "")
  })
  
  #---- Helper function to create download data ---- 
  
  create_download_data_with_per_capita <- function(data, year, variable, selected_country = NULL, geographic_level) {
    if (geographic_level == "countries") {
      if (as.integer(year) == 2022) {
        # For 2022: include all columns
        filtered <- data %>%
          filter(Year == year) %>%
          select(country, Year, all_of(variable), `Inhabitants in thousands`, `Catholics in thousands`) %>%
          mutate(
            `Per 1000 Inhabitants` = ifelse(
              !is.na(`Inhabitants in thousands`) & `Inhabitants in thousands` > 0,
              round(.data[[variable]] / `Inhabitants in thousands`, 3),
              NA_real_
            ),
            `Per 1000 Catholics` = ifelse(
              !is.na(`Catholics in thousands`) & `Catholics in thousands` > 0,
              round(.data[[variable]] / `Catholics in thousands`, 3),
              NA_real_
            )
          )
      } else {
        # For non-2022: only absolute values
        filtered <- data %>%
          filter(Year == year) %>%
          select(country, Year, all_of(variable))
      }
      
      if (!is.null(selected_country) && selected_country %in% filtered$country) {
        filtered <- filtered %>% filter(country == selected_country)
      }
    } else {
      # Macroregions - use filtered aggregation
      
      # Get countries with data for this variable
      countries_with_data <- coverage_matrix %>%
        filter(variable == variable, year == year, has_data == TRUE) %>%
        pull(country)
      
      if (length(countries_with_data) == 0) {
        # No countries have data
        filtered <- data.frame(
          macroregion = character(0),
          Year = integer(0),
          variable = numeric(0)
        )
        names(filtered)[3] <- variable
      } else {
        # Aggregate only countries with data
        aggregated_data <- data_countries %>%
          filter(Year == input$explorer_year, country %in% countries_with_data) %>%
          mutate(macroregion = assign_macroregion(country)) %>%
          filter(!is.na(macroregion)) %>%
          group_by(macroregion) %>%
          summarise(
            Year = first(Year),  # Keep the original year value
            across(where(is.numeric) & !Year, ~ if (all(is.na(.))) NA_real_ else sum(., na.rm = TRUE)),
            .groups = "drop"
          )
        
        if (as.integer(year) == 2022) {
          # For 2022: include all columns  
          filtered <- aggregated_data %>%
            select(macroregion, Year, all_of(variable), `Inhabitants in thousands`, `Catholics in thousands`) %>%
            mutate(
              `Per 1000 Inhabitants` = ifelse(
                !is.na(`Inhabitants in thousands`) & `Inhabitants in thousands` > 0,
                round(.data[[variable]] / `Inhabitants in thousands`, 3),
                NA_real_
              ),
              `Per 1000 Catholics` = ifelse(
                !is.na(`Catholics in thousands`) & `Catholics in thousands` > 0,
                round(.data[[variable]] / `Catholics in thousands`, 3),
                NA_real_
              )
            )
        } else {
          # For non-2022: only absolute values
          filtered <- aggregated_data %>%
            select(macroregion, Year, all_of(variable))
        }
      }
      
      if (!is.null(selected_country) && selected_country %in% filtered$macroregion) {
        filtered <- filtered %>% filter(macroregion == selected_country)
      }
    }
    return(filtered)
  }
  
  # ---- Download Data as CSV ----
  output$download_csv <- downloadHandler(
    filename = function() {
      level <- if (input$explorer_geographic_level == "countries") "countries" else "macroregions"
      paste0("data_explorer_", level, "_", input$explorer_variable, "_", input$explorer_year, ".csv")
    },
    content = function(file) {
      req(input$explorer_variable, input$explorer_year)
      data_to_download <- if (input$explorer_geographic_level == "countries") {
        create_download_data_with_per_capita(
          data_countries, 
          input$explorer_year, 
          input$explorer_variable, 
          selected_country(),
          "countries"
        )
      } else {
        create_download_data_with_per_capita(
          data_macroregions, 
          input$explorer_year, 
          input$explorer_variable, 
          selected_macroregion(),
          "macroregions"
        )
      }
      write.csv(data_to_download, file, row.names = FALSE)
    }
  )
  
  # ---- Download Data as Excel ----
  output$download_excel <- downloadHandler(
    filename = function() {
      level <- if (input$explorer_geographic_level == "countries") "countries" else "macroregions"
      paste0("data_explorer_", level, "_", input$explorer_variable, "_", input$explorer_year, ".xlsx")
    },
    content = function(file) {
      req(input$explorer_variable, input$explorer_year)
      data_to_download <- if (input$explorer_geographic_level == "countries") {
        create_download_data_with_per_capita(
          data_countries, 
          input$explorer_year, 
          input$explorer_variable, 
          selected_country(),
          "countries"
        )
      } else {
        create_download_data_with_per_capita(
          data_macroregions, 
          input$explorer_year, 
          input$explorer_variable, 
          selected_macroregion(),
          "macroregions"
        )
      }
      writexl::write_xlsx(data_to_download, path = file)
    }
  )
}


# ---- Launch the Shiny App ----
shinyApp(ui, server)