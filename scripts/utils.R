# utils.R: contains functions for cleaning data.

library(hash)

# A look-up table for converting different time units to years.
TIME_UNITS <- hash(
  c("YEAR", "YEARS", "MONTH", "MONTHS", "WEEK", "WEEKS"),
  c(1,      1,       1/12,    1/12,     1/12/4, 1/12/4)
)

# A look-up table for converting "exotic" colors to their hex codes.
COLORS <- hash(
  "brindle"    = c(0x66, 0x33, 0x08),
  "brn tabby"  = c(0x76, 0x55, 0x3b),
  "orange tab" = c(0xab, 0x70, 0x33),
  "gray tab"   = c(0xa2, 0xa9, 0xb2),
  "tortie"     = c(0x7b, 0x71, 0x57),
  "tricolor"   = c(0xc7, 0xc7, 0xc7),
  "fawn"       = c(0xde, 0xa5, 0x6c)
)

getData <- function() {
  df <- read.csv("data/Adoptable_Pets.csv")
  df <- df %>%
    rename(Pet.Name = Pet.name) %>%
    mutate(Intake.Type = str_to_title(Intake.Type)) %>%
    mutate(In.Date = as.Date(In.Date, "%m/%d/%Y")) %>%
    mutate(Pet.Name = str_to_title(Pet.Name)) %>%
    mutate(Animal.Type = str_to_title(Animal.Type)) %>%
    mutate(Pet.Age = Vectorize(getYearsFromAgeString)(Pet.Age)) %>%
    mutate(Pet.Size = str_to_title(Pet.Size)) %>%
    mutate(hex = Vectorize(getHexFromColorString)(Color)) %>%
    mutate(Color = Vectorize(getVecFromColorString)(Color))
  
  # Join the dataframe with coordinates
  df_geo <- read.csv("data/Adoptable_Pets_Locations.csv")
  left_join(df, df_geo, by = c("Animal.ID" = "id"))
}

# Converts a string representative of animal age to
# its number of years.
# String format: "x YEAR(S) y MONTH(S) z WEEK(S)" or "NO AGE"
getYearsFromAgeString <- function (str) {
  total <- 0
  if (str == "NO AGE") {
    return(total)
  }

  words <- str_split(str, " ", simplify = TRUE)
  for (i in seq(1, length(words), 2)) {
    # Every value is followed by its unit
    ratio = TIME_UNITS[[words[i + 1]]]
    total <- total + as.numeric(words[i]) * ratio
  }
  total
}

# Converts a string combination of colors to vectors of those.
# Each color is in title case, i.e. first letter capitalized.
getVecFromColorString <- function (str) {
  str_to_title(str_split(str, " / ", simplify = TRUE))
}

# COnverts a string combination of colors to its hex code.
# If there are multiple colors, they are mixed by average.
getHexFromColorString <- function (str) {
  n <- r <- g <- b <- 0
  # Split colors
  cols <- str_split(str, " / ", simplify = TRUE)
  for (i in 1:length(cols)) {
    col <- str_to_lower(cols[i])
    if (col %in% colors()) {
      # R has this color
      rgb <- col2rgb(col)
    } else {
      # One of the weird colors
      rgb <- COLORS[[col]]
    }
    # Add the color to the mix
    r <- r + rgb[1]
    g <- g + rgb[2]
    b <- b + rgb[3]
    n <- n + 1
  }
  
  # Mix colors
  r <- round(r / n)
  g <- round(g / n)
  b <- round(b / n)
  sprintf("#%02x%02x%02x", r, g, b, collapse = "")
}

getHexFromColorString("Gray Tab / White")

# Pretty-prints an age in years to a simple description.
prettyPrintAge <- function (age) {
  if (age < 1) {
    "less than a year-old"
  } else if (age %% 1 > 0.5) {
    paste0(
      "almost ", ceiling(age),
      if (ceiling(age) > 1) "-years" else "-year", "-old")
  } else {
    paste0(
      trunc(age),
      if (trunc(age) > 1) "-years" else "-year", "-old")
  }
}

# Pretty-prints count summaries about all adoptable animals.
prettyPrintSpecies <- function() {
  str <- "There are "
  specs <- df %>%
    group_by(Animal.Type) %>%
    summarise(n = n()) %>%
    select(Animal.Type, n) %>%
    arrange(desc(n))
  for (i in 1:nrow(specs)) {
    spec = specs[i,]
    if (i == nrow(specs)) {
      str <- paste0(str, "and ")
    }
    str <- paste0(str, spec$n, " ", tolower(spec$Animal.Type), "s, ")
  }
  str <- paste(
    str, "among all", nrow(df), "animals waiting for a home."
  )
}
