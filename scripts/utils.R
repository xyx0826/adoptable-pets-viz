# utils.R: contains functions for cleaning data.

library(hash)

# A look-up table for converting different time units to years.
TIME_UNITS <- hash(
  c("YEAR", "YEARS", "MONTH", "MONTHS", "WEEK", "WEEKS"),
  c(1,      1,       1/12,    1/12,     1/12/4, 1/12/4)
)

getData <- function() {
  df <- read.csv("data/Adoptable_Pets.csv")
  df <- df %>%
    rename(Pet.Name = Pet.name) %>%
    mutate(Intake.Type = str_to_title(Intake.Type)) %>%
    mutate(Pet.Name = str_to_title(Pet.Name)) %>%
    mutate(Animal.Type = str_to_title(Animal.Type)) %>%
    mutate(Pet.Age = Vectorize(getYearsFromAgeString)(Pet.Age)) %>%
    mutate(Pet.Size = str_to_title(Pet.Size)) %>%
    mutate(Color = Vectorize(getVecFromColorString)(Color))
  df
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
    select(Animal.Type, n)
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
