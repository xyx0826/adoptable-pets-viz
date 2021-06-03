# utils.R: contains functions for cleaning data.

library(hash)

# A look-up table for converting different time units to years.
TIME_UNITS <- hash(
  c("YEAR", "YEARS", "MONTH", "MONTHS", "WEEK", "WEEKS"),
  c(1,      1,       1/12,    1/12,     1/12/4, 1/12/4)
)

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
