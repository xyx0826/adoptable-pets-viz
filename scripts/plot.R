# plot.R: Creates plots for the Explorer view.

# Global dataframe
df <- getData()

# Creates a bar plot of species counts.
plotSpecies <- function() {
  ggplot(df) +
    ggtitle("Count of abandoned animals by species") +
    geom_bar(aes(Animal.Type), stat = "count") +
    theme(
      text = element_text(size = 15)
    )
}

# Creates a bar plot of breed counts.
# data: The data to plot. This function doesn't take global df
#       to allow filtering.
plotBreeds <- function (data) {
  data <- data %>%
    group_by(Breed) %>%
    summarise(n = n()) %>%
    arrange(desc(n))
  
  tops <- data %>%
    head(6) %>%
    pull(Breed)
  
  data <- df %>%
    filter(Breed %in% tops)

  ggplot(data) +
    ggtitle("Top 6 abandoned breeds") +
    geom_bar(aes(Breed), stat = "count") +
    theme(
      text = element_text(size = 15),
      axis.text.x = element_text(angle = -45)
    )
}
