# plot.R: Creates plots for the Explorer view.

# Global dataframe
df <- getData()

# Creates a bar plot of species counts.
plotSpecies <- function() {
  ggplot(df) +
    ggtitle("Count of abandoned animals by species") +
    geom_bar(aes(Animal.Type, fill = Animal.Type), stat = "count") +
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
    geom_bar(aes(Breed, fill = Breed), stat = "count") +
    theme(
      text = element_text(size = 15),
      axis.text.x = element_text(angle = -45)
    )
}

plotDate <- function (from, to) {
  days <- as.numeric(to - from)
  breaks <- "1 month"
  labels <- "%b %Y"
  if (days < 31) {
    breaks <- "1 week"
    labels <- "%d %b %Y"
  } else if (days > 31 * 6) {
    breaks <- "2 months"
  }
  ggplot(df, aes(In.Date, 0)) +
    ggtitle("Color, size, and age of pets over intake date") +
    geom_point(
      aes(size = Pet.Age, shape = Pet.Size),
      color = df$hex    # Has to stay out of aes for hex code to work
    ) +
    scale_x_date(
      date_breaks = breaks,
      date_labels = labels,
      limits = c(from, to)
    ) +
    scale_size(guide = "none") +
    # Hack to remove Y axis
    scale_y_discrete(labels = NULL, breaks = NULL) +
    labs(y = "")
}
