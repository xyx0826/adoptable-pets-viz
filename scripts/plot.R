testPlot <- function() {
  data <- df %>%
    group_by(Breed) %>%
    summarise(n = n()) %>%
    arrange(desc(n))
  
  tops <- data %>%
    head(6) %>%
    pull(Breed)
  
  data <- df %>%
    filter(Breed %in% tops)

  ggplot(data) +
    geom_bar(aes(Breed), stat = "count")
}

testPlot()
