# Code to generate plots shown at the OHDSI APAC Symposium

library(dplyr)

# Details to connect to the results server (using local SQLITE for now) --------
resultsDatabaseConnectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "sqlite",
  server = "d:/temp/fq.sqlite"
)
resultsDatabaseSchema <- "main"


# Download data needed for forest plots ----------------------------------------
connectionHandler <- ResultModelManager::ConnectionHandler$new(resultsDatabaseConnectionDetails)

source("ShinyAppBuilderFunctions.R")
targetId <- getESTargetIds(
  connectionHandler = connectionHandler,
  mySchema = resultsDatabaseSchema,
  cmTablePrefix = "cm_",
  cgTablePrefix = "cg_"
)

outcomeId <- getESOutcomeIds(
  connectionHandler = connectionHandler,
  mySchema = resultsDatabaseSchema,
  cmTablePrefix = "cm_",
  cgTablePrefix = "cg_"
)

cmResults <- getCMEstimation(
  connectionHandler = connectionHandler,
  mySchema = resultsDatabaseSchema,
  cmTablePrefix = 'cm_',
  cgTablePrefix = 'cg_',
  databaseMetaData = 'database_meta_data',
  targetId = targetId,
  outcomeId = 1782489 # Composite
)

esResults <- getMetaEstimation(
  connectionHandler = connectionHandler,
  mySchema = resultsDatabaseSchema,
  cmTablePrefix = 'cm_',
  cgTablePrefix = 'cg_',
  esTablePrefix = 'es_',
  targetId,
  outcomeId = 1782489 # Composite
)
results <- bind_rows(cmResults, esResults)

# Create Forest plots ----------------------------------------------------------
library(ggplot2)

# comparatorName <- "Cephalosporin"
createForestPlot <- function(comparatorName) {
  header <- tibble(
    calibratedRr = -100,
    calibratedCi95Lb = -100,
    calibratedCi95Ub = -100,
    type = "header",
    label = "Source"
  )
  plotData <- results %>%
    filter(grepl(comparatorName, comparator, ignore.case = TRUE), 
           grepl("60d", description)) %>%
    select(label = "database", "calibratedRr", "calibratedCi95Lb", "calibratedCi95Ub") %>%
    mutate(type = if_else(grepl("random-effects", label), "ma", "db")) %>%
    mutate(label = if_else(grepl("random-effects", label), "Summary", label)) %>%
    mutate(label = if_else(grepl("CUMC", label), "CUMC", label)) %>%
    mutate(label = if_else(grepl("DOD", label), "Optum DoD", label)) %>%
    arrange(type, label)
  plotData <- bind_rows(header, plotData) %>%
    mutate(y = seq(from = n(), to = 1, by = -1))
  
  limits = c(0.1, 10)
  rowHeight <- 0.8
  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
  yLimits <- c(min(plotData$y) - rowHeight / 2, max(plotData$y) + rowHeight / 2)
  forest <- ggplot(plotData, aes(x = calibratedRr, y = y)) +
    geom_vline(xintercept = breaks, colour = "#AAAAAA", lty = 1, size = 0.2) +
    geom_vline(xintercept = 1, size = 0.5) +
    geom_errorbarh(aes(
      xmin = .data$calibratedCi95Lb,
      xmax = .data$calibratedCi95Ub
    ), height = 0.15) +
    geom_point(size = 3, shape = 23, aes(fill = .data$type)) +
    scale_fill_manual(values = c("#000000", "#000000", "#FFFFFF")) +
    scale_x_continuous("Hazard Ratio", trans = "log10", breaks = breaks, labels = breaks) +
    coord_cartesian(xlim = limits, ylim = yLimits) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      legend.position = "none",
      panel.border = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      plot.margin = grid::unit(c(0, 0, 0.1, 0), "lines")
    )
  
  labels <- sprintf("%0.2f (%0.2f - %0.2f)", plotData$calibratedRr, plotData$calibratedCi95Lb, plotData$calibratedCi95Ub)
  labels <- gsub("NA", "", labels)
  labels <- gsub(" \\( - \\)", "-", labels)
  labels <- data.frame(
    y = rep(plotData$y, 2),
    x = rep(1:2, each = nrow(plotData)),
    label = c(as.character(plotData$label), labels),
    stringsAsFactors = FALSE
  )
  labels$label[nrow(plotData) + 1] <- paste("Hazard Ratio", "(95% CI)")
  data_table <- ggplot(labels, aes(
    x = .data$x,
    y = .data$y,
    label = .data$label
  )) +
    geom_text(size = 4, hjust = 0, vjust = 0.5) +
    geom_hline(aes(yintercept = nrow(plotData) - 0.5)) +
    scale_y_continuous(limits = yLimits) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.text.x = element_text(colour = "white"),
      axis.text.y = element_blank(),
      axis.ticks = element_line(colour = "white"),
      plot.margin = grid::unit(c(0, 0, 0.1, 0), "lines")
    ) +
    labs(x = "", y = "") +
    coord_cartesian(xlim = c(1, 3))
  data_table
  plot <- gridExtra::grid.arrange(data_table, forest, ncol = 2)
  
  ggsave(sprintf("forest_%s.png", comparatorName), plot, width = 7, height = 1 + length(plotData) * 0.3, dpi = 400)
}

createForestPlot("Cephalosporin")
createForestPlot("Trimethoprim")

