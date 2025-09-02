# 1. SETUP LIBRARIES AND FONTS
# --------------------------------------------------
# Load necessary libraries
library(tidyverse)
library(tidyquant)
library(lubridate)
library(scales)
library(showtext)
library(glue)

# Add Google Font for plots
font_add_google("Montserrat", "montserrat")
showtext_auto()


# 2. DEFINE TICKERS AND NAMES
# --------------------------------------------------
# Define the list of NSA CPI tickers to download from FRED
# Includes the original tickers plus the newly added ones
cpi_nsa_tickers <- c(
  # Original Tickers
  "CPIAUCNS", "CPILFENS", "CUUR0000SAD", "CUUR0000SACL1E",
  "CUUR0000SAH1", "CUUR0000SEHC", "CPIAPPNS", "CUUR0000SASLE",
  # New Tickers
  "CUUR0000SAF11", "CUUR0000SEFV", "CPIENGNS", "CUUR0000SEHF",
  "CUUR0000SACE", "CUUR0000SETA01", "CUUR0000SETA02", "CUUR0000SEHA",
  "CUUR0000SEHB", "CUUR0000SAM2", "CUUR0000SAS4", "CPIEDUNS",
  "CPIFABNS"
)

# Create a named vector for clear plot titles
ticker_names <- c(
  # Original Names
  "CPIAUCNS" = "Headline CPI", "CPILFENS" = "Core CPI",
  "CUUR0000SAD" = "Durable Goods", "CUUR0000SACL1E" = "Core Goods",
  "CUUR0000SAH1" = "Shelter", "CUUR0000SEHC" = "Owners' Equivalent Rent",
  "CPIAPPNS" = "Apparel", "CUUR0000SASLE" = "Core Services",
  # New Names
  "CUUR0000SAF11" = "Food at Home",
  "CUUR0000SEFV" = "Food Away from Home",
  "CPIENGNS" = "Energy",
  "CUUR0000SEHF" = "Energy Services",
  "CUUR0000SACE" = "Energy Commodities",
  "CUUR0000SETA01" = "New Vehicles",
  "CUUR0000SETA02" = "Used Cars and Trucks",
  "CUUR0000SEHA" = "Rent of Primary Residence",
  "CUUR0000SEHB" = "Lodging Away from Home",
  "CUUR0000SAM2" = "Medical Care Services",
  "CUUR0000SAS4" = "Transportation Services",
  "CPIEDUNS" = "Education and Communication",
  "CPIFABNS" = "Food and Beverages"
)


# 3. DOWNLOAD AND PROCESS DATA
# --------------------------------------------------
# Download data from FRED
tq_cpi_nsa <- tq_get(
  cpi_nsa_tickers,
  get = "economic.data",
  from = "1960-01-01",
  to = Sys.Date()
)

# Process the data to calculate month-over-month changes
cpi_nsa_processed <- tq_cpi_nsa %>%
  group_by(symbol) %>%
  arrange(date) %>%
  mutate(
    mom_change = (price / lag(price)) - 1,
    year = year(date),
    month = month(date, label = TRUE, abbr = TRUE)
  ) %>%
  ungroup() %>%
  filter(!is.na(mom_change))


# 4. PLOTTING FUNCTION
# --------------------------------------------------
# Reusable function to create the seasonality plot
plot_cpi_sazonal <- function(data, ticker_symbol, title,
                             historical_start = 2010, historical_end = 2019,
                             recent_start = 2023) {
  
  plot_data <- data %>% filter(symbol == ticker_symbol)
  
  # Calculate historical statistics (median, 10th and 90th percentiles)
  historical_stats <- plot_data %>%
    filter(year >= historical_start, year <= historical_end) %>%
    group_by(month) %>%
    summarise(
      median_val = median(mom_change, na.rm = TRUE),
      p10 = quantile(mom_change, probs = 0.1, na.rm = TRUE),
      p90 = quantile(mom_change, probs = 0.9, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Filter data for recent years
  current_year <- year(Sys.Date())
  recent_data <- plot_data %>%
    filter(year >= recent_start, year <= current_year)
  
  # Define labels for the legend
  ribbon_label <- glue::glue("10-90th Percentile Range ({historical_start}-{historical_end})")
  median_label <- "Median"
  
  # Set up a dynamic color palette for recent years
  recent_years <- sort(unique(recent_data$year))
  color_palette <- c("#082631", "#166083", "#37A6D9", "#F2690E", "#F2B705")[1:length(recent_years)]
  names(color_palette) <- recent_years
  
  # Create the plot using ggplot2
  ggplot() +
    geom_ribbon(
      data = historical_stats,
      aes(x = month, ymin = p10, ymax = p90, group = 1, fill = ribbon_label),
      alpha = 0.25
    ) +
    geom_line(
      data = historical_stats,
      aes(x = month, y = median_val, group = 1, linetype = median_label),
      linewidth = 0.5,
      color = "black"
    ) +
    geom_line(
      data = recent_data,
      aes(x = month, y = mom_change, color = factor(year), group = year),
      linewidth = 1
    ) +
    
    # --- Scales and Guides ---
    scale_fill_manual(name = NULL, values = setNames("gray80", ribbon_label)) +
    scale_linetype_manual(name = NULL, values = setNames("dashed", median_label)) +
    scale_color_manual(name = "", values = color_palette) +
    guides(
      linetype = guide_legend(override.aes = list(color = "black")),
      fill = guide_legend(override.aes = list(alpha = 1))
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    
    # --- Labels and Titles ---
    labs(
      title = title,
      subtitle = "MoM, NSA",
      caption = "Source: BLS | Impactus UFRJ",
      y = "",
      x = NULL
    ) +
    
    # --- Theme and Styling ---
    theme_minimal(base_family = "montserrat") +
    theme(
      plot.title = element_text(size = 40, face = "bold", hjust = 0.5, margin = margin(b = 10)),
      plot.subtitle = element_text(size = 25, hjust = 0.5, margin = margin(b = 20), color = "black"),
      plot.caption = element_text(size = 25, hjust = 0.5, color = "black"),
      legend.position = "top",
      legend.title = element_text(size = 25, face = "bold"),
      legend.text = element_text(size = 25),
      legend.key.width = unit(1.5, "cm"),
      axis.title.y = element_text(size = 25, color = "black"),
      axis.text.x = element_text(angle = 90, hjust = 1, size = 25, color = "black"),
      axis.text = element_text(size = 25, color = "black"),
      panel.grid.major = element_line(color = "gray90", linetype = "dotted"),
      panel.grid.minor = element_blank()
    )
}

# 5. GENERATE AND SAVE PLOTS
# --------------------------------------------------
# Define the output directory
output_dir_nsa <- "cpi_seasonality_plots"
if (!dir.exists(output_dir_nsa)) {
  dir.create(output_dir_nsa)
}

# Create a tibble to iterate over
graficos_cpi_nsa <- tibble(
  ticker_symbol = cpi_nsa_tickers,
  titulo = ticker_names[cpi_nsa_tickers]
)

# Use pwalk to generate and save a plot for each ticker
graficos_cpi_nsa %>%
  pwalk(function(ticker_symbol, titulo) {
    # 1. Generate the plot
    plot <- plot_cpi_sazonal(
      data = cpi_nsa_processed,
      ticker_symbol = ticker_symbol,
      title = titulo
    )
    
    # 2. Define the filename
    nome_arquivo <- glue("{output_dir_nsa}/{ticker_symbol}_seasonal.png")
    
    # 3. Save the plot
    ggsave(
      filename = nome_arquivo,
      plot = plot,
      width = 8.4,
      height = 5,
      dpi = 300,
      bg = "white"
    )
    
    print(glue("Plot saved: {nome_arquivo}"))
  })
