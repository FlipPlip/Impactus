library(tidyverse)
library(tidyquant)
library(lubridate)
library(scales)
library(showtext)
library(glue)

font_add_google("Montserrat", "montserrat")
showtext_auto()

cpi_nsa_tickers <- c(
  "CPIAUCNS", "CPILFENS", "CUUR0000SAD", "CUUR0000SACL1E",
  "CUUR0000SAH1", "CUUR0000SEHC", "CPIAPPNS", "CUUR0000SASLE"
)

ticker_names <- c(
  "CPIAUCNS" = "Headline CPI", "CPILFENS" = "Core CPI",
  "CUUR0000SAD" = "Durable Goods", "CUUR0000SACL1E" = "Core Goods",
  "CUUR0000SAH1" = "Shelter", "CUUR0000SEHC" = "OER",
  "CPIAPPNS" = "Apparel", "CUUR0000SASLE" = "Core Services"
)


tq_cpi_nsa <- tq_get(
  cpi_nsa_tickers,
  get = "economic.data",
  from = "1960-01-01",
  to = Sys.Date()
)


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



plot_cpi_sazonal <- function(data, ticker_symbol, title,
                             historical_start = 2010, historical_end = 2019,
                             recent_start = 2023) {
  

  plot_data <- data %>% filter(symbol == ticker_symbol)
  

  historical_stats <- plot_data %>%
    filter(year >= historical_start, year <= historical_end) %>%
    group_by(month) %>%
    summarise(
      median_val = median(mom_change, na.rm = TRUE),
      p10 = quantile(mom_change, probs = 0.1, na.rm = TRUE),
      p90 = quantile(mom_change, probs = 0.9, na.rm = TRUE),
      .groups = "drop"
    )
  
  current_year <- year(Sys.Date())
  recent_data <- plot_data %>%
    filter(year >= recent_start, year <= current_year)
  

  ribbon_label <- glue::glue("Intervalo 10-90 ({historical_start}-{historical_end})")
  median_label <- "Mediana Histórica"
  

  recent_years <- sort(unique(recent_data$year))
  color_palette <- c("#082631", "#166083", "#37A6D9", "#F2690E", "#F2B705")[1:length(recent_years)]
  names(color_palette) <- recent_years
  

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
    
    scale_fill_manual(name = NULL, values = setNames("gray80", ribbon_label)) +
    scale_linetype_manual(name = NULL, values = setNames("dashed", median_label)) +
    scale_color_manual(name = "Anos Recentes", values = color_palette) +
    guides(
      linetype = guide_legend(override.aes = list(color = "black")),
      fill = guide_legend(override.aes = list(alpha = 1))
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    
    labs(
      title = title,
      subtitle = "Variação Mensal Não-Sazonal (m/m, NSA)",
      caption = "Fonte: FRED | Impactus UFRJ",
      y = "Variação Mensal (%)",
      x = NULL
    ) +
    theme_minimal(base_family = "montserrat") +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5, margin = margin(b = 10)),
      plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 20), color = "gray40"),
      plot.caption = element_text(size = 9, hjust = 0.5, color = "gray50"),
      legend.position = "top",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 10),
      legend.key.width = unit(1.5, "cm"),
      axis.title.y = element_text(size = 11, color = "black"),
      axis.text = element_text(size = 10, color = "black"),
      panel.grid.major = element_line(color = "gray90", linetype = "dotted"),
      panel.grid.minor = element_blank()
    )
}


output_dir_nsa <- "graficos_cpi_sazonal"
if (!dir.exists(output_dir_nsa)) {
  dir.create(output_dir_nsa)
}

graficos_cpi_nsa <- tibble(
  ticker_symbol = cpi_nsa_tickers,
  titulo = ticker_names[cpi_nsa_tickers]
)

graficos_cpi_nsa %>%
  pwalk(function(ticker_symbol, titulo) {
    # 1. Gerar o gráfico
    plot <- plot_cpi_sazonal(
      data = cpi_nsa_processed,
      ticker_symbol = ticker_symbol,
      title = titulo
    )
    
    nome_arquivo <- glue("{output_dir_nsa}/{ticker_symbol}_sazonal.png")
    
    ggsave(
      filename = nome_arquivo,
      plot = plot,
      width = 6.8,
      height = 4,
      dpi = 300,
      bg = "white"
    )

    print(glue("Gráfico salvo: {nome_arquivo}"))
  })
