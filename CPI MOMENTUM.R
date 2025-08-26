library(tidyverse)
library(tidyquant)
library(recipes)
library(fredr)
library(showtext)
library(readxl)
library(scales)
library(zoo)
library(lubridate)
library(glue)

fredr_set_key("f7632ad7f5645c4681affe5a15cdca51")

font_add_google("Montserrat", "montserrat")
showtext_auto()

# Defina os tickers para dados com ajuste sazonal (SA) e sem ajuste sazonal (NSA)
cpi_tickers <- c(
  "CPIAUCSL", "CPIAUCNS",   # CPI (Cheio)
  "CPILFESL", "CPILFENS",   # Núcleo do CPI
  "CUSR0000SAD", "CUUR0000SAD", # Bens Duráveis
  "CUSR0000SACL1E", "CUUR0000SACL1E", # Núcleo de Bens
  "CUSR0000SAH1", "CUUR0000SAH1",   # Aluguel (Shelter)
  "CUSR0000SEHC", "CUUR0000SEHC",   # Aluguel Equivalente do Proprietário (OER)
  "CPIAPPSL", "CPIAPPNS",   # Vestuário
  "CUSR0000SASLE", "CUUR0000SASLE"  # Núcleo de Serviços
)


tq_cpi_tickers_wide <- tq_get(
  cpi_tickers,
  get = "economic.data",
  from = "1960-01-01",
  to = Sys.Date()
) %>%
  pivot_wider(
    names_from = symbol,
    values_from = price
  )

# FUNÇÕES AUXILIARES: Para cálculo e plotagem
#---------------------------------------------------------
# Função para calcular as métricas de momentum

momentum_formula <- function(data) {
  data |>
    mutate(
      YOY_NSA = (NSA / lag(NSA, 12) - 1) * 100,
      MOM_SA = (SA / lag(SA, 1) - 1) * 100,
      SAAR_3MMA = ((1 + (rollmean(MOM_SA, k = 3, align = "right", fill = NA) / 100))^12 - 1) * 100,
      SAAR_6MMA = ((1 + (rollmean(MOM_SA, k = 6, align = "right", fill = NA) / 100))^12 - 1) * 100
    ) |>
    select(date, `YOY NSA` = YOY_NSA, `3MMA-SAAR` = SAAR_3MMA, `6MMA-SAAR` = SAAR_6MMA) |>
    pivot_longer(cols = -date, names_to = "metric", values_to = "value") |>
    mutate(metric = factor(metric, levels = c("YOY NSA", "3MMA-SAAR", "6MMA-SAAR")))
}

# Função para gerar o gráfico
# Simplifiquei as escalas e o tema para maior clareza
grafico_momentum <- function(processed_data, chart_title) {

  metric_colors <- c("YOY NSA" = "#082631", "3MMA-SAAR" = "#166083", "6MMA-SAAR" = "#37A6D9")
  
  ggplot(data = subset(processed_data, year(date) >= year(Sys.Date()) - 5),
         aes(x = date, y = value, color = metric, linetype = metric)) +
    geom_line(linewidth = 0.5) +
    scale_color_manual(values = metric_colors) +
    scale_linetype_manual(values = c("YOY NSA" = "solid", "3MMA-SAAR" = "dashed", "6MMA-SAAR" = "dotted")) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
    scale_y_continuous(labels = label_number(suffix = "%", accuracy = 0.1)) +
    coord_cartesian(ylim = c(-5, 12)) + 
    labs(
      title = chart_title,
      subtitle = "Variação Anual (YOY) e Taxa Anualizada com Ajuste Sazonal (SAAR, %)",
      caption = "Fonte: FRED | Gráfico por Impactus UFRJ",
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_family = "montserrat") +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 5)),
      plot.subtitle = element_text(size = 10, hjust = 0.5, margin = margin(b = 15), color = "gray40"),
      plot.caption = element_text(size = 8, hjust = 0.5, color = "gray50"),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 9),
      legend.key.width = unit(1.5, "cm"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = "black"),
      axis.text.y = element_text(size = 9, color = "black"),
      panel.grid.major = element_line(color = "gray90", linetype = "dotted"),
      panel.grid.minor = element_blank()
    )
}

series_map <- tribble(
  ~nome,               ~titulo,               ~SA,              ~NSA,
  "cpi",               "CPI (Cheio)",        "CPIAUCSL",       "CPIAUCNS",
  "core_cpi",          "Núcleo do CPI",       "CPILFESL",       "CPILFENS",
  "durable_cpi",       "CPI de Bens Duráveis", "CUSR0000SAD",    "CUUR0000SAD",
  "core_goods_cpi",    "CPI do Núcleo de Bens", "CUSR0000SACL1E", "CUUR0000SACL1E",
  "shelter_cpi",       "CPI de Aluguel (Shelter)", "CUSR0000SAH1",   "CUUR0000SAH1",
  "oer_cpi",           "CPI de OER",             "CUSR0000SEHC",   "CUUR0000SEHC",
  "apparel_cpi",       "CPI de Vestuário",       "CPIAPPSL",       "CPIAPPNS",
  "core_services_cpi", "CPI do Núcleo de Serviços", "CUSR0000SASLE",  "CUUR0000SASLE"
)


output_dir <- "graficos_cpi_momentum"
if (!dir.exists(output_dir)) dir.create(output_dir)


series_map %>%
  pwalk(function(nome, titulo, SA, NSA) {
    current_data <- tq_cpi_tickers_wide %>%
      select(date, SA = .data[[SA]], NSA = .data[[NSA]])
    processed_data <- momentum_formula(current_data)
    final_plot <- grafico_momentum(processed_data, titulo)
    filename <- glue("{output_dir}/{nome}_momentum.png")
    ggsave(
      filename,
      final_plot,
      width = 5,
      height = 3,
      dpi = 300,
      bg = "white"
    )

    print(glue("Gráfico para '{titulo}' salvo como '{filename}'"))
  })
#-----------------------------------------------------------------------
