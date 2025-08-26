library(tidyverse)
library(tidyquant)
library(lubridate)
library(scales)
library(showtext)
library(glue)

font_add_google("Montserrat", "montserrat")
showtext_auto()

market_map <- tribble(
  ~ticker,    ~titulo,                    ~nome_arquivo,
  "^GSPC",    "S&P 500",                  "SP500",
  "^IXIC",    "Nasdaq Composite",         "NASDAQ",
  "^RUT",     "Russell 2000",             "RUSSELL2000",
  "DX-Y.NYB", "US Dollar Index (DXY)",    "DXY",
  "^TNX",     "US Treasury 10 Anos",      "T10Y",
  "2YY=F",    "US Treasury 2 Anos",       "T02Y",
  "GC=F",     "Ouro (Gold)",              "GOLD"
)

market_data_raw <- tq_get(
  market_map$ticker,
  get = "stock.prices",
  from = Sys.Date() - days(45),
  to = Sys.Date()
)

market_data_long <- market_data_raw %>%
  select(symbol, date, open, close) %>%
  pivot_longer(
    cols = c(open, close),
    names_to = "tipo",
    values_to = "preco"
  ) %>%
  drop_na()

plot_market_asset <- function(data, title) {
  
  ggplot(data = data, aes(x = date, y = preco)) +
    geom_line(aes(color = tipo, linetype = tipo), linewidth = 0.5) +
    scale_color_manual(
      values = c("close" = "#082631", "open" = "#37A6D9"),
      labels = c("close" = "Fechamento", "open" = "Abertura")
    ) +
    scale_linetype_manual(
      values = c("close" = "solid", "open" = "dashed"),
      labels = c("close" = "Fechamento", "open" = "Abertura")
    ) +
    scale_x_date(date_labels = "%d-%b", date_breaks = "5 days") +
    
    labs(
      title = title,
      subtitle = "Preços de Abertura e Fechamento (últimos 30 dias)",
      caption = "Fonte: Yahoo Finance | Impactus UFRJ",
      x = NULL,
      y = "Preço / Nível"
    ) +
    
    theme_minimal(base_family = "montserrat") +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5, margin = margin(b = 10)),
      plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 20), color = "gray40"),
      plot.caption = element_text(size = 9, hjust = 0.5, color = "gray50"),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 11),
      legend.key.width = unit(1.5, "cm"),
      axis.title.y = element_text(size = 11, color = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "black"),
      axis.text.y = element_text(size = 10, color = "black"),
      panel.grid.major = element_line(color = "gray90", linetype = "dotted"),
      panel.grid.minor = element_blank()
    )
}

output_dir_market <- "graficos_mercado"
if (!dir.exists(output_dir_market)) {
  dir.create(output_dir_market)
}

market_map %>%
  pwalk(function(ticker, titulo, nome_arquivo) {

    dados_filtrados <- market_data_long %>%
      filter(symbol == ticker) %>%
      filter(date >= (Sys.Date() - days(30)))

    plot <- plot_market_asset(
      data = dados_filtrados,
      title = titulo
    )

    caminho_arquivo <- glue("{output_dir_market}/{nome_arquivo}.png")

    ggsave(
      filename = caminho_arquivo,
      plot = plot,
      width = 6.8,
      height = 4,
      dpi = 320,
      bg = "white"
    )

    print(glue("Gráfico salvo: {caminho_arquivo}"))
  })