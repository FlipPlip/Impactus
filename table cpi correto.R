#---------------------------------------------------------
# Função para pegar variações mensais SA (%)
cpi_mom <- function(series, ticker_sa) {
  series %>%
    select(date, value = all_of(ticker_sa)) %>%
    mutate(mom = (value / lag(value, 1) - 1) * 100) %>%
    select(date, mom)
}

# Escolher apenas últimos 3 meses
last_3_months <- function(df) {
  max_date <- max(df$date, na.rm = TRUE)
  df %>% filter(date %in% seq(max_date %m-% months(2), max_date, by = "1 month"))
}

#---------------------------------------------------------
# Montar tabela do CPI com base na series_map do momentum
#---------------------------------------------------------
tabela_cpi <- series_map %>%
  mutate(data = map(SA, ~ cpi_mom(tq_cpi_tickers_wide, .x) |> last_3_months())) %>%
  select(titulo, data) %>%
  unnest(data) %>%
  pivot_wider(
    names_from = date,
    values_from = mom
  )

# Identificar colunas de data
date_cols_cpi <- sort(names(tabela_cpi)[-1])
last_month_col_cpi <- date_cols_cpi[length(date_cols_cpi)]
second_last_month_col_cpi <- date_cols_cpi[length(date_cols_cpi) - 1]

# Adicionar coluna delta (último mês - penúltimo mês)
tabela_cpi <- tabela_cpi %>%
  mutate(
    delta = .data[[last_month_col_cpi]] - .data[[second_last_month_col_cpi]]
  ) %>%
  select(titulo, all_of(date_cols_cpi), delta)

# Formatar tabela
tabela_cpi_formatada <- tabela_cpi %>%
  rename(
    `Tipo de CPI` = titulo,
    `Δ` = delta
  ) %>%
  rename_with(
    ~ format(ymd(.x), "%b %y"),
    matches("^\\d{4}-\\d{2}-\\d{2}$")
  )

# Subtítulo automático
latest_month_subtitle_cpi <- format(ymd(last_month_col_cpi), "%B %Y")

#---------------------------------------------------------
# Design da tabela final igual ao PCE
#---------------------------------------------------------
library(gt)

tabela_final_cpi <- tabela_cpi_formatada %>%
  gt() %>%
  tab_header(
    title = md("**Variação Mensal do CPI**"),
    subtitle = latest_month_subtitle_cpi
  ) %>%
  tab_source_note(
    source_note = "Fonte: BLS. Variação percentual mensal, com ajuste sazonal."
  ) %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 2
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  data_color(
    columns = `Δ`,
    colors = scales::col_numeric(
      palette = c("#760A02", "#FFFFFF", "#166083"),
      domain = c(-0.5, 0.5)
    )
  ) %>%
  cols_label(
    `Δ` = md("MoM Δ")
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold", size = "large")
    ),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = everything())
  )

# Exibir no RStudio Viewer
tabela_final_cpi

# Salvar como PNG/HTML
gtsave(tabela_final_cpi, file = "tabela_variacao_cpi.png")
gtsave(tabela_final_cpi, file = "tabela_variacao_cpi.html")