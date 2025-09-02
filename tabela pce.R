#---------------------------------------------------------
# CARREGAR PACOTES
#---------------------------------------------------------
# Certifique-se de que os pacotes estão instalados
# install.packages(c("tidyverse", "tidyquant", "lubridate", "scales", "glue", "gt"))
library(tidyverse)
library(tidyquant)
library(lubridate)
library(scales)
library(glue)
library(gt)

#---------------------------------------------------------
# DEFINIR TICKERS E BAIXAR DADOS
#---------------------------------------------------------
# Vetor com todos os tickers de PCE necessários
pce_tickers <- c(
  "PCEPI",               # PCE (Cheio)
  "PCEPILFE",            # Núcleo do PCE
  "DGDSRG3M086SBEA",     # Bens
  "DDURRG3M086SBEA",     # Bens Duráveis
  "DNDGRG3M086SBEA",     # Bens Não Duráveis
  "DSERRG3M086SBEA",     # Serviços
  "DNRGRG3M086SBEA",     # Energia
  "IA001260M",           # Núcleo de Serviços ex-Habitação
  "DFXARG3M086SBEA",     # Alimentos
  "IA001176M"            # Núcleo do PCE ex-Habitação
)

# Baixar os dados do FRED e formatar em wide format
tq_pce_tickers_wide <- tq_get(
  pce_tickers,
  get = "economic.data",
  from = "1960-01-01",
  to = Sys.Date()
) %>%
  pivot_wider(
    names_from = symbol,
    values_from = price
  )

# Mapeamento de nomes, títulos e tickers para o PCE
series_map_pce <- tribble(
  ~nome,                             ~titulo,                                  ~SA_TICKER,
  "pce",                             "Headline PCE",                           "PCEPI",
  "core_pce",                        "Core PCE",                               "PCEPILFE",
  "goods_pce",                       "Goods PCE",                              "DGDSRG3M086SBEA",
  "durable_pce",                     "Durable Goods PCE",                      "DDURRG3M086SBEA",
  "nondurable_pce",                  "Non-Durable Goods PCE",                  "DNDGRG3M086SBEA",
  "services_pce",                    "Services PCE",                           "DSERRG3M086SBEA",
  "energy_pce",                      "Energy PCE",                             "DNRGRG3M086SBEA",
  "food_pce",                        "Food PCE",                               "DFXARG3M086SBEA",
  "core_services_ex_housing_pce",    "Core Services ex-Housing PCE",           "IA001260M",
  "core_pce_ex_housing",             "Core PCE ex-Housing",                    "IA001176M"
)

#---------------------------------------------------------
# FUNÇÕES AUXILIARES
#---------------------------------------------------------
# Função para calcular a variação mensal (MoM) em %
pce_mom <- function(series, ticker_sa) {
  series %>%
    select(date, value = all_of(ticker_sa)) %>%
    mutate(mom = (value / lag(value, 1) - 1) * 100) %>%
    select(date, mom)
}

# Função para filtrar apenas os últimos 3 meses de dados
last_3_months <- function(df) {
  max_date <- max(df$date, na.rm = TRUE)
  df %>% filter(date %in% seq(max_date %m-% months(2), max_date, by = "1 month"))
}

#---------------------------------------------------------
# MONTAGEM E FORMATAÇÃO DA TABELA
#---------------------------------------------------------
# Montar a tabela base com base no series_map_pce
tabela_pce <- series_map_pce %>%
  mutate(data = map(SA_TICKER, ~ pce_mom(tq_pce_tickers_wide, .x) |> last_3_months())) %>%
  select(titulo, data) %>%
  unnest(data) %>%
  # Trata casos onde o último mês pode não ter dados para todos os tickers
  filter(!is.na(mom)) %>%
  pivot_wider(
    names_from = date,
    values_from = mom
  )

# Identifica as colunas de data para calcular o delta
date_cols <<- sort(names(tabela_pce)[-1])
last_month_col <<- date_cols[length(date_cols)]
second_last_month_col <<- date_cols[length(date_cols) - 1]

# Adiciona a coluna delta (diferença entre o último e o penúltimo mês)
tabela_pce <- tabela_pce |>
  mutate(
    delta = .data[[last_month_col]] - .data[[second_last_month_col]]
  ) |>
  select(titulo, all_of(date_cols), delta)

# Formata a tabela para exibição
tabela_pce_formatada <- tabela_pce %>%
  rename(
    `Componente PCE` = titulo,
    `Δ` = delta
  ) %>%
  rename_with(
    # Formata os nomes das colunas de data para "Mês Ano"
    ~ format(ymd(.x), "%b %y"),
    # Aplica a formatação apenas em colunas que correspondem ao padrão de data
    matches("^\\d{4}-\\d{2}-\\d{2}$")
  )

#---------------------------------------------------------
# DESIGN DA TABELA FINAL COM 'gt'
#---------------------------------------------------------
# Determina o mês mais recente para o subtítulo
latest_month_subtitle <- format(ymd(last_month_col), "%B %Y")

tabela_final_pce <- tabela_pce_formatada %>%
  gt() %>%
  # Adiciona Títulos e Subtítulos
  tab_header(
    title = md("**Variação Mensal do PCE**"),
    subtitle = latest_month_subtitle
  ) |>
  tab_source_note(
    source_note = "Fonte: BEA. Variação percentual mensal, com ajuste sazonal."
  ) %>%
  # Formata todas as colunas numéricas
  fmt_number(
    columns = where(is.numeric),
    decimals = 2,
    suffixing = TRUE # Adiciona sufixo % automaticamente
  ) %>%
  # Centraliza o conteúdo de todas as colunas
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  # Colore a coluna Delta: verde para positivo, vermelho para negativo
  data_color(
    columns = `Δ`,
    colors = scales::col_numeric(
      palette = c("#760A02", "#FFFFFF", "#166083"), # Vermelho, Branco, Azul
      domain = c(-0.5, 0.5) # Define o intervalo de cores
    )
  ) %>%
  # Adiciona um rótulo mais descritivo para a coluna Delta
  cols_label(
    `Δ` = md("MoM Δ")
  ) %>%
  # Estilo do cabeçalho
  tab_style(
    style = list(
      cell_text(weight = "bold", size = "large")
    ),
    locations = cells_title(groups = "title")
  ) %>%
  # Estilo do cabeçalho das colunas
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = everything())
  )

# Exibe a tabela no RStudio Viewer
tabela_final_pce

# Salva a tabela como um arquivo HTML
gtsave(tabela_final_pce, file = "tabela_variacao_pce.png")

print("Tabela salva como 'tabela_variacao_pce.html'")

