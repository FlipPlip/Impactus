#---------------------------------------------------------
# FUNÇÕES AUXILIARES
#---------------------------------------------------------

# Filtra últimos 3 meses
last_3_months <- function(df) {
  max_date <- max(df$date, na.rm = TRUE)
  df %>% filter(date %in% seq(max_date %m-% months(2), max_date, by = "1 month"))
}

#---------------------------------------------------------
# MAPEAMENTO DAS SÉRIES (NOME LEGÍVEL)
#---------------------------------------------------------
series_map_pce_index <- tribble(
  ~nome,                ~titulo,                                      ~MOM_TICKER,
  "pce_nominal",         "PCE (nominal)",                              "PCE_mom",
  "goods_nominal",       "Goods (nominal)",                            "DGDSRC1_mom",
  "durable_nominal",     "Durable goods (nominal)",                    "PCEDG_mom",
  "nondurable_nominal",  "Nondurable goods (nominal)",                 "PCEND_mom",
  "services_nominal",    "Services (nominal)",                         "PCES_mom",
  
  "pce_real",            "Real PCE",                                   "PCEC96_mom",
  "goods_real",          "Real goods",                                 "DGDSRX1_mom",
  "durable_real",        "Real durable goods",                         "PCEDGC96_mom",
  "nondurable_real",     "Real nondurable goods",                      "PCENDC96_mom",
  "services_real",       "Real services",                              "PCESC96_mom",
  
  "rdi_real",            "Real Disposable Personal Income",            "DSPIC96_mom",
  "psave",               "Personal Saving",                            "PMSAVE_mom",
  
  "comp_employees",      "Compensation of employees",                  "W209RC1_mom",
  "wages",               "Wage and salary disbursements",              "A576RC1_mom",
  "supp_wages",          "Supplements to wages and salaries",          "A038RC1_mom",
  
  "receipts_assets",     "Personal income receipts on assets",         "PIROA_mom",
  "interest_income",     "Personal interest income",                   "PII_mom",
  "dividends",           "Personal dividend income",                   "PDI_mom",
  
  "transfers_total",     "Current transfer receipts",                  "PCTR_mom",
  "transfers_other",     "Other transfers from business (net)",        "B931RC1_mom",
  "benefits_total",      "Gov. social benefits",                       "A063RC1_mom",
  "benefits_ss",         "Social Security",                            "W823RC1_mom",
  "benefits_medicare",   "Medicare",                                    "W824RC1_mom",
  "benefits_medicaid",   "Medicaid",                                    "W729RC1_mom",
  "benefits_ui",         "Unemployment insurance",                      "W825RC1_mom",
  "benefits_veterans",   "Veterans' benefits",                           "W826RC1_mom",
  "benefits_other",      "Other gov. social benefits",                   "W827RC1_mom"
)

#---------------------------------------------------------
# MONTAR A TABELA BASE COM MOM
#---------------------------------------------------------
tabela_pce_index <- series_map_pce_index %>%
  mutate(data = map(MOM_TICKER, ~ {
    tq_pce_metrics %>%
      select(date, value = all_of(.x)) %>%
      last_3_months() %>%
      filter(!is.na(value)) %>%
      rename(mom = value)
  })) %>%
  select(titulo, data) %>%
  unnest(data) %>%
  pivot_wider(
    names_from = date,
    values_from = mom
  )

#---------------------------------------------------------
# CALCULAR DELTA (último menos penúltimo mês)
#---------------------------------------------------------
date_cols <- sort(names(tabela_pce_index)[-1])
last_month_col <- date_cols[length(date_cols)]
second_last_month_col <- date_cols[length(date_cols) - 1]

tabela_pce_index <- tabela_pce_index %>%
  mutate(delta = .data[[last_month_col]] - .data[[second_last_month_col]]) %>%
  select(titulo, all_of(date_cols), delta)

#---------------------------------------------------------
# FORMATAR PARA EXIBIÇÃO
#---------------------------------------------------------
tabela_pce_index_formatada <- tabela_pce_index %>%
  rename(
    `Componente` = titulo,
    `Δ` = delta
  ) %>%
  rename_with(
    ~ format(ymd(.x), "%b %y"),
    matches("^\\d{4}-\\d{2}-\\d{2}$")
  )

#---------------------------------------------------------
# DESIGN COM GT
#---------------------------------------------------------
latest_month_subtitle <- format(ymd(last_month_col), "%B %Y")

tabela_final_pce_index <- tabela_pce_index_formatada %>%
  gt() %>%
  tab_header(
    title = md("**Variação Mensal (MoM) – PCE e Componentes**"),
    subtitle = latest_month_subtitle
  ) %>%
  tab_source_note(
    source_note = "Fonte: BEA. Variação percentual mensal (SA)."
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
    style = list(cell_text(weight = "bold", size = "large")),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = everything())
  )

# Exibir no Viewer
tabela_final_pce_index

# Salvar como PNG
gtsave(tabela_final_pce_index, file = "tabela_variacao_pce_index.png")

print("Tabela salva como 'tabela_variacao_pce_index.png'")
