#---------------------------------------------------------
# CARREGAR PACOTES
#---------------------------------------------------------
# install.packages(c("tidyverse", "tidyquant", "zoo", "scales", "glue"))
library(tidyverse)
library(tidyquant)
library(zoo)
library(scales)
library(glue)

#---------------------------------------------------------
# DEFINIR TICKERS E BAIXAR DADOS
#---------------------------------------------------------
# Vetor com todos os tickers de CPI necessários
cpi_tickers <- c(
  # Tickers Originais
  "CPIAUCSL", "CPIAUCNS",       # CPI (Cheio)
  "CPILFESL", "CPILFENS",       # Núcleo do CPI
  "CUSR0000SAD", "CUUR0000SAD", # Bens Duráveis
  "CUSR0000SACL1E", "CUUR0000SACL1E", # Núcleo de Bens
  "CUSR0000SAH1", "CUUR0000SAH1",   # Aluguel (Shelter)
  "CUSR0000SEHC", "CUUR0000SEHC",   # Aluguel Equivalente do Proprietário (OER)
  "CPIAPPSL", "CPIAPPNS",       # Vestuário
  "CUSR0000SASLE", "CUUR0000SASLE", # Núcleo de Serviços
  
  # Novos Tickers Adicionados
  "CUSR0000SAF11", "CUUR0000SAF11", # Food at home
  "CUSR0000SEFV", "CUUR0000SEFV",   # Food away from home
  "CPIENGSL", "CPIENGNS",           # Energy
  "CUSR0000SEHF", "CUUR0000SEHF",   # Energy services
  "CUSR0000SACE", "CUUR0000SACE",   # Energy commodities
  "CUSR0000SETA01", "CUUR0000SETA01", # New vehicles
  "CUSR0000SETA02", "CUUR0000SETA02", # Used Cars and Trucks
  "CUSR0000SEHA", "CUUR0000SEHA",   # Rent
  "CUSR0000SEHB", "CUUR0000SEHB",   # Lodging Away from Home
  "CUSR0000SAM2", "CUUR0000SAM2",   # Medical Care Services
  "CUSR0000SAS4", "CUUR0000SAS4",   # Transportation Services
  "CPIEDUSL", "CPIEDUNS",           # Education and Communication
  "CPIFABSL", "CPIFABNS"            # Food and Beverages
)

# Baixar os dados do FRED e formatar em wide format
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

#---------------------------------------------------------
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
grafico_momentum <- function(processed_data, chart_title) {
  
  metric_colors <- c("YOY NSA" = "#082631", "3MMA-SAAR" = "#166083", "6MMA-SAAR" = "#37A6D9")
  
  # O eixo x agora começa em 2012-01-01
  ggplot(data = subset(processed_data, date >= as.Date("2012-01-01")),
         aes(x = date, y = value, color = metric, linetype = metric)) +
    geom_line(linewidth = 0.5) +
    geom_hline(yintercept = 1.5, color = "#760A02", linetype = "solid", linewidth = 0.5) +
    scale_color_manual(values = metric_colors) +
    scale_linetype_manual(values = c("YOY NSA" = "solid", "3MMA-SAAR" = "dashed", "6MMA-SAAR" = "dotted")) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") + # Ajustado para o novo período
    scale_y_continuous(labels = label_number(suffix = "%", accuracy = 0.1)) +
    coord_cartesian(ylim = c(-10, 15)) + # Ajustado o limite do eixo y
    labs(
      title = chart_title,
      subtitle = "YoY, SAAR, %",
      caption = "Fonte: BLS | Impactus UFRJ",
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(size = 40, face = "bold", hjust = 0.5, margin = margin(b = 5)),
      plot.subtitle = element_text(size = 25, hjust = 0.5, margin = margin(b = 15), color = "black"),
      plot.caption = element_text(size = 25, hjust = 0.5, color = "black"),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 25),
      legend.key.width = unit(1.5, "cm"),
      axis.text.x = element_text(angle = 90, hjust = 1, size = 25, color = "black"),
      axis.text.y = element_text(size = 25, color = "black"),
      panel.grid.major = element_line(color = "gray90", linetype = "dotted"),
      panel.grid.minor = element_blank()
    )
}

#---------------------------------------------------------
# MAPEAMENTO DAS SÉRIES E GERAÇÃO DOS GRÁFICOS
#---------------------------------------------------------
# Mapeamento de nomes, títulos e tickers
series_map <- tribble(
  ~nome,                    ~titulo,                                  ~SA,              ~NSA,
  "cpi",                    "Headline CPI",                           "CPIAUCSL",       "CPIAUCNS",
  "core_cpi",                 "Core CPI",                               "CPILFESL",       "CPILFENS",
  "food_beverages_cpi",     "Food and Beverages CPI",                 "CPIFABSL",       "CPIFABNS",
  "food_home_cpi",          "Food at Home CPI",                       "CUSR0000SAF11",  "CUUR0000SAF11",
  "food_away_cpi",          "Food Away from Home CPI",                "CUSR0000SEFV",   "CUUR0000SEFV",
  "shelter_cpi",              "Shelter CPI",                            "CUSR0000SAH1",   "CUUR0000SAH1",
  "rent_cpi",                 "Rent of Primary Residence CPI",          "CUSR0000SEHA",   "CUUR0000SEHA",
  "oer_cpi",                  "Owners' Equivalent Rent (OER) CPI",      "CUSR0000SEHC",   "CUUR0000SEHC",
  "lodging_cpi",              "Lodging Away from Home CPI",             "CUSR0000SEHB",   "CUUR0000SEHB",
  "energy_cpi",               "Energy CPI",                             "CPIENGSL",       "CPIENGNS",
  "energy_commodities_cpi", "Energy Commodities CPI",                 "CUSR0000SACE",   "CUUR0000SACE",
  "energy_services_cpi",    "Energy Services CPI",                    "CUSR0000SEHF",   "CUUR0000SEHF",
  "core_goods_cpi",           "Core Goods CPI",                         "CUSR0000SACL1E", "CUUR0000SACL1E",
  "durable_cpi",              "Durable Goods CPI",                      "CUSR0000SAD",    "CUUR0000SAD",
  "new_vehicles_cpi",         "New Vehicles CPI",                       "CUSR0000SETA01", "CUUR0000SETA01",
  "used_vehicles_cpi",        "Used Cars and Trucks CPI",               "CUSR0000SETA02", "CUUR0000SETA02",
  "apparel_cpi",              "Apparel CPI",                            "CPIAPPSL",       "CPIAPPNS",
  "core_services_cpi",        "Core Services CPI",                      "CUSR0000SASLE",  "CUUR0000SASLE",
  "transport_services_cpi", "Transportation Services CPI",            "CUSR0000SAS4",   "CUUR0000SAS4",
  "medical_services_cpi",   "Medical Care Services CPI",              "CUSR0000SAM2",   "CUUR0000SAM2",
  "edu_comm_cpi",             "Education and Communication CPI",        "CPIEDUSL",       "CPIEDUNS"
)

# Cria o diretório para salvar os gráficos, se não existir
output_dir_momentum <- "cpi_momentum_plots"
if (!dir.exists(output_dir_momentum)) dir.create(output_dir_momentum)

# Itera sobre o mapa de séries para processar e gerar cada gráfico
series_map %>%
  pwalk(function(nome, titulo, SA, NSA) {
    
    # 1. PREPARAR OS DADOS PARA A ITERAÇÃO ATUAL
    #    A função `select` pega o dataframe principal e:
    #    - Mantém a coluna 'date'.
    #    - Pega a coluna cujo nome está na variável SA (ex: "CPIAUCSL") e a renomeia para "SA".
    #    - Pega a coluna cujo nome está na variável NSA (ex: "CPIAUCNS") e a renomeia para "NSA".
    #    O resultado ('current_data') é um dataframe no formato que a função 'momentum_formula' precisa.
    current_data <- tq_cpi_tickers_wide %>%
      select(date, SA = all_of(SA), NSA = all_of(NSA))
    
    # 2. Processa os dados, gera o gráfico e salva o arquivo
    processed_data <- momentum_formula(current_data)
    final_plot <- grafico_momentum(processed_data, titulo)
    filename <- glue("{output_dir_momentum}/{nome}.png")
    
    ggsave(
      filename,
      final_plot,
      width = 8.4, 
      height = 5, 
      dpi = 300,
      bg = "white"
    )
    
    print(glue("Gráfico para '{titulo}' salvo como '{filename}'"))
  })
