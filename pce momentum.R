#---------------------------------------------------------
# CARREGAR PACOTES
#---------------------------------------------------------
# Certifique-se de que os pacotes estão instalados
# install.packages(c("tidyverse", "tidyquant", "zoo", "scales", "glue"))
library(tidyverse)
library(tidyquant)
library(zoo)
library(scales)
library(glue)

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

#---------------------------------------------------------
# FUNÇÕES AUXILIARES: Para cálculo e plotagem
#---------------------------------------------------------
# Função para calcular as métricas de momentum
# MODIFICADA: Agora calcula o YoY usando a própria série SA, conforme solicitado.
momentum_formula <- function(data) {
  data |>
    mutate(
      # O cálculo YoY agora usa a coluna 'SA' para a variação de 12 meses
      YOY_SA = (SA / lag(SA, 12) - 1) * 100,
      MOM_SA = (SA / lag(SA, 1) - 1) * 100,
      SAAR_3MMA = ((1 + (rollmean(MOM_SA, k = 3, align = "right", fill = NA) / 100))^12 - 1) * 100,
      SAAR_6MMA = ((1 + (rollmean(MOM_SA, k = 6, align = "right", fill = NA) / 100))^12 - 1) * 100
    ) |>
    # Seleciona as colunas e renomeia para o gráfico
    select(date, `YOY SA` = YOY_SA, `3MMA-SAAR` = SAAR_3MMA, `6MMA-SAAR` = SAAR_6MMA) |>
    pivot_longer(cols = -date, names_to = "metric", values_to = "value") |>
    mutate(metric = factor(metric, levels = c("YOY SA", "3MMA-SAAR", "6MMA-SAAR")))
}

# Função para gerar o gráfico
grafico_momentum <- function(processed_data, chart_title) {
  
  # Define as cores para cada métrica
  metric_colors <- c("YOY SA" = "#082631", "3MMA-SAAR" = "#166083", "6MMA-SAAR" = "#37A6D9")
  
  # O eixo x agora começa em 2012-01-01
  ggplot(data = subset(processed_data, date >= as.Date("2012-01-01")),
         aes(x = date, y = value, color = metric, linetype = metric)) +
    geom_line(linewidth = 0.5) +
    # Linha de referência em 2% (meta de inflação do Fed)
    geom_hline(yintercept = 2.0, color = "#760A02", linetype = "solid", linewidth = 0.5) +
    scale_color_manual(values = metric_colors) +
    scale_linetype_manual(values = c("YOY SA" = "solid", "3MMA-SAAR" = "dashed", "6MMA-SAAR" = "dotted")) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
    scale_y_continuous(labels = label_number(suffix = "%", accuracy = 0.1)) +
    coord_cartesian(ylim = c(-5, 10)) + # Ajustado o limite do eixo y
    labs(
      title = chart_title,
      subtitle = "YoY / SAAR, %",
      caption = "Fonte: BEA | Impactus UFRJ",
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_family = "Montserrat") +
    theme(
      plot.title = element_text(size = 50, face = "bold", hjust = 0.5, margin = margin(b = 5)),
      plot.subtitle = element_text(size = 25, hjust = 0.5, margin = margin(b = 15), color = "black"),
      plot.caption = element_text(size = 25, hjust = 0.5, color = "black"),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 25),
      legend.key.width = unit(1.5, "cm"),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 25, color = "black"),
      axis.text.y = element_text(size = 25, color = "black"),
      panel.grid.major = element_line(color = "gray90", linetype = "dotted"),
      panel.grid.minor = element_blank()
    )
}

#---------------------------------------------------------
# MAPEAMENTO DAS SÉRIES E GERAÇÃO DOS GRÁFICOS
#---------------------------------------------------------
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

# Cria o diretório para salvar os gráficos, se não existir
output_dir <- "graficos_pce_momentum"
if (!dir.exists(output_dir)) dir.create(output_dir)

# Itera sobre o mapa de séries para processar e gerar cada gráfico
series_map_pce %>%
  pwalk(function(nome, titulo, SA_TICKER) {
    
    # 1. PREPARAR OS DADOS PARA A ITERAÇÃO ATUAL
    #    Seleciona a data e a coluna do ticker atual, renomeando-a para "SA".
    #    Isso cria o formato de entrada esperado pela função 'momentum_formula'.
    current_data <- tq_pce_tickers_wide %>%
      select(date, SA = all_of(SA_TICKER))
    
    # 2. Processa os dados, gera o gráfico e salva o arquivo
    processed_data <- momentum_formula(current_data)
    final_plot <- grafico_momentum(processed_data, titulo)
    filename <- glue("{output_dir}/{nome}_momentum.png")
    
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
