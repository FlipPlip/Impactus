library(tidyquant)
library(tidyverse)
library(zoo)
library(scales)
library(glue)
library(ggplot2)

#--------------------------------------------------------------------
# PASSO 0: CAPTURA E TRATAMENTO DE DADOS
#--------------------------------------------------------------------

dgorder <- "DGORDER"

tq_dgorders <- tq_get(
  dgorder,
  get = "economic.data",
  from = "1960-01-01",
  to = Sys.Date()
) %>%
  pivot_wider(
    names_from = symbol,
    values_from = price
  )

tq_dgorger_metrics <- tq_dgorders %>%
  # reindexar cada série para 2019=100
  mutate(across(-date, ~ .x / mean(.x[format(date, "%Y")=="2019"], na.rm = TRUE) * 100,
                .names = "{.col}_2019")) %>%
  
  # MoM (já é SA)
  mutate(across(-date, ~ ( .x/lag(.x,1) - 1 )*100,
                .names = "{.col}_mom")) %>%
  
  # QoQ SAAR
  mutate(across(-date, ~ (( ( .x/lag(.x,3) )^(4) - 1 )*100),
                .names = "{.col}_qoq_saar")) %>%
  
  # 3MMA + SAAR
  mutate(across(-date,
                ~ zoo::rollmean(.x, k = 3, fill = NA, align = "right"),
                .names = "{.col}_3mma")) %>%
  mutate(across(ends_with("_3mma"),
                ~ (( (.x/lag(.x,3))^(4) -1 )*100),
                .names = "{.col}_saar"))

#--------------------------------------------------------------------
# PASSO 1: DEFINIÇÃO DO TEMA CUSTOMIZADO
#--------------------------------------------------------------------

tema_impactus <- function() {
  theme_minimal(base_family = "Montserrat") +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 5)),
      plot.subtitle = element_text(size = 9, hjust = 0.5, margin = margin(b = 15), color = "black"),
      plot.caption = element_text(size = 9, hjust = 0.5, color = "black"),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 9),
      legend.key.width = unit(1.5, "cm"),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 9, color = "black"),
      axis.text.y = element_text(size = 9, color = "black"),
      panel.grid.major = element_line(color = "gray90", linetype = "dotted"),
      panel.grid.minor = element_blank()
    )
}

#--------------------------------------------------------------------
# PASSO 2: GRÁFICOS
#--------------------------------------------------------------------

# Gráfico 1 - Índice de Durable Goods Orders
df_dgorder <- tq_dgorger_metrics %>%
  select(date, DGORDER_2019) %>%
  pivot_longer(-date, names_to = "categoria", values_to = "valor")

gg_dgorder_index <- ggplot(df_dgorder, aes(x = date, y = valor, color = categoria)) +
  geom_line(linewidth = 0.8) +
  labs(title = "Durable Goods Orders", subtitle = "Índice 2019 = 100", caption = "Fonte: FRED | Impactus UFRJ", x = NULL, y = NULL) +
  scale_color_manual(values = c("DGORDER_2019"="#082631"), labels = c("Durable Goods")) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 years") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)), breaks = scales::pretty_breaks()) +
  coord_cartesian(
    xlim = as.Date(c("2012-01-01", NA)),
    ylim = c(60, NA)
  ) +
  tema_impactus()

# Gráfico 2 - Variação Mensal
gg_dgorder_mom <- tq_dgorger_metrics %>%
  ggplot(aes(x = date, y = DGORDER_mom)) +
  geom_col(fill = "#082631") +
  labs(title = "Durable Goods Orders - Variação mensal", subtitle = "MoM SA, %", caption = "Fonte: FRED | Impactus UFRJ", x = NULL, y = NULL) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  scale_y_continuous(labels = label_number(suffix="%", accuracy=0.1), expand = expansion(mult = c(0.05, 0.1)), breaks = scales::pretty_breaks()) +
  coord_cartesian(
    xlim = as.Date(c("2022-01-01", NA)),
    ylim = c(-5, 5) # Ajustado o limite para melhor visualização
  ) +
  tema_impactus()


print(gg_dgorder_index)
print(gg_dgorder_mom)

# PASSO 3: SALVAR OS GRÁFICOS EM ARQUIVOS
#--------------------------------------------------------------------

# Definir o nome do diretório de saída
output_dir <- "dgorder_graficos"

# Criar o diretório se ele não existir
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Criar uma lista com os objetos de gráfico
plot_list <- list(
  gg_dgorder_index, 
  gg_dgorder_mom
)

# Criar um vetor com os nomes dos arquivos correspondentes
filenames <- c(
  "01_dgorder_index_2019.png", 
  "02_dgorder_variacao_mensal.png"
)

# Usar a função Map para iterar sobre as listas e salvar cada gráfico
Map(function(plot, filename) {
  ggsave(
    filename = filename,
    plot = plot,
    path = output_dir,
    width = 10,  # Largura em polegadas
    height = 6,   # Altura em polegadas
    dpi = 300     # Resolução (dots per inch)
  )
}, plot_list, filenames)

# Exibir mensagem de confirmação
message(glue("✨ Sucesso! Todos os {length(plot_list)} gráficos foram salvos na pasta '{output_dir}'."))

