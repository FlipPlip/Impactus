#--------------------------------------------------------------------
# PASSO 0: PREPARAÇÃO DO AMBIENTE
#--------------------------------------------------------------------
library(dplyr)
library(glue)
library(gt)
library(purrr)
library(base64enc)
library(lubridate) # Adicionado para a função ymd()
library(scales)    # Adicionado para a função col_numeric()

#---------------------------------------------------------
# TABELA DE VARIAÇÃO MENSAL DO PCE
#---------------------------------------------------------
# FORMATAR PARA EXIBIÇÃO
#---------------------------------------------------------
# Assumindo que 'tabela_pce_index' e 'last_month_col' já existem no seu ambiente
# Se não, você precisará carregar ou criar esses objetos primeiro.
# Exemplo de criação (substitua pelos seus dados reais):
# last_month_col <- "2023-10-01"
# tabela_pce_index <- tibble(
#   titulo = c("PCE Price Index", "PCE less Food and Energy", "PCE Goods", "PCE Services"),
#   delta = c(0.2, 0.3, 0.1, 0.4),
#   `2023-08-01` = c(0.4, 0.1, 0.5, 0.3),
#   `2023-09-01` = c(0.4, 0.3, 0.3, 0.4),
#   `2023-10-01` = c(0.2, 0.2, 0.1, 0.3)
# )

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

# Salvar a tabela como um arquivo de imagem para usar no relatório
gtsave(tabela_final_pce_index, file = "tabela_variacao_pce_index.png")

print("Tabela salva como 'tabela_variacao_pce_index.png'")


#--------------------------------------------------------------------
# 1️⃣ TABELA DE RESUMO (Opcional, mas recomendado)
#--------------------------------------------------------------------
# Criando uma tabela simples com 'gt' para servir de introdução.
tabela_resumo <- tibble(
  `Seção` = c("Visão Geral do Consumo (PCE)", "Componentes do Consumo", "Renda Pessoal e Poupança", "Componentes da Renda"),
  `Descrição` = c(
    "Análise do índice, variação mensal (MoM) e trimestral (QoQ) do consumo agregado.",
    "Comparações diretas entre o consumo de Bens e Serviços, e a decomposição de Bens em Duráveis e Não Duráveis.",
    "Relação entre a Renda Disponível (DPI), o Consumo (PCE) e a Poupança Pessoal.",
    "Análise detalhada dos componentes da Renda Pessoal, como salários, transferências do governo e rendimentos de ativos."
  )
) %>%
  gt() %>%
  tab_header(
    title = "Relatório de Análise do Consumo Pessoal (PCE)",
    subtitle = "Estrutura do Relatório e Gráficos Apresentados"
  ) %>%
  cols_label(
    `Seção` = "Tópico de Análise",
    `Descrição` = "Conteúdo"
  ) %>%
  tab_options(
    table.width = pct(100),
    heading.title.font.size = px(24),
    heading.subtitle.font.size = px(16),
    column_labels.font.weight = "bold"
  )

# Convertendo a tabela GT para HTML puro
tabela_html <- tabela_resumo %>%
  as_raw_html()


#--------------------------------------------------------------------
# 2️⃣ FUNÇÃO AUXILIAR PARA CONVERTER IMAGEM
#--------------------------------------------------------------------
img_to_base64 <- function(path) {
  if (file.exists(path)) {
    dataURI(file = path, mime = "image/png")
  } else {
    warning(glue("Arquivo de imagem não encontrado: {path}"))
    return("") # Retorna string vazia se o arquivo não existir
  }
}

#--------------------------------------------------------------------
# 3️⃣ DEFINIÇÃO LÓGICA DOS PARES DE GRÁFICOS
#--------------------------------------------------------------------
output_dir <- "pce_graficos"

pares_df <- tribble(
  ~titulo, ~img1, ~img2,
  # --- Visão Geral do Consumo ---
  "Visão Geral do Consumo (PCE)", "01_pce_index.png", "03_pce_mom.png",
  "Variação Trimestral vs. Contribuição Mensal", "05_pce_qoq_saar.png", "04_pce_contribution_mom.png",
  
  # --- Bens vs. Serviços ---
  "Bens vs. Serviços (Nível - Índice 2019=100)", "07_pce_goods_index.png", "12_pce_services_index.png",
  "Bens vs. Serviços (Variação Mensal %)", "09_pce_goods_mom.png", "13_pce_services_mom.png",
  "Bens vs. Serviços (Variação Trimestral % SAAR)", "11_pce_goods_qoq.png", "15_pce_services_qoq.png",
  
  # --- Detalhes de Bens ---
  "Decomposição de Bens (Índice 2019=100)", "02_pce_goods_vs_services_index.png", "08_pce_durable_vs_nondurable_index.png",
  
  # --- Renda e Poupança ---
  "Renda Disponível vs. Consumo (Índice 2019=100)", "18_g3_realDPI_vs_realPCE_index.png", "19_g4_personal_saving_mom.png",
  "Renda Disponível vs. Consumo (Variação % 3MMA-SAAR)", "06_income_vs_pce_3mma_saar.png", "16_g1_wages_realDPI_realPCE_3mma_saar.png",
  
  # --- Componentes da Renda ---
  "Componentes da Renda Pessoal (Índice)", "21_g5_2_personal_income_breakdown_index.png", "20_g5_1_personal_income_breakdown_3mma_saar.png",
  "Componentes da Compensação (Índice vs. SAAR)", "23_g6_2_compensation_breakdown_index.png", "22_g6_1_compensation_breakdown_3mma_saar.png",
  "Outras Rendas e Transferências (Índice vs. SAAR)", "25_g7_2_other_incomes_transfers_index.png", "24_g7_1_other_incomes_transfers_3mma_saar.png",
  "Rendimentos de Ativos (Índice vs. SAAR)", "27_g8_2_receipts_on_assets_index.png", "26_g8_1_receipts_on_assets_3mma_saar.png",
  "Benefícios e Transferências (Índice)", "28_g9_current_transfer_receipts_index.png", "29_g10_govt_social_benefits_index.png"
) %>%
  # Monta o caminho completo para cada imagem
  mutate(
    img1_path = file.path(output_dir, img1),
    img2_path = file.path(output_dir, img2)
  )

#--------------------------------------------------------------------
# 4️⃣ MONTAGEM DO ARQUIVO HTML
#--------------------------------------------------------------------
html_file <- "relatorio_pce.html"
con <- file(html_file, open = "w", encoding = "UTF-8")

# --- Escreve o cabeçalho e o CSS ---
writeLines("<!DOCTYPE html>
<html lang='pt-BR'>
<head>
<meta charset='UTF-8'>
<link href='https://fonts.googleapis.com/css2?family=Montserrat:wght@400;700&display=swap' rel='stylesheet'>
<style>
  body { font-family: 'Montserrat', Arial, sans-serif; margin: 24px; background-color: #f9f9f9; color: #333; }
  .container { max-width: 1600px; margin: auto; }
  h1 { text-align: center; color: #082631; margin-bottom: 20px; }
  .gt_table { margin-bottom: 40px; }
  .single-image-container { text-align: center; margin-bottom: 40px; padding: 24px; background-color: #fff; border-radius: 8px; box-shadow: 0 4px 8px rgba(0,0,0,0.05); }
  .pair { display: flex; gap: 24px; align-items: flex-start; margin-bottom: 40px; padding: 24px; background-color: #fff; border-radius: 8px; box-shadow: 0 4px 8px rgba(0,0,0,0.05); }
  .col { flex: 1; min-width: 0; }
  .col h2 { margin-top: 0; margin-bottom: 15px; font-size: 20px; color: #166083; border-bottom: 2px solid #f0f0f0; padding-bottom: 10px; }
  img { max-width: 100%; height: auto; border-radius: 4px; border: 1px solid #ddd; }
  @media (max-width: 1200px) { .pair { flex-direction: column; } }
</style>
<title>Relatório de Análise - PCE</title>
</head>
<body>
<div class='container'>
<h1>Análise de Componentes do Consumo Pessoal (PCE)</h1>
", con)

# --- Insere a tabela de resumo ---
writeLines(tabela_html, con)

# --- Insere a tabela de variação do PCE (salva como imagem) ---
tabela_pce_base64 <- img_to_base64("tabela_variacao_pce_index.png")
if (tabela_pce_base64 != "") {
  writeLines(glue("
  <div class='single-image-container'>
    <img src='{tabela_pce_base64}' alt='Tabela de Variação Mensal do PCE' style='max-width: 800px; margin: auto; display: block;'>
  </div>
  "), con)
}

# --- Itera sobre os pares e insere os gráficos ---
pwalk(pares_df, function(titulo, img1_path, img2_path, ...) {
  img1_base64 <- img_to_base64(img1_path)
  img2_base64 <- img_to_base64(img2_path)
  
  html_chunk <- glue("
  <div class='pair'>
    <div class='col'>
      <h2>{titulo} - Gráfico 1</h2>
      <img src='{img1_base64}' alt='{img1_path}'>
    </div>
    <div class='col'>
      <h2>{titulo} - Gráfico 2</h2>
      <img src='{img2_base64}' alt='{img2_path}'>
    </div>
  </div>
  ")
  
  writeLines(html_chunk, con)
})


# --- Escreve o final do arquivo ---
writeLines("</div></body></html>", con)
close(con)

message(glue("✨ Relatório '{html_file}' gerado com sucesso!"))

#--------------------------------------------------------------------
# 5️⃣ ABRIR O RELATÓRIO NO NAVEGADOR
#--------------------------------------------------------------------
browseURL(html_file)
