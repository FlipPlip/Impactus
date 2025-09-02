library(dplyr)
library(glue)
library(gt)
library(base64enc)
library(purrr)

# ---------------------------------------------------------
# 1️⃣ Transformar a tabela GT em HTML
# ---------------------------------------------------------
tabela_html <- tabela_final_pce %>% 
  as_raw_html()

# ---------------------------------------------------------
# 2️⃣ Função para converter imagem PNG em Base64
# ---------------------------------------------------------
img_to_base64 <- function(path) {
  if (file.exists(path)) {
    dataURI(file = path, mime = "image/png")
  } else {
    warning(glue("Arquivo não encontrado: {path}"))
    return("")
  }
}

# ---------------------------------------------------------
# 3️⃣ Montar dataframe com caminhos das imagens
# ---------------------------------------------------------
# Diretórios de gráficos já gerados
momentum_dir <- "graficos_pce_momentum"
variacao_dir <- "graficos_pce_variacao_mensal"

pairs_df <- series_map_pce %>%
  transmute(
    titulo,
    mom_img = file.path(momentum_dir, paste0(nome, "_momentum.png")),
    var_img = file.path(variacao_dir, paste0(nome, "_variacao_mensal.png"))
  )

# Filtra apenas pares que existem
missing <- pairs_df %>%
  filter(!file.exists(mom_img) | !file.exists(var_img))
if (nrow(missing) > 0) {
  message("Aviso: há pares sem arquivo. Serão ignorados no HTML:\n",
          paste0("- ", missing$titulo, collapse = "\n"))
}
pairs_df <- pairs_df %>%
  filter(file.exists(mom_img) & file.exists(var_img))

# ---------------------------------------------------------
# 4️⃣ Criar HTML completo
# ---------------------------------------------------------
html_file <- "pce_paired_plots.html"
con <- file(html_file, open = "w", encoding = "UTF-8")

writeLines("<!DOCTYPE html>
<html>
<head>
<meta charset='UTF-8'>
<style>
body { font-family: Montserrat, Arial, sans-serif; margin: 24px; }
h1 { margin-top: 0; }
.pair { display: flex; gap: 20px; align-items: flex-start; margin-bottom: 28px; }
.col { flex: 1 1 0; }
.col h2 { margin: 0 0 10px 0; font-size: 20px; }
img { width: 100%; height: auto; border: 1px solid #eee; }
@media (max-width: 1200px) { .pair { flex-direction: column; } }
</style>
<title>PCE - Momentum vs Variação Mensal</title>
</head>
<body>
<h1>PCE - Momentum vs Variação Mensal</h1>
", con)

# ---------------------------------------------------------
# 5️⃣ Inserir tabela GT no início
# ---------------------------------------------------------
writeLines(tabela_html, con)

# ---------------------------------------------------------
# 6️⃣ Inserir pares de gráficos (convertidos para Base64)
# ---------------------------------------------------------
apply(pairs_df, 1, function(r) {
  mom_base64 <- img_to_base64(r[['mom_img']])
  var_base64 <- img_to_base64(r[['var_img']])
  
  writeLines(glue("
  <div class='pair'>
    <div class='col'>
      <h2>{r[['titulo']]}</h2>
      <img src='{mom_base64}' alt='Momentum'>
    </div>
    <div class='col'>
      <img src='{var_base64}' alt='Variação Mensal'>
    </div>
  </div>
  "), con)
})

writeLines("</body></html>", con)
close(con)

# Abrir HTML no navegador
browseURL(html_file)
