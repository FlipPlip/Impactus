library(tidyverse)
library(tidyquant)
library(zoo)
library(scales)
library(glue)

#---------------------------------------------------------
# DEFINIR TICKERS E BAIXAR DADOS
#---------------------------------------------------------
# Vetor com todos os tickers necessários
pce_tickers <- c(
  "PCE",        # Personal Consumption Expenditures (nominal)
  "DGDSRC1",    # Goods (nominal)
  "PCEDG",      # Durable goods (nominal)
  "PCEND",      # Nondurable goods (nominal)
  "PCES",       # Services (nominal)
  
  "PCEC96",     # Real PCE
  "DGDSRX1",    # Real goods
  "PCEDGC96",   # Real durable goods
  "PCENDC96",   # Real nondurable goods
  "PCESC96",    # Real services
  "DSPI",       # Disposable personal income
  "DSPIC96",    # Real Disposable Personal Income
  
  "PMSAVE",     # Personal saving
  
  "W209RC1",    # Compensation of employees, received (total)
  "A576RC1",    # Wage and salary disbursements
  "A038RC1",    # Supplements to wages and salaries
  
  "PIROA",      # Personal income receipts on assets (total)
  "PII",        # Personal interest income
  "PDI",        # Personal dividend income
  
  "PCTR",       # Personal current transfer receipts (total)
  "B931RC1",    # Other current transfer receipts, from business (net)
  "A063RC1",    # Government social benefits to persons (total)
  "W823RC1",    # Social security
  "W824RC1",    # Medicare
  "W729RC1",    # Medicaid
  "W825RC1",    # Unemployment insurance
  "W826RC1",    # Veterans' benefits
  "W827RC1"     # Other government social benefits
)

#---------------------------------------------------------
# BAIXAR OS DADOS DO FRED E FORMATAR EM WIDE FORMAT
#---------------------------------------------------------
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

#--------------------------------------------------------------------
# PASSO 0: PREPARAÇÃO DO AMBIENTE
#--------------------------------------------------------------------



#---------------------------------------------------------
# BAIXAR OS DADOS (mantém o que já fizemos)
#---------------------------------------------------------
#---------------------------------------------------------
# ADICIONAR MÉTRICAS PARA CADA SÉRIE
#---------------------------------------------------------
tq_pce_metrics <- tq_pce_tickers_wide %>%
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

# Assumindo que os dataframes 'tq_pce_metrics' e 'tq_pce_tickers_wide' já existem.

#--------------------------------------------------------------------
# PASSO 2: GERAÇÃO DE TODOS OS GRÁFICOS
#--------------------------------------------------------------------

# Gráfico 1
gg_pce <- tq_pce_metrics %>%
  ggplot(aes(x = date, y = PCE_2019)) +
  geom_line(color = "#082631", linewidth = 0.8) +
  labs(title = "Personal Consumption Expenditures (PCE)", subtitle = "Índice 2019 = 100", caption = "Fonte: BEA | Impactus UFRJ", x = NULL, y = NULL) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)), breaks = scales::pretty_breaks()) +
  coord_cartesian(
    xlim = as.Date(c("2012-01-01", NA)),
    ylim = c(70, NA)
  ) +
  tema_impactus()

# Gráfico 2
df_goods_services <- tq_pce_metrics %>% select(date, DGDSRC1_2019, PCES_2019) %>% pivot_longer(-date, names_to = "categoria", values_to = "valor")
gg_goods_services <- ggplot(df_goods_services, aes(x = date, y = valor, color = categoria)) +
  geom_line(linewidth = 0.8) +
  labs(title = "PCE: Goods vs Services", subtitle = "Índice 2019 = 100", caption = "Fonte: BEA | Impactus UFRJ", x = NULL, y = NULL) +
  scale_color_manual(values = c("DGDSRC1_2019"="#082631","PCES_2019"="#166083"), labels = c("Goods","Services")) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)), breaks = scales::pretty_breaks()) +
  coord_cartesian(
    xlim = as.Date(c("2012-01-01", NA)),
    ylim = c(60, NA)
  ) +
  tema_impactus()

# Gráfico 3
gg_pce_mom <- tq_pce_metrics %>%
  ggplot(aes(x = date, y = PCE_mom)) +
  geom_col(fill = "#166083") +
  labs(title = "PCE - Variação mensal", subtitle = "MoM SA, %", caption = "Fonte: BEA | Impactus UFRJ", x = NULL, y = NULL) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  scale_y_continuous(labels = label_number(suffix="%", accuracy=0.1), expand = expansion(mult = c(0.05, 0.1)), breaks = scales::pretty_breaks()) +
  coord_cartesian(
    xlim = as.Date(c("2022-01-01", NA)),
    ylim = c(-1, 2)
  ) +
  tema_impactus()

# Gráfico 4
df_contrib <- tq_pce_metrics %>% select(date, PCEDGC96_mom, PCENDC96_mom, PCESC96_mom, PCEC96_mom) %>% rename(Durable=PCEDGC96_mom, Nondurable=PCENDC96_mom, Services=PCESC96_mom, Total=PCEC96_mom) %>% pivot_longer(cols = c(Durable,Nondurable,Services), names_to = "categoria", values_to = "valor")
gg_contrib <- ggplot() +
  geom_col(data = df_contrib, aes(x = date, y = valor, fill = categoria)) +
  geom_line(data = tq_pce_metrics, aes(x = date, y = PCEC96_mom, group=1), color = "#082631", linewidth=0.8) +
  labs(title = "Contribuição ao crescimento do Real PCE", subtitle = "MoM SA, % (colunas) com linha do total", caption = "Fonte: BEA | Impactus UFRJ", x = NULL, y = NULL) +
  scale_fill_manual(values=c("Durable"="#37A6D9","Nondurable"="#166083","Services"="#082631")) +
  scale_y_continuous(labels = label_number(suffix="%", accuracy=0.1), expand = expansion(mult = c(0.05, 0.1)), breaks = scales::pretty_breaks()) +
  coord_cartesian(
    xlim = as.Date(c("2022-01-01", NA)),
    ylim = c(-2, 5)
  ) +
  tema_impactus()

# Gráfico 5
gg_pce_qoq <- tq_pce_metrics %>%
  ggplot(aes(x = date, y = PCE_qoq_saar)) +
  geom_col(fill = "#37A6D9") +
  labs(title = "PCE - Variação trimestral anualizada", subtitle = "QoQ SAAR, %", caption = "Fonte: BEA | Impactus UFRJ", x = NULL, y = NULL) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  scale_y_continuous(labels = label_number(suffix="%", accuracy=0.1), expand = expansion(mult = c(0.05, 0.1)), breaks = scales::pretty_breaks()) +
  coord_cartesian(
    xlim = as.Date(c("2022-01-01", NA)),
    ylim = c(-1, 10)
  ) +
  tema_impactus()

# Gráfico 6
df_income_pce <- tq_pce_metrics %>% select(date, DSPIC96_3mma_saar, PCEC96_3mma_saar) %>% pivot_longer(-date, names_to = "serie", values_to = "valor")
gg_income_pce <- ggplot(df_income_pce, aes(x = date, y = valor, color = serie)) +
  geom_line(linewidth=0.8) +
  labs(title = "Real Disposable Income vs Real PCE", subtitle = "3MMA SAAR, %", caption = "Fonte: BEA | Impactus UFRJ", x = NULL, y = NULL) +
  scale_color_manual(values = c("DSPIC96_3mma_saar"="#082631","PCEC96_3mma_saar"="#166083"), labels = c("Real Disposable Income","Real PCE")) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  scale_y_continuous(labels = label_number(suffix="%", accuracy=0.1), expand = expansion(mult = c(0.05, 0.05)), breaks = scales::pretty_breaks()) +
  coord_cartesian(
    xlim = as.Date(c("2012-01-01", NA)),
    ylim = c(-5, 10)
  ) +
  tema_impactus()

# Gráfico 7
gg_goods <- tq_pce_metrics %>%
  ggplot(aes(x = date, y = DGDSRC1_2019)) +
  geom_line(color = "#082631", linewidth = 0.8) +
  labs(title = "PCE - Goods", subtitle = "Índice 2019 = 100", caption = "Fonte: BEA | Impactus UFRJ", x = NULL, y = NULL) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)), breaks = scales::pretty_breaks()) +
  coord_cartesian(
    xlim = as.Date(c("2012-01-01", NA)),
    ylim = c(70, NA)
  ) +
  tema_impactus()

# Gráfico 8
df_dur_nondur <- tq_pce_metrics %>% select(date, PCEDG_2019, PCEND_2019) %>% pivot_longer(-date, names_to = "categoria", values_to = "valor")
gg_dur_nondur <- ggplot(df_dur_nondur, aes(x = date, y = valor, color = categoria)) +
  geom_line(linewidth = 0.8) +
  labs(title = "PCE - Durable vs Non Durable Goods", subtitle = "Índice 2019 = 100", caption = "Fonte: BEA | Impactus UFRJ", x = NULL, y = NULL) +
  scale_color_manual(values=c("PCEDG_2019"="#082631","PCEND_2019"="#166083"), labels=c("Durable","Non Durable")) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)), breaks = scales::pretty_breaks()) +
  coord_cartesian(
    xlim = as.Date(c("2012-01-01", NA)),
    ylim = c(60, NA)
  ) +
  tema_impactus()

# Gráfico 9
gg_goods_mom <- tq_pce_metrics %>%
  ggplot(aes(x = date, y = DGDSRC1_mom)) +
  geom_col(fill = "#166083") +
  labs(title = "PCE - Goods", subtitle = "MoM SA, %", caption = "Fonte: BEA | Impactus UFRJ", x = NULL, y = NULL) +
  scale_y_continuous(labels = label_number(suffix="%", accuracy=0.1), expand = expansion(mult = c(0.05, 0.1)), breaks = scales::pretty_breaks()) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  coord_cartesian(
    xlim = as.Date(c("2022-01-01", NA)),
    ylim = c(-2, 5)
  ) +
  tema_impactus()

# Gráfico 10
df_goods_yoy <- tq_pce_metrics %>% mutate(DGDSRC1_yoy = (DGDSRC1/lag(DGDSRC1,12)-1)*100) %>% select(date, DGDSRC1_yoy, DGDSRC1_3mma_saar) %>% pivot_longer(-date, names_to = "metric", values_to = "value")
gg_goods_yoy_saar <- ggplot(df_goods_yoy, aes(x = date, y = value, color = metric, linetype = metric)) +
  geom_line(linewidth=0.8) +
  geom_hline(yintercept=2, color="#760A02", linetype="solid", linewidth=0.5) +
  scale_color_manual(values=c("DGDSRC1_yoy"="#082631","DGDSRC1_3mma_saar"="#166083"), labels=c("YoY SA","3MMA-SAAR")) +
  scale_linetype_manual(values=c("DGDSRC1_yoy"="solid","DGDSRC1_3mma_saar"="dashed")) +
  labs(title = "PCE - Goods", subtitle = "YoY vs 3MMA-SAAR, %", caption = "Fonte: BEA | Impactus UFRJ", x=NULL, y=NULL) +
  scale_y_continuous(labels=label_number(suffix="%",accuracy=0.1), breaks = scales::pretty_breaks()) +
  scale_x_date(date_labels="%b %Y", date_breaks="6 months") +
  coord_cartesian(xlim = as.Date(c("2022-01-01", NA)), ylim=c(-0.5, 7.5)) +
  tema_impactus()

# Gráfico 11
gg_goods_qoq <- tq_pce_metrics %>%
  ggplot(aes(x = date, y = DGDSRC1_qoq_saar)) +
  geom_col(fill = "#37A6D9") +
  labs(title = "PCE - Goods", subtitle = "QoQ SAAR, %", caption = "Fonte: BEA | Impactus UFRJ", x=NULL, y=NULL) +
  scale_y_continuous(labels=label_number(suffix="%",accuracy=0.1), expand = expansion(mult = c(0.05, 0.1)), breaks = scales::pretty_breaks()) +
  scale_x_date(date_labels="%b %Y", date_breaks="6 months") +
  coord_cartesian(
    xlim = as.Date(c("2022-01-01", NA)),
    ylim = c(-2, 10)
  ) +
  tema_impactus()

# Gráfico 12
gg_services <- tq_pce_metrics %>%
  ggplot(aes(x = date, y = PCES_2019)) +
  geom_line(color = "#082631", linewidth = 0.8) +
  labs(title = "PCE - Services", subtitle = "Índice 2019 = 100", caption = "Fonte: BEA | Impactus UFRJ", x=NULL, y=NULL) +
  scale_x_date(date_labels="%b %Y", date_breaks="6 months") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)), breaks = scales::pretty_breaks()) +
  coord_cartesian(
    xlim = as.Date(c("2012-01-01", NA)),
    ylim = c(60, NA)
  ) +
  tema_impactus()

# Gráfico 13
gg_services_mom <- tq_pce_metrics %>%
  ggplot(aes(x = date, y = PCES_mom)) +
  geom_col(fill = "#166083") +
  labs(title = "PCE - Services", subtitle = "MoM SA, %", caption = "Fonte: BEA | Impactus UFRJ", x=NULL, y=NULL) +
  scale_y_continuous(labels=label_number(suffix="%",accuracy=0.1), expand = expansion(mult = c(0.05, 0.1)), breaks = scales::pretty_breaks()) +
  scale_x_date(date_labels="%b %Y", date_breaks="6 months") +
  coord_cartesian(
    xlim = as.Date(c("2022-01-01", NA)),
    ylim = c(-1, 2)
  ) +
  tema_impactus()

# Gráfico 14
df_services_yoy <- tq_pce_metrics %>% mutate(PCES_yoy = (PCES/lag(PCES,12)-1)*100) %>% select(date, PCES_yoy, PCES_3mma_saar) %>% pivot_longer(-date, names_to="metric", values_to="value")
gg_services_yoy_saar <- ggplot(df_services_yoy, aes(x=date,y=value,color=metric,linetype=metric)) +
  geom_line(linewidth=0.8) +
  geom_hline(yintercept=2,color="#760A02",linetype="solid",linewidth=0.5) +
  scale_color_manual(values=c("PCES_yoy"="#082631","PCES_3mma_saar"="#166083"), labels=c("YoY SA","3MMA-SAAR")) +
  scale_linetype_manual(values=c("PCES_yoy"="solid","PCES_3mma_saar"="dashed")) +
  labs(title = "PCE - Services", subtitle = "YoY vs 3MMA-SAAR, %", caption = "Fonte: BEA | Impactus UFRJ", x=NULL, y=NULL) +
  scale_y_continuous(labels=label_number(suffix="%",accuracy=0.1), breaks = scales::pretty_breaks()) +
  scale_x_date(date_labels="%b %Y", date_breaks="6 months") +
  coord_cartesian(
    xlim = as.Date(c("2022-01-01", NA)),
    ylim = c(0, 10)
  ) +
  tema_impactus()

# Gráfico 15
gg_services_qoq <- tq_pce_metrics %>%
  ggplot(aes(x = date, y = PCES_qoq_saar)) +
  geom_col(fill = "#37A6D9") +
  labs(title = "PCE - Services", subtitle = "QoQ SAAR, %", caption = "Fonte: BEA | Impactus UFRJ", x=NULL, y=NULL) +
  scale_y_continuous(labels=label_number(suffix="%",accuracy=0.1), expand = expansion(mult = c(0.05, 0.1)), breaks = scales::pretty_breaks()) +
  scale_x_date(date_labels="%b %Y", date_breaks="6 months") +
  coord_cartesian(
    xlim = as.Date(c("2022-01-01", NA)),
    ylim = c(-2, 20)
  ) +
  tema_impactus()

# --- Gráficos de Análise de Renda ---
last_print <- format(max(tq_pce_metrics$date, na.rm = TRUE), "%b-%Y")
pal3 <- c("#082631", "#166083", "#37A6D9")
pal2 <- c("#082631", "#166083")

# G1
df1 <- tq_pce_metrics %>% transmute(date, `Wages and salaries` = A576RC1_3mma_saar, `Real disposable personal income` = DSPIC96_3mma_saar, `Real PCE` = PCEC96_3mma_saar) %>% pivot_longer(-date, names_to = "serie", values_to = "valor")
g1 <- ggplot(df1, aes(date, valor, color = serie)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = pal3) +
  scale_y_continuous(labels = scales::label_number(suffix = "%", accuracy = 0.1), expand = expansion(mult = c(0.05, 0.05)), breaks = scales::pretty_breaks()) +
  labs(title = "US PCE: Wages, Real DPI, Real PCE", subtitle = glue::glue("3MMA-SAAR | Last print: {last_print}"), caption = "Fonte: BEA | Impactus UFRJ", x = NULL, y = NULL) +
  coord_cartesian(
    xlim = as.Date(c("2012-01-01", NA)),
    ylim = c(-5, 10)
  ) +
  tema_impactus()

# G2
df2 <- tq_pce_metrics %>% transmute(date, `Real DPI` = DSPIC96_mom, `Real PCE` = PCEC96_mom) %>% pivot_longer(-date, names_to = "serie", values_to = "valor")
g2 <- ggplot(df2, aes(date, valor, fill = serie)) +
  geom_col(position = position_dodge(width = 25)) +
  scale_fill_manual(values = pal2) +
  scale_y_continuous(labels = scales::label_number(suffix = "%", accuracy = 0.1), expand = expansion(mult = c(0.05, 0.1)), breaks = scales::pretty_breaks()) +
  labs(title = "US PCE: Real DPI vs Real PCE", subtitle = glue::glue("MoM SA, % | Last print: {last_print}"), caption = "Fonte: BEA | Impactus UFRJ", x = NULL, y = NULL) +
  coord_cartesian(
    xlim = as.Date(c("2022-01-01", NA)),
    ylim = c(-1, 1)
  ) +
  tema_impactus()

# G3
df3 <- tq_pce_metrics %>% transmute(date, `Real DPI` = DSPIC96_2019, `Real PCE` = PCEC96_2019) %>% pivot_longer(-date, names_to = "serie", values_to = "valor")
g3 <- ggplot(df3, aes(date, valor, color = serie)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = pal2) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)), breaks = scales::pretty_breaks()) +
  labs(title = "US PCE: Real DPI & Real PCE", subtitle = glue::glue("Index, 2019 = 100 | Last print: {last_print}"), caption = "Fonte: BEA | Impactus UFRJ", x = NULL, y = NULL) +
  coord_cartesian(
    xlim = as.Date(c("2012-01-01", NA)),
    ylim = c(70, NA)
  ) +
  tema_impactus()

# G4
g4 <- tq_pce_metrics %>%
  ggplot(aes(date, PMSAVE_mom)) +
  geom_col(fill = "#082631") +
  scale_y_continuous(labels = scales::label_number(suffix = "%", accuracy = 0.1), expand = expansion(mult = c(0.05, 0.1)), breaks = scales::pretty_breaks()) +
  labs(title = "US PCE: Personal Saving", subtitle = glue::glue("MoM SA, % | Last print: {last_print}"), caption = "Fonte: BEA | Impactus UFRJ", x = NULL, y = NULL) +
  coord_cartesian(
    xlim = as.Date(c("2022-01-01", NA)),
    ylim = c(-15, 15)
  ) +
  tema_impactus()

# G5.1
df51 <- tq_pce_metrics %>% transmute(date, `Personal income` = DSPI_3mma_saar, `Personal income receipts on assets` = PIROA_3mma_saar, `Compensation of employees` = W209RC1_3mma_saar, `Personal current transfer receipts` = PCTR_3mma_saar) %>% pivot_longer(-date, names_to = "serie", values_to = "valor")
g5_1 <- ggplot(df51, aes(date, valor, color = serie)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c("#082631","#A12A2A","#1B9AAA","#7A9A01")) +
  scale_y_continuous(labels = scales::label_number(suffix = "%", accuracy = 0.1), expand = expansion(mult = c(0.05, 0.05)), breaks = scales::pretty_breaks()) +
  labs(title = "US PCE: Personal Income Breakdown", subtitle = glue::glue("3MMA-SAAR | Last print: {last_print}"), caption = "Fonte: BEA | Impactus UFRJ", x = NULL, y = NULL) +
  coord_cartesian(
    xlim = as.Date(c("2012-01-01", NA)),
    ylim = c(-10, 20)
  ) + 
  tema_impactus()

# G5.2
df52 <- tq_pce_metrics %>% transmute(date, `Personal income` = DSPI_2019, `Personal income receipts on assets` = PIROA_2019, `Compensation of employees` = W209RC1_2019, `Personal current transfer receipts` = PCTR_2019) %>% pivot_longer(-date, names_to = "serie", values_to = "valor")
g5_2 <- ggplot(df52, aes(date, valor, color = serie)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c("#082631","#A12A2A","#1B9AAA","#7A9A01")) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)), breaks = scales::pretty_breaks()) +
  labs(title = "US PCE: Personal Income Breakdown", subtitle = glue::glue("Index, 2019 = 100 | Last print: {last_print}"), caption = "Fonte: BEA | Impactus UFRJ", x = NULL, y = NULL) +
  coord_cartesian(
    xlim = as.Date(c("2012-01-01", NA)),
    ylim = c(50, NA)
  ) +
  tema_impactus()

# G6.1
df61 <- tq_pce_metrics %>% transmute(date, `Wages and salaries` = A576RC1_3mma_saar, `Supplements to wages and salaries` = A038RC1_3mma_saar) %>% pivot_longer(-date, names_to = "serie", values_to = "valor")
g6_1 <- ggplot(df61, aes(date, valor, color = serie)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = pal2) +
  scale_y_continuous(labels = scales::label_number(suffix = "%", accuracy = 0.1), expand = expansion(mult = c(0.05, 0.05)), breaks = scales::pretty_breaks()) +
  labs(title = "US PCE: Compensation of Employees Breakdown", subtitle = glue::glue("3MMA-SAAR | Last print: {last_print}"), caption = "Fonte: BEA | Impactus UFRJ", x = NULL, y = NULL) +
  coord_cartesian(
    xlim = as.Date(c("2012-01-01", NA)),
    ylim = c(0, 15)
  ) +
  tema_impactus()

# G6.2
df62 <- tq_pce_metrics %>% transmute(date, `Wages and salaries` = A576RC1_2019, `Supplements to wages and salaries` = A038RC1_2019) %>% pivot_longer(-date, names_to = "serie", values_to = "valor")
g6_2 <- ggplot(df62, aes(date, valor, color = serie)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = pal2) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)), breaks = scales::pretty_breaks()) +
  labs(title = "US PCE: Compensation of Employees Breakdown", subtitle = glue::glue("Index, 2019 = 100 | Last print: {last_print}"), caption = "Fonte: BEA | Impactus UFRJ", x = NULL, y = NULL) +
  coord_cartesian(
    xlim = as.Date(c("2012-01-01", NA)),
    ylim = c(50, NA)
  ) +
  tema_impactus()

# G7.1
df71 <- tq_pce_metrics %>% transmute(date, `Personal income receipts on assets` = PIROA_3mma_saar, `Personal current transfer receipts` = PCTR_3mma_saar) %>% pivot_longer(-date, names_to = "serie", values_to = "valor")
g7_1 <- ggplot(df71, aes(date, valor, color = serie)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = pal2) +
  scale_y_continuous(labels = scales::label_number(suffix = "%", accuracy = 0.1), expand = expansion(mult = c(0.05, 0.05)), breaks = scales::pretty_breaks()) +
  labs(title = "US PCE: Other Incomes and Transfers", subtitle = glue::glue("3MMA-SAAR | Last print: {last_print}"), caption = "Fonte: BEA | Impactus UFRJ", x = NULL, y = NULL) +
  coord_cartesian(
    xlim = as.Date(c("2012-01-01", NA)),
    ylim = c(-20, 20)
  ) +
  tema_impactus()

# G7.2
df72 <- tq_pce_metrics %>% transmute(date, `Personal income receipts on assets` = PIROA_2019, `Personal current transfer receipts` = PCTR_2019) %>% pivot_longer(-date, names_to = "serie", values_to = "valor")
g7_2 <- ggplot(df72, aes(date, valor, color = serie)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = pal2) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)), breaks = scales::pretty_breaks()) +
  labs(title = "US PCE: Other Incomes and Transfers", subtitle = glue::glue("Index, 2019 = 100 | Last print: {last_print}"), caption = "Fonte: BEA | Impactus UFRJ", x = NULL, y = NULL) +
  coord_cartesian(
    xlim = as.Date(c("2012-01-01", NA)),
    ylim = c(50, NA)
  ) +
  tema_impactus()

# G8.1
df81 <- tq_pce_metrics %>% transmute(date, `Personal interest income` = PII_3mma_saar, `Personal dividend income` = PDI_3mma_saar) %>% pivot_longer(-date, names_to = "serie", values_to = "valor")
g8_1 <- ggplot(df81, aes(date, valor, color = serie)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = pal2) +
  scale_y_continuous(labels = scales::label_number(suffix = "%", accuracy = 0.1), expand = expansion(mult = c(0.05, 0.05)), breaks = scales::pretty_breaks()) +
  labs(title = "US PCE: Personal Income Receipts on Assets", subtitle = glue::glue("3MMA-SAAR | Last print: {last_print}"), caption = "Fonte: BEA | Impactus UFRJ", x = NULL, y = NULL) +
  coord_cartesian(
    xlim = as.Date(c("2012-01-01", NA)),
    ylim = c(-25, 25)
  ) +
  tema_impactus()

# G8.2
df82 <- tq_pce_metrics %>% transmute(date, `Personal interest income` = PII_2019, `Personal dividend income` = PDI_2019) %>% pivot_longer(-date, names_to = "serie", values_to = "valor")
g8_2 <- ggplot(df82, aes(date, valor, color = serie)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = pal2) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)), breaks = scales::pretty_breaks()) +
  labs(title = "US PCE: Personal Income Receipts on Assets", subtitle = glue::glue("Index, 2019 = 100 | Last print: {last_print}"), caption = "Fonte: BEA | Impactus UFRJ", x = NULL, y = NULL) +
  coord_cartesian(
    xlim = as.Date(c("2012-01-01", NA)),
    ylim = c(50, NA)
  ) +
  tema_impactus()

# G9
df9 <- tq_pce_metrics %>% transmute(date, `Government social benefits to persons` = A063RC1_2019, `Other current transfer receipts` = B931RC1_2019) %>% pivot_longer(-date, names_to = "serie", values_to = "valor")
g9 <- ggplot(df9, aes(date, valor, color = serie)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = pal2) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)), breaks = scales::pretty_breaks()) +
  labs(title = "US PCE: Personal Current Transfer Receipts", subtitle = glue::glue("Index, 2019 = 100 | Last print: {last_print}"), caption = "Fonte: BEA | Impactus UFRJ", x = NULL, y = NULL) +
  coord_cartesian(
    xlim = as.Date(c("2012-01-01", NA)),
    ylim = c(50, NA)
  ) +
  tema_impactus()

# G10
df10 <- tq_pce_metrics %>% transmute(date, `Social security` = W823RC1_2019, `Unemployment insurance` = W825RC1_2019, `Medicare` = W824RC1_2019, `Medicaid` = W729RC1_2019, `Veterans' benefits` = W826RC1_2019, `Other` = W827RC1_2019) %>% pivot_longer(-date, names_to = "serie", values_to = "valor")
g10 <- ggplot(df10, aes(date, valor, color = serie)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c("#082631","#7A9A01","#1B9AAA","#A12A2A","#E39A27","#6B5CA5")) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)), breaks = scales::pretty_breaks()) +
  labs(title = "US PCE: Government Social Benefits to Persons", subtitle = glue::glue("Index, 2019 = 100 | Last print: {last_print}"), caption = "Fonte: BEA | Impactus UFRJ", x = NULL, y = NULL) +
  coord_cartesian(
    xlim = as.Date(c("2012-01-01", NA)),
    ylim = c(NA, 250)
  ) +
  tema_impactus()


#--------------------------------------------------------------------
# PASSO 3: SALVAR TODOS OS GRÁFICOS
#--------------------------------------------------------------------

output_dir <- "pce_graficos"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

plot_list <- list(
  gg_pce, gg_goods_services, gg_pce_mom, gg_contrib, gg_pce_qoq, gg_income_pce, 
  gg_goods, gg_dur_nondur, gg_goods_mom, gg_goods_yoy_saar, gg_goods_qoq, 
  gg_services, gg_services_mom, gg_services_yoy_saar, gg_services_qoq,
  g1, g2, g3, g4, g5_1, g5_2, g6_1, g6_2, g7_1, g7_2, g8_1, g8_2, g9, g10
)

filenames <- c(
  "01_pce_index.png", "02_pce_goods_vs_services_index.png", "03_pce_mom.png", 
  "04_pce_contribution_mom.png", "05_pce_qoq_saar.png", "06_income_vs_pce_3mma_saar.png",
  "07_pce_goods_index.png", "08_pce_durable_vs_nondurable_index.png", "09_pce_goods_mom.png",
  "10_pce_goods_yoy_vs_saar.png", "11_pce_goods_qoq.png", "12_pce_services_index.png",
  "13_pce_services_mom.png", "14_pce_services_yoy_vs_saar.png", "15_pce_services_qoq.png",
  "16_g1_wages_realDPI_realPCE_3mma_saar.png", "17_g2_realDPI_vs_realPCE_mom.png",
  "18_g3_realDPI_vs_realPCE_index.png", "19_g4_personal_saving_mom.png",
  "20_g5_1_personal_income_breakdown_3mma_saar.png", "21_g5_2_personal_income_breakdown_index.png",
  "22_g6_1_compensation_breakdown_3mma_saar.png", "23_g6_2_compensation_breakdown_index.png",
  "24_g7_1_other_incomes_transfers_3mma_saar.png", "25_g7_2_other_incomes_transfers_index.png",
  "26_g8_1_receipts_on_assets_3mma_saar.png", "27_g8_2_receipts_on_assets_index.png",
  "28_g9_current_transfer_receipts_index.png", "29_g10_govt_social_benefits_index.png"
)

Map(function(plot, filename) {
  ggsave(
    filename = filename,
    plot = plot,
    path = output_dir,
    width = 10,
    height = 6,
    dpi = 300
  )
}, plot_list, filenames)

message(glue("✨ Sucesso! Todos os {length(plot_list)} gráficos foram salvos na pasta '{output_dir}'."))