# ============================================================================
# Prática Estatística III - Projeto I
# Disfunções Endócrinas em Pacientes com DHEM (Sub-projeto: Sarcopenia)
# ----------------------------------------------------------------------------
# Objetivo: gerar os insumos das análises descritivas univariadas e bivariadas
#   - tabelas LaTeX (numéricas, categóricas e cruzamento por DHEM/fibrose)
#   - gráficos vetoriais (.pdf) univariados e bivariados
#   - macros LaTeX dinâmicas (outputs/macros.tex)
#
# Uso: basta executar `Rscript script_principal.R` na raiz do projeto.
# Os resultados são gravados na pasta `outputs/`.
# ============================================================================

# ---- 0. Configuração -------------------------------------------------------

suppressPackageStartupMessages({
  library(MASS) # carregado antes do tidyverse para não mascarar dplyr::select
  library(tidyverse)
  library(readxl)
  library(kableExtra)
  library(patchwork)
  library(scales)
  library(broom)
  library(ResourceSelection) # Hosmer-Lemeshow (calibração)
  library(car)              # VIF / GVIF (multicolinearidade)
  library(pROC)             # ROC / AUC (discriminação)
})

dir_saida <- "outputs"
dir.create(dir_saida, showWarnings = FALSE)
set.seed(42) # reprodutibilidade (testes de Fisher por simulação, etc.)

# Device PDF (vetorial). Se o cairo estiver funcional, use "cairo_pdf" para
# suporte Unicode completo; o device padrão substitui "≥" por ">=" nos rótulos.
device_pdf <- "pdf"

# Formatação de números no padrão brasileiro (vírgula decimal)
fmt_num <- function(x, d = 1) {
  ifelse(is.na(x), "—", formatC(x, digits = d, format = "f", decimal.mark = ","))
}

fmt_pct <- function(x) {
  fmt_num(x, 1)
}

# p-valor formatado: "<0,001" para valores muito pequenos
fmt_p <- function(p) {
  ifelse(
    is.na(p),
    "—",
    ifelse(p < 0.001, "< 0,001", fmt_num(round(p, 3), 3))
  )
}

shapiro_p <- function(x) {
  n <- sum(!is.na(x))
  if (n >= 3) shapiro.test(x)$p.value else NA_real_
}

# Remove o ambiente "table" criado pelo kableExtra (o relatório adiciona
# caption/legenda e controle de posicionamento próprios)
tex_sem_table_env <- function(x) {
  x <- sub(
    "\\\\begin\\{table\\}\\[!h\\]\\n(?:\\\\caption\\{[^\\n]*\\}\\n)?\\\\centering\\n",
    "", x, perl = TRUE
  )
  x <- sub("\n\\end{table}", "", x, fixed = TRUE)
  x
}

# Faz a nota de rodapé (linhas \multicolumn{N}{l}{\rule{0pt}{1em}...}) quebrar
# dentro da largura da tabela: troca o alinhamento {l} por p{} e aplica
# \raggedright, permitindo que o texto da legenda ocupe várias linhas.
# Aceita tabelas com 3 ou 4 colunas (triagens e modelos).
tex_quebra_nota <- function(x) {
  x <- gsub("\\multicolumn{3}{l}{\\rule{0pt}{1em}",
            "\\multicolumn{3}{p{0.82\\linewidth}}{\\rule{0pt}{1em}\\raggedright{}",
            x, fixed = TRUE)
  x <- gsub("\\multicolumn{4}{l}{\\rule{0pt}{1em}",
            "\\multicolumn{4}{p{0.82\\linewidth}}{\\rule{0pt}{1em}\\raggedright{}",
            x, fixed = TRUE)
  x
}

# Envolve a tabela em um grupo com \tabcolsep reduzido, de modo que as
# larguras de coluna (proporcionais à página) somadas ao espaçamento entre
# colunas não ultrapassem a largura de texto (evita overfull hbox).
tex_tabcolsep <- function(x, sep = "1.5pt") {
  paste0("\\begingroup\\setlength{\\tabcolsep}{", sep, "}", x, "\\endgroup")
}

# Português para o cabeçalho de continuação das longtables
tex_continuacao <- function(x) {
  gsub("(continued)", "(continuação)", x, fixed = TRUE)
}

# Escapa caracteres especiais para LaTeX (aplicado às strings das tabelas)
tex_safe <- function(x) {
  x <- gsub("≥", "$\\geq$", x, fixed = TRUE)
  x <- gsub("≤", "$\\leq$", x, fixed = TRUE)
  x <- gsub("×", "$\\times$", x, fixed = TRUE)
  x <- gsub("&", "\\&", x, fixed = TRUE)
  x <- gsub("%", "\\%", x, fixed = TRUE)
  x <- gsub("_", "\\_", x, fixed = TRUE)
  x <- gsub("<", "\\textless{}", x, fixed = TRUE)
  x <- gsub(">", "\\textgreater{}", x, fixed = TRUE)
  x
}

# ---------------------------------------------------------------------------- #
# ==== 1. LEITURA E LIMPEZA DOS DADOS ==========================================
# ---------------------------------------------------------------------------- #

base_raw <- read_excel("base.xlsx", sheet = "Planilha de Dados") %>%
  filter(rowSums(!is.na(across(everything()))) > 0) # remove linhas totalmente vazias

# Renomeia colunas para nomes curtos e sem caracteres especiais
base <- base_raw %>%
  rename(
    Idade_Elast      = `Idade Elastografia`,
    Atividade_Fisica = Ativifis,
    Tabagismo        = Tabag,
    Obesidade_IMC    = `Obesidade (IMC >= 30)`,
    Sobrepeso        = Sobrep,
    Diabetes         = DM,
    Hipertensao      = HAS,
    DHEM             = MASLD,
    Esteatose_FLI    = `Esteato hepatite pelo FLI`,
    Esteatose_Saadeh = `Esteatose (Saadeh)`,
    Esteatose_CAP    = `Esteatose pelo CAP (>236dB/m)`,
    Fibrose          = `Fibrose (Kpa≥8, SW >8, point >1,2)`,
    DHEM_Comb        = `1.Sem_MASLD 2.MASLD_Sem_Fibrose 3.MASLD_e_Fibrose`,
    Gordura_Total_Ind = `%Massa Gorda Total (W:>40/M>30)`,
    IMMA_Baixo       = `Baixo IMMA (W:<15/M:<20)`,
    IMMA_Newman      = `IMMAReduzido_Newman (W:- 1,45/M:-2,06)`,
    SARC_Newman      = `SARC_Newman (IMMA + HG)`,
    IMMA_Baumgartner = `IMMAReduzido_Baumg. (W: 5,45/M:7,26)`,
    SARC_Baumgartner = `SARC_Baumg. (IMMA + HG)`,
    IMMA_FNIH        = `IMMAReduzido_FNIH (W:<0,512/M:<0,789)`,
    SARC_FNIH        = `SARC_FNIH (IMMA + HG)`,
    IMMA_PESO_Ind    = `IMMAReduzido_PESO (W:<19,4/M:<25,7)`,
    IMC              = `BMI Body Mass Index (IMC)`,
    SARC_OS          = `SARC Obesidade Sarcopenica`,
    Sarcopenia_SARCF = `Sarcopenia_SARC-F (≥4)`,
    Sarcopenia_SARCF_CC = `Sarcopenia SARC-F_CC (≥11)`,
    Sarcopenia_Handgrip = `Sarcopenia_handgrip (<16, <27)`,
    Sarcopenia_Elev  = `Sarcopenia_Elevação (>15?)`,
    Sarcopenia_Vel   = `Sarcopenia_teste vel. (<0,8?)`,
    Gordura_Total    = `%GC (%Gord.Total)`,
    Massa_Magra_Total = `Massa Magra Total`,
    ALM_IMMA         = `(ALM) IMMA`,
    IMMA_Ajustado    = `IMMA AJUSTADO`,
    IMMA_PESO        = `IMMA/Peso`
  )

# Auxiliar para fatores binários 0/1
fator01 <- function(x, rotulos = c("Não", "Sim")) {
  factor(x, levels = c(0, 1), labels = rotulos)
}

base <- base %>%
  mutate(
    Sexo = fator01(Sexo, c("Feminino", "Masculino")),
    Etnia = factor(
      Etnia,
      levels = c(1, 2, 3, 4),
      labels = c("Branca", "Preta", "Parda", "Outras")
    ),
    # Códigos 0 e 1 correspondem a sedentarismo (0 não consta na legenda;
    # assumimos que representa o mesmo comportamento do código 1)
    Atividade_Fisica = factor(
      Atividade_Fisica,
      levels = c(0, 1, 2, 3),
      labels = c("Sedentário", "Sedentário", ">150 min/sem", "<150 min/sem")
    ),
    Tabagismo = factor(
      Tabagismo,
      levels = c(0, 1, 2),
      labels = c("Nunca fumou", "Fumante", "Ex-fumante")
    ),
    Obesidade_IMC    = fator01(Obesidade_IMC),
    Sobrepeso        = fator01(Sobrepeso),
    Diabetes         = fator01(Diabetes),
    Hipertensao      = fator01(Hipertensao),
    DHEM             = fator01(DHEM),
    # Código 2 = "não aplicável" na legenda; tratado como ausente
    Esteatose_FLI    = fator01(case_when(Esteatose_FLI == "0" ~ 0,
                                         Esteatose_FLI == "1" ~ 1,
                                         TRUE ~ NA_real_)),
    Esteatose_Saadeh = fator01(case_when(Esteatose_Saadeh == "0" ~ 0,
                                         Esteatose_Saadeh == "1" ~ 1,
                                         TRUE ~ NA_real_)),
    Esteatose_CAP    = fator01(Esteatose_CAP),
    Fibrose          = fator01(case_when(Fibrose == "0" ~ 0,
                                         Fibrose == "1" ~ 1,
                                         TRUE ~ NA_real_)),
    DHEM_Comb = factor(
      DHEM_Comb,
      levels = c(1, 2, 3),
      labels = c("Sem DHEM", "DHEM sem Fibrose", "DHEM com Fibrose")
    ),
    Gordura_Total_Ind = fator01(Gordura_Total_Ind, c("Normal", "Alta")),
    IMMA_Baixo       = fator01(IMMA_Baixo, c("Normal", "Reduzido")),
    IMMA_Newman      = fator01(IMMA_Newman, c("Normal", "Reduzido")),
    SARC_Newman      = fator01(SARC_Newman),
    IMMA_Baumgartner = fator01(IMMA_Baumgartner, c("Normal", "Reduzido")),
    SARC_Baumgartner = fator01(SARC_Baumgartner),
    IMMA_FNIH        = fator01(IMMA_FNIH, c("Normal", "Reduzido")),
    SARC_FNIH        = fator01(SARC_FNIH),
    IMMA_PESO_Ind    = fator01(IMMA_PESO_Ind, c("Normal", "Reduzido")),
    SARC_OS          = fator01(SARC_OS),
    Sarcopenia_SARCF = fator01(case_when(Sarcopenia_SARCF == "0" ~ 0,
                                         Sarcopenia_SARCF == "1" ~ 1,
                                         TRUE ~ NA_real_),
                               c("Normal", "Alterado")),
    Sarcopenia_SARCF_CC = fator01(Sarcopenia_SARCF_CC, c("Normal", "Alterado")),
    Sarcopenia_Handgrip = fator01(Sarcopenia_Handgrip, c("Normal", "Alterado")),
    Sarcopenia_Elev  = fator01(Sarcopenia_Elev, c("Normal", "Alterado")),
    Sarcopenia_Vel   = fator01(Sarcopenia_Vel, c("Normal", "Alterado"))
  )

# Remove a variável "Obesidade Sarcopênica" (OS) de todo o fluxo (Fase 1.1).
# A coluna permanece no base com o nome original, pois foi removida do rename.
base <- base %>% select(-`Obesidade Sarcopênica`)

# Listas de variáveis
numericas <- c(
  "Idade_Elast", "Gordura_Total", "Massa_Magra_Total", "ALM_IMMA",
  "IMMA_Ajustado", "Newman", "Baumgartner", "FNIH", "IMMA_PESO", "IMC"
)
categoricas <- setdiff(colnames(base), numericas)

# Rótulos de exibição (tabelas e gráficos)
rotulos_var <- c(
  Idade_Elast       = "Idade (anos)",
  Gordura_Total     = "Gordura corporal (%)",
  Massa_Magra_Total = "Massa magra total (kg)",
  ALM_IMMA          = "ALM/MMEA (kg)",
  IMMA_Ajustado     = "MMEA ajustado",
  Newman            = "Resíduo de Newman",
  Baumgartner       = "MMEA/altura² (kg/m²)",
  FNIH              = "MMEA/IMC",
  IMMA_PESO         = "MMEA/peso",
  IMC               = "IMC (kg/m²)"
)

rotulos_cat_grupo <- c(
  Sexo                = "Sexo",
  Etnia               = "Etnia",
  Atividade_Fisica    = "Atividade física",
  Tabagismo           = "Tabagismo",
  Obesidade_IMC       = "Obesidade (IMC ≥ 30 kg/m²)",
  Sobrepeso           = "Sobrepeso",
  Diabetes            = "Diabetes",
  Hipertensao         = "Hipertensão arterial",
  DHEM                = "DHEM",
  Esteatose_FLI       = "Esteatose pelo FLI",
  Esteatose_Saadeh    = "Esteatose (Saadeh)",
  Esteatose_CAP       = "Esteatose pelo CAP",
  Fibrose             = "Fibrose hepática significativa",
  DHEM_Comb           = "Grupo DHEM/fibrose",
  Gordura_Total_Ind   = "Gordura corporal",
  IMMA_Baixo          = "Baixa massa magra (MMEA < 15/20 kg)",
  IMMA_Newman         = "MMEA reduzida (Newman)",
  SARC_Newman         = "Sarcopenia (Newman + handgrip)",
  IMMA_Baumgartner    = "MMEA reduzida (Baumgartner)",
  SARC_Baumgartner    = "Sarcopenia (Baumgartner + handgrip)",
  IMMA_FNIH           = "MMEA reduzida (FNIH)",
  SARC_FNIH           = "Sarcopenia (FNIH + handgrip)",
  IMMA_PESO_Ind       = "MMEA reduzida (peso)",
  SARC_OS             = "Sarcopenia (obesidade sarcopênica)",
  Sarcopenia_SARCF    = "Sarcopenia (SARC-F ≥ 4)",
  Sarcopenia_SARCF_CC = "Sarcopenia (SARC-F + panturrilha ≥ 11)",
  Sarcopenia_Handgrip = "Sarcopenia (handgrip < 16/27 kg)",
  Sarcopenia_Elev     = "Sarcopenia (elevação da cadeira > 15 s)",
  Sarcopenia_Vel      = "Sarcopenia (velocidade de marcha < 0,8 m/s)"
)

# Rótulos exclusivos da ANÁLISE DESCRITIVA (sem "reduzida"), conforme
# arquitetura do plano: na descritiva remove-se "reduzida", mas nos
# objetivos mantém-se o rótulo completo de `rotulos_cat_grupo`.
rotulos_cat_descr <- c(
  Sexo                = "Sexo",
  Etnia               = "Etnia",
  Atividade_Fisica    = "Atividade física",
  Tabagismo           = "Tabagismo",
  Obesidade_IMC       = "Obesidade (IMC ≥ 30 kg/m²)",
  Sobrepeso           = "Sobrepeso",
  Diabetes            = "Diabetes",
  Hipertensao         = "Hipertensão arterial",
  DHEM                = "DHEM",
  Esteatose_FLI       = "Esteatose pelo FLI",
  Esteatose_Saadeh    = "Esteatose (Saadeh)",
  Esteatose_CAP       = "Esteatose pelo CAP",
  Fibrose             = "Fibrose hepática significativa",
  DHEM_Comb           = "Grupo DHEM/fibrose",
  Gordura_Total_Ind   = "Gordura corporal",
  IMMA_Baixo          = "Baixa massa magra (MMEA < 15/20 kg)",
  IMMA_Newman         = "MMEA (Newman)",
  SARC_Newman         = "Sarcopenia (Newman + handgrip)",
  IMMA_Baumgartner    = "MMEA (Baumgartner)",
  SARC_Baumgartner    = "Sarcopenia (Baumgartner + handgrip)",
  IMMA_FNIH           = "MMEA (FNIH)",
  SARC_FNIH           = "Sarcopenia (FNIH + handgrip)",
  IMMA_PESO_Ind       = "MMEA (peso)",
  SARC_OS             = "Sarcopenia (obesidade sarcopênica)",
  Sarcopenia_SARCF    = "Sarcopenia (SARC-F ≥ 4)",
  Sarcopenia_SARCF_CC = "Sarcopenia (SARC-F + panturrilha ≥ 11)",
  Sarcopenia_Handgrip = "Sarcopenia (handgrip < 16/27 kg)",
  Sarcopenia_Elev     = "Sarcopenia (elevação da cadeira > 15 s)",
  Sarcopenia_Vel      = "Sarcopenia (velocidade de marcha < 0,8 m/s)"
)

# Rótulos curtos para uso em figuras (evita sobreposição em facetas/eixos)
rotulos_curtos_fig <- c(
  Sarcopenia_SARCF       = "SARC-F",
  Sarcopenia_SARCF_CC    = "SARC-F + panturrilha",
  Sarcopenia_Handgrip    = "Handgrip",
  Sarcopenia_Elev        = "Elevação da cadeira",
  Sarcopenia_Vel         = "Velocidade de marcha",
  SARC_Newman            = "Newman + handgrip",
  SARC_Baumgartner       = "Baumgartner + handgrip",
  SARC_FNIH              = "FNIH + handgrip"
)

# Rótulos curtos para a matriz de correlação
rotulos_cor <- c(
  Idade_Elast       = "Idade",
  Gordura_Total     = "Gordura (%)",
  Massa_Magra_Total = "Massa magra",
  ALM_IMMA          = "ALM (kg)",
  IMMA_Ajustado     = "MMEA ajust.",
  Newman            = "Newman",
  Baumgartner       = "Baumg. (kg/m²)",
  FNIH              = "FNIH",
  IMMA_PESO         = "MMEA/peso",
  IMC               = "IMC"
)

# ---------------------------------------------------------------------------- #
# ==== 2. TABELAS DESCRITIVAS ==================================================
# ---------------------------------------------------------------------------- #

# ---- 2.1 Variáveis numéricas -------------------------------------------------

tabela_numericas <- map_dfr(numericas, function(v) {
  x <- base[[v]]
  p <- shapiro_p(x)
  tibble(
    Variável        = tex_safe(rotulos_var[[v]]),
    n               = sum(!is.na(x)),
    `Média (DP)`    = sprintf("%s (%s)", fmt_num(mean(x, na.rm = TRUE), 1),
                              fmt_num(sd(x, na.rm = TRUE), 1)),
    `Mediana (Q1;Q3)` = sprintf("%s (%s;%s)",
                               fmt_num(median(x, na.rm = TRUE), 1),
                               fmt_num(quantile(x, 0.25, na.rm = TRUE, names = FALSE), 1),
                               fmt_num(quantile(x, 0.75, na.rm = TRUE, names = FALSE), 1)),
    `Mín–Máx`       = sprintf("%s–%s", fmt_num(min(x, na.rm = TRUE), 1),
                              fmt_num(max(x, na.rm = TRUE), 1)),
    p_num           = p,
    `valor-p (SW)`  = tex_safe(fmt_p(p))
  )
})

tabela_numericas %>%
  mutate(`valor-p (SW)` = cell_spec(`valor-p (SW)`, format = "latex",
                                    bold = p_num < 0.05, escape = FALSE)) %>%
  select(-p_num) %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    escape    = FALSE,
    linesep   = "",
    col.names = c("Variável", "n", "Média (DP)", "Mediana (Q1;Q3)",
                  "Mín–Máx", "valor-p (SW)")
  ) %>%
  kable_styling(latex_options = c("striped", "hold_position", "scale_down")) %>%
  as.character() %>%
  tex_sem_table_env() %>%
  writeLines(file.path(dir_saida, "tabela_descritiva_numericas.tex"))

# ---- 2.2 Variáveis categóricas ----------------------------------------------

# Frequências por variável, preservando níveis com frequência zero
lista_cat <- map(categoricas, function(v) {
  f <- base[[v]]
  tab <- table(f)
  tibble(
    Categoria           = names(tab),
    n                   = as.integer(tab),
    perc                = n / sum(n) * 100
  ) %>%
    mutate(`Freq. Relativa (%)` = fmt_pct(perc)) %>%
    select(Categoria, n, `Freq. Relativa (%)`)
})
names(lista_cat) <- categoricas

df_cat <- bind_rows(lista_cat, .id = "var")

info_cat <- map_dfr(categoricas, function(v) {
  tibble(var = v,
         n_val = sum(!is.na(base[[v]])),
         n_miss = sum(is.na(base[[v]])))
})

linhas_por_var <- sapply(lista_cat, nrow)

tab_cat_kbl <- df_cat %>%
  select(-var) %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    longtable = TRUE,
    linesep   = "",
    caption   = paste0(
      "Frequências absolutas e relativas das variáveis categóricas. ",
      "Percentuais calculados sobre os dados observados; níveis sem ",
      "ocorrência aparecem com frequência zero.",
      "\\label{tab:categoricas}"
    ),
    col.names = tex_safe(c("Característica", "Freq. Absoluta",
                           "Freq. Relativa (%)")),
    escape    = FALSE
  ) %>%
  kable_styling(latex_options = c("striped", "repeat_header"))

inicio <- 1
for (i in seq_along(categoricas)) {
  v <- categoricas[i]
  qtd <- linhas_por_var[[v]]
  fim <- inicio + qtd - 1
  rotulo <- sprintf(
    "%s (n = %d%s)",
    rotulos_cat_descr[[v]],
    info_cat$n_val[info_cat$var == v],
    ifelse(info_cat$n_miss[info_cat$var == v] > 0,
           sprintf("; %d ausentes", info_cat$n_miss[info_cat$var == v]),
           "")
  )
  tab_cat_kbl <- tab_cat_kbl %>%
    pack_rows(group_label = tex_safe(rotulo), start_row = inicio,
              end_row = fim, indent = TRUE, escape = FALSE)
  inicio <- fim + 1
}

tab_cat_kbl %>%
  as.character() %>%
  tex_continuacao() %>%
  writeLines(file.path(dir_saida, "tabela_descritiva_categoricas.tex"))

# ---- 2.3 Tabela de cruzamento (Table 1) por grupo DHEM/fibrose ---------------

grupos_nomes <- c("Geral", "Sem DHEM", "DHEM sem Fibrose", "DHEM com Fibrose")
n_por_grupo <- c(nrow(base), as.integer(table(base$DHEM_Comb)))

# Categóricas: n (%) por grupo, com percentuais sobre observados
# (o cruzamento da Tabela 3 exclui "Fibrose" e "DHEM_Comb", conforme plano)
vars_cruzamento <- setdiff(categoricas, c("Fibrose", "DHEM_Comb"))

df_cruz_cat <- map_dfr(vars_cruzamento, function(v) {
  map_dfr(grupos_nomes, function(g) {
    dados <- if (g == "Geral") base else filter(base, DHEM_Comb == g)
    f <- dados[[v]]
    f <- f[!is.na(f)]
    tab <- table(f)
    tibble(grupo = g, Categoria = names(tab), n = as.integer(tab))
  }) %>%
    pivot_wider(id_cols = Categoria, names_from = grupo, values_from = n) %>%
    mutate(
      var = v,
      across(-c(var, Categoria), ~ {
        tot <- sum(.x)
        sprintf("%d (%s%%)", .x, fmt_pct(if (tot == 0) 0 else .x / tot * 100))
      })
    )
})

# Numéricas: Média (DP) e Mediana (IQR) por grupo
df_cruz_num <- map_dfr(numericas, function(v) {
  map_dfr(grupos_nomes, function(g) {
    dados <- if (g == "Geral") base else filter(base, DHEM_Comb == g)
    x <- dados[[v]]
    tibble(
      grupo = g,
      Metrica = c("Média (DP)", "Mediana (Q1;Q3)"),
      Valor = c(
        sprintf("%s (%s)", fmt_num(mean(x, na.rm = TRUE), 1),
                fmt_num(sd(x, na.rm = TRUE), 1)),
        sprintf("%s (%s;%s)",
                fmt_num(median(x, na.rm = TRUE), 1),
                fmt_num(quantile(x, 0.25, na.rm = TRUE, names = FALSE), 1),
                fmt_num(quantile(x, 0.75, na.rm = TRUE, names = FALSE), 1))
      )
    )
  }) %>%
    pivot_wider(id_cols = Metrica, names_from = grupo, values_from = Valor) %>%
    mutate(var = v)
})

df_cruz <- bind_rows(df_cruz_cat, df_cruz_num) %>%
  mutate(
    var = factor(var, levels = c(vars_cruzamento, numericas)),
    Característica = coalesce(Categoria, Metrica)
  ) %>%
  select(var, Característica, all_of(grupos_nomes)) %>%
  arrange(var)

linhas_cruz <- table(df_cruz$var)

tab_cruz_kbl <- df_cruz %>%
  select(-var) %>%
  mutate(across(everything(), tex_safe)) %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    longtable = TRUE,
    escape    = FALSE,
    linesep   = "",
    caption   = paste0(
      "Características clínicas, demográficas e de composição corporal ",
      "segundo o grupo DHEM/fibrose. Percentuais calculados sobre os dados ",
      "observados.",
      "\\label{tab:cruzamento}"
    ),
    col.names = c("Característica / Métrica",
                  sprintf("%s (n = %d)", grupos_nomes, n_por_grupo))
  ) %>%
  column_spec(1, width = "4.3cm") %>%
  column_spec(2:5, width = "2.6cm") %>%
  kable_styling(latex_options = c("striped", "repeat_header"), font_size = 9)

# Blocos de seção (linhas originais do data.frame)
blocos_cat <- list(
  "Características sociodemográficas" = c("Sexo", "Etnia", "Atividade_Fisica", "Tabagismo"),
  "Comorbidades" = c("Obesidade_IMC", "Sobrepeso", "Diabetes", "Hipertensao"),
  "Doença hepática" = c("DHEM", "Esteatose_FLI", "Esteatose_Saadeh",
                        "Esteatose_CAP"),
  "Composição corporal e sarcopenia" = c(
    "Gordura_Total_Ind", "IMMA_Baixo", "IMMA_Newman", "SARC_Newman",
    "IMMA_Baumgartner", "SARC_Baumgartner", "IMMA_FNIH", "SARC_FNIH",
    "IMMA_PESO_Ind", "SARC_OS", "Sarcopenia_SARCF",
    "Sarcopenia_SARCF_CC", "Sarcopenia_Handgrip", "Sarcopenia_Elev",
    "Sarcopenia_Vel"
  )
)
blocos_num <- list("Variáveis numéricas" = numericas)

# Posições originais (no data.frame) de cada variável
pos_var <- split(seq_len(nrow(df_cruz)), df_cruz$var)
inicio_original <- sapply(pos_var, min)
fim_original    <- sapply(pos_var, max)

# 1) Blocos de seção (rótulos maiores, aplicados primeiro)
todos_blocos <- c(blocos_cat, blocos_num)
for (rotulo_bloco in names(todos_blocos)) {
  bloco <- todos_blocos[[rotulo_bloco]]
  start <- min(inicio_original[bloco])
  end   <- max(fim_original[bloco])
  tab_cruz_kbl <- tab_cruz_kbl %>%
    pack_rows(group_label = tex_safe(rotulo_bloco), start_row = start,
              end_row = end, bold = TRUE, escape = FALSE)
}

# 2) Indentação por variável (rótulos menores, aninhados nos blocos)
for (v in c(vars_cruzamento, numericas)) {
  qtd <- linhas_cruz[[v]]
  fim <- inicio_original[[v]] + qtd - 1
  rotulo <- if (v %in% names(rotulos_var)) {
    rotulos_var[[v]]
  } else {
    rotulos_cat_descr[[v]]
  }
  tab_cruz_kbl <- tab_cruz_kbl %>%
    pack_rows(group_label = tex_safe(rotulo),
              start_row = inicio_original[[v]], end_row = fim,
              indent = TRUE, escape = FALSE)
}

tab_cruz_kbl %>%
  as.character() %>%
  {
    sub(
      pattern     = "\\begingroup\\fontsize{9}{11}\\selectfont",
      replacement = "\\begingroup\\fontsize{9}{11}\\selectfont\\setlength{\\tabcolsep}{3pt}",
      x           = .,
      fixed       = TRUE
    )
  } %>%
  tex_continuacao() %>%
  writeLines(file.path(dir_saida, "tabela_cruzamento_DHEM.tex"))

# ---------------------------------------------------------------------------- #
# ==== 3. MACROS DINÂMICAS (outputs/macros.tex) ================================
# ---------------------------------------------------------------------------- #

perc_var <- function(v, nivel) {
  x <- base[[v]]
  sum(x == nivel, na.rm = TRUE) / sum(!is.na(x)) * 100
}

macros <- c(
  sprintf("\\newcommand{\\NTotal}{%d}", nrow(base)),
  sprintf("\\newcommand{\\NSemDHEM}{%d}", n_por_grupo[2]),
  sprintf("\\newcommand{\\NDHEMSemFibrose}{%d}", n_por_grupo[3]),
  sprintf("\\newcommand{\\NDHEMComFibrose}{%d}", n_por_grupo[4]),
  sprintf("\\newcommand{\\MediaIdade}{%s}", fmt_num(mean(base$Idade_Elast), 1)),
  sprintf("\\newcommand{\\MedianaIdade}{%s}", fmt_num(median(base$Idade_Elast), 0)),
  sprintf("\\newcommand{\\MediaIMC}{%s}", fmt_num(mean(base$IMC), 1)),
  sprintf("\\newcommand{\\DPIMC}{%s}", fmt_num(sd(base$IMC), 1)),
  sprintf("\\newcommand{\\PercMulheres}{%s\\%%}", fmt_pct(perc_var("Sexo", "Feminino"))),
  sprintf("\\newcommand{\\PercDHEM}{%s\\%%}", fmt_pct(perc_var("DHEM", "Sim"))),
  sprintf("\\newcommand{\\PercFibrose}{%s\\%%}", fmt_pct(perc_var("Fibrose", "Sim"))),
  sprintf("\\newcommand{\\PercObesidade}{%s\\%%}", fmt_pct(perc_var("Obesidade_IMC", "Sim"))),
  sprintf("\\newcommand{\\PercDiabetes}{%s\\%%}", fmt_pct(perc_var("Diabetes", "Sim"))),
  sprintf("\\newcommand{\\PercHipertensao}{%s\\%%}", fmt_pct(perc_var("Hipertensao", "Sim"))),
  sprintf("\\newcommand{\\PercSarcSARCF}{%s\\%%}", fmt_pct(perc_var("Sarcopenia_SARCF", "Alterado"))),
  sprintf("\\newcommand{\\PercSarcSARCFCC}{%s\\%%}", fmt_pct(perc_var("Sarcopenia_SARCF_CC", "Alterado"))),
  sprintf("\\newcommand{\\PercSarcHandgrip}{%s\\%%}", fmt_pct(perc_var("Sarcopenia_Handgrip", "Alterado"))),
  sprintf("\\newcommand{\\PercSarcElev}{%s\\%%}", fmt_pct(perc_var("Sarcopenia_Elev", "Alterado"))),
  sprintf("\\newcommand{\\PercSarcVel}{%s\\%%}", fmt_pct(perc_var("Sarcopenia_Vel", "Alterado"))),
  sprintf("\\newcommand{\\PercIMMABaixo}{%s\\%%}", fmt_pct(perc_var("IMMA_Baixo", "Reduzido"))),
  sprintf("\\newcommand{\\PercIMMANewman}{%s\\%%}", fmt_pct(perc_var("IMMA_Newman", "Reduzido"))),
  sprintf("\\newcommand{\\PercIMMABaumgartner}{%s\\%%}", fmt_pct(perc_var("IMMA_Baumgartner", "Reduzido"))),
  sprintf("\\newcommand{\\PercIMMAFNIH}{%s\\%%}", fmt_pct(perc_var("IMMA_FNIH", "Reduzido"))),
  sprintf("\\newcommand{\\PercIMMAPeso}{%s\\%%}", fmt_pct(perc_var("IMMA_PESO_Ind", "Reduzido"))),
  sprintf("\\newcommand{\\PercSarcNewman}{%s\\%%}", fmt_pct(perc_var("SARC_Newman", "Sim"))),
  sprintf("\\newcommand{\\PercSarcBaumgartner}{%s\\%%}", fmt_pct(perc_var("SARC_Baumgartner", "Sim"))),
  sprintf("\\newcommand{\\PercSarcFNIH}{%s\\%%}", fmt_pct(perc_var("SARC_FNIH", "Sim")))
)

writeLines(macros, file.path(dir_saida, "macros.tex"))

# ---------------------------------------------------------------------------- #
# ==== 4. GRÁFICOS =============================================================
# ---------------------------------------------------------------------------- #

tema <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 9.5, face = "bold", hjust = 0.5,
                              margin = margin(0, 0, 5, 0))
  )

paleta_barras <- c("#95A5A6", "#2C3E50")
paleta_grupos <- c("Sem DHEM" = "#AAB7B8",
                   "DHEM sem Fibrose" = "#5D6D7E",
                   "DHEM com Fibrose" = "#2C3E50")

# ---- 4.1 Univariadas: histograma + boxplot (numéricas) -----------------------

grafico_numerica <- function(v, legenda = TRUE) {
  dados <- base %>% drop_na(!!sym(v))

  p_box <- ggplot(dados, aes(x = !!sym(v))) +
    geom_boxplot(fill = "#BDC3C7", color = "#2C3E50", alpha = 0.7, width = 0.6) +
    tema +
    theme(axis.title = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())

  p_hist <- ggplot(dados, aes(x = !!sym(v))) +
    geom_histogram(fill = "#34495E", color = "white", bins = 15, alpha = 0.9) +
    labs(title = rotulos_var[[v]], x = NULL, y = "Frequência") +
    tema

  p_box / p_hist + plot_layout(heights = c(1, 4))
}

walk(numericas, function(v) {
  ggsave(
    filename = file.path(dir_saida, sprintf("dist_numerica_%s.pdf", v)),
    plot     = grafico_numerica(v),
    device   = device_pdf, width = 14, height = 10, units = "cm"
  )
})

# ---- 4.2 Univariadas: barras com n (%) (categóricas) -------------------------

grafico_categorica <- function(v) {
  dados <- base %>%
    count(!!sym(v), .drop = FALSE) %>%
    drop_na() %>%
    filter(n > 0) %>%
    mutate(
      perc   = n / sum(n),
      rotulo = sprintf("%d (%s%%)", n, fmt_pct(perc * 100))
    )

  # Angula os rótulos do eixo x quando são longos (evita sobreposição)
  ang <- if (any(nchar(as.character(dados[[v]])) > 12)) 20 else 0

  ggplot(dados, aes(x = !!sym(v), y = n)) +
    geom_col(fill = "#34495E", alpha = 0.9, width = 0.65) +
    geom_text(aes(label = rotulo), vjust = -0.5, size = 3, color = "#2C3E50") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
    labs(title = rotulos_cat_descr[[v]], x = NULL, y = "Frequência absoluta") +
    tema +
    theme(axis.text.x = element_text(angle = ang, hjust = if (ang > 0) 1 else 0.5,
                                     size = if (ang > 0) 7.5 else 8.5))
}

walk(categoricas, function(v) {
  ggsave(
    filename = file.path(dir_saida, sprintf("barra_categorica_%s.pdf", v)),
    plot     = grafico_categorica(v),
    device   = device_pdf, width = 14, height = 10, units = "cm"
  )
})

# ---- 4.3 Painéis para o corpo do relatório -----------------------------------

painel_numericas <- wrap_plots(
  map(c("Idade_Elast", "IMC", "Gordura_Total", "Massa_Magra_Total"),
      ~ grafico_numerica(.x, legenda = FALSE)),
  ncol = 2
) & theme(plot.margin = margin(4, 6, 4, 6))
ggsave(file.path(dir_saida, "painel_numericas_principais.pdf"),
       painel_numericas, device = device_pdf, width = 19, height = 15, units = "cm")

painel_categoricas <- wrap_plots(
  map(c("Sexo", "Sarcopenia_Handgrip", "Sarcopenia_SARCF", "DHEM_Comb"),
      grafico_categorica),
  ncol = 2
) & theme(plot.margin = margin(4, 6, 4, 6))
ggsave(file.path(dir_saida, "painel_categoricas_principais.pdf"),
       painel_categoricas, device = device_pdf, width = 19, height = 15, units = "cm")

# ---- 4.4 Bivariadas -----------------------------------------------------------

# Cat x Cat: prevalência de sarcopenia (critérios funcionais) por grupo
criterios_funcionais <- c(
  "Sarcopenia_SARCF", "Sarcopenia_SARCF_CC", "Sarcopenia_Handgrip",
  "Sarcopenia_Elev", "Sarcopenia_Vel"
)

dados_sarc_func <- map_dfr(criterios_funcionais, function(v) {
  base %>%
    filter(!is.na(DHEM_Comb), !is.na(!!sym(v))) %>%
    count(DHEM_Comb, criterio = rotulos_curtos_fig[[v]],
          alterado = !!sym(v) == "Alterado") %>%
    group_by(DHEM_Comb, criterio) %>%
    mutate(perc = n / sum(n) * 100) %>%
    filter(alterado) %>%
    ungroup()
})

p_sarc_func <- ggplot(dados_sarc_func,
                      aes(x = DHEM_Comb, y = perc, fill = DHEM_Comb)) +
  geom_col(width = 0.68, alpha = 0.9) +
  geom_text(aes(label = sprintf("%s%%", fmt_pct(perc))),
            vjust = -0.4, size = 2.7, color = "#2C3E50") +
  facet_wrap(~ criterio, ncol = 3) +
  scale_fill_manual(values = paleta_grupos) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
  labs(x = NULL, y = "Prevalência de sarcopenia (%)", fill = NULL) +
  tema +
  theme(
    axis.text.x = element_text(angle = 15, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    strip.text = element_text(size = 8, margin = margin(2, 2, 2, 2)),
    legend.position = "bottom",
    plot.margin = margin(6, 8, 6, 8)
  )

ggsave(file.path(dir_saida, "cruzamento_sarcopenia_funcional_DHEMComb.pdf"),
       p_sarc_func, device = device_pdf, width = 18, height = 11, units = "cm")

# Cat x Cat: critérios de massa magra (Newman, Baumgartner, FNIH)
criterios_massa <- c("SARC_Newman", "SARC_Baumgartner", "SARC_FNIH")

dados_sarc_massa <- map_dfr(criterios_massa, function(v) {
  base %>%
    filter(!is.na(DHEM_Comb), !is.na(!!sym(v))) %>%
    count(DHEM_Comb, criterio = rotulos_curtos_fig[[v]],
          sarc = !!sym(v) == "Sim") %>%
    group_by(DHEM_Comb, criterio) %>%
    mutate(perc = n / sum(n) * 100) %>%
    filter(sarc) %>%
    ungroup()
})

p_sarc_massa <- ggplot(dados_sarc_massa,
                       aes(x = DHEM_Comb, y = perc, fill = DHEM_Comb)) +
  geom_col(width = 0.68, alpha = 0.9) +
  geom_text(aes(label = sprintf("%s%%", fmt_pct(perc))),
            vjust = -0.4, size = 2.8, color = "#2C3E50") +
  facet_wrap(~ criterio, ncol = 3) +
  scale_fill_manual(values = paleta_grupos) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
  labs(x = NULL, y = "Prevalência de sarcopenia (%)", fill = NULL) +
  tema +
  theme(
    axis.text.x = element_text(angle = 15, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    strip.text = element_text(size = 8, margin = margin(2, 2, 2, 2)),
    legend.position = "bottom",
    plot.margin = margin(6, 8, 6, 8)
  )

ggsave(file.path(dir_saida, "cruzamento_sarcopenia_massa_DHEMComb.pdf"),
       p_sarc_massa, device = device_pdf, width = 18, height = 9, units = "cm")

# Cat x Cat: DHEM x handgrip (frequências absolutas)
df_cc <- base %>%
  filter(!is.na(DHEM), !is.na(Sarcopenia_Handgrip)) %>%
  count(DHEM, Sarcopenia_Handgrip) %>%
  group_by(DHEM) %>%
  mutate(
    perc   = n / sum(n),
    rotulo = sprintf("%d (%s%%)", n, fmt_pct(perc * 100))
  ) %>%
  ungroup()

p_cc <- ggplot(df_cc, aes(x = DHEM, y = n, fill = Sarcopenia_Handgrip)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.75) +
  geom_text(aes(label = rotulo),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3, color = "#2C3E50") +
  scale_fill_manual(values = paleta_barras) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(x = "DHEM", y = "Frequência absoluta",
       fill = "Handgrip (sarcopenia)") +
  tema

ggsave(file.path(dir_saida, "cruzamento_barras_DHEM_Handgrip.pdf"),
       p_cc, device = device_pdf, width = 14, height = 9, units = "cm")

# Num x Cat: massa magra e IMC por grupo DHEM/fibrose
grafico_box_grupo <- function(v) {
  dados <- base %>% drop_na(DHEM_Comb, !!sym(v))
  ggplot(dados, aes(x = DHEM_Comb, y = !!sym(v), fill = DHEM_Comb)) +
    geom_boxplot(alpha = 0.85, outlier.shape = NA, width = 0.55) +
    geom_jitter(width = 0.14, size = 1.1, alpha = 0.35, color = "#2C3E50") +
    scale_fill_manual(values = paleta_grupos) +
    labs(x = NULL, y = rotulos_var[[v]], fill = NULL) +
    tema +
    theme(axis.text.x = element_text(angle = 15, hjust = 1, size = 7.5),
          legend.position = "none",
          plot.margin = margin(6, 8, 6, 8))
}

ggsave(file.path(dir_saida, "cruzamento_boxplot_MassaMagra_DHEMComb.pdf"),
       grafico_box_grupo("Massa_Magra_Total"), device = device_pdf,
       width = 15, height = 9, units = "cm")
ggsave(file.path(dir_saida, "cruzamento_boxplot_IMC_DHEMComb.pdf"),
       grafico_box_grupo("IMC"), device = device_pdf,
       width = 15, height = 9, units = "cm")

# Painel combinado (massa magra e IMC) para o corpo do relatório
painel_box_grupos <- wrap_plots(
  list(grafico_box_grupo("Massa_Magra_Total"), grafico_box_grupo("IMC")),
  ncol = 2
) & theme(plot.margin = margin(4, 6, 4, 6))
ggsave(file.path(dir_saida, "painel_boxplots_DHEMComb.pdf"),
       painel_box_grupos, device = device_pdf,
       width = 19, height = 9, units = "cm")

# Num x Num: matriz de correlação (heatmap)
mat_cor <- base %>%
  select(all_of(numericas)) %>%
  na.omit() %>%
  cor()

df_cor <- as.data.frame(as.table(mat_cor)) %>%
  rename(var1 = Var1, var2 = Var2, r = Freq)

p_cor <- ggplot(df_cor, aes(var1, var2, fill = r)) +
  geom_tile(color = "white") +
  geom_text(aes(label = fmt_num(r, 2)), size = 2.4, color = "#1B2631") +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                       midpoint = 0, limits = c(-1, 1),
                       name = "Correlação\nde Pearson") +
  scale_x_discrete(labels = rotulos_cor[numericas]) +
  scale_y_discrete(labels = rotulos_cor[numericas]) +
  coord_fixed() +
  labs(x = NULL, y = NULL) +
  tema +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 7),
        axis.text.y = element_text(size = 7),
        legend.title = element_text(size = 8),
        plot.margin = margin(8, 10, 8, 10))

ggsave(file.path(dir_saida, "correlacao_numericas.pdf"),
       p_cor, device = device_pdf, width = 17, height = 14, units = "cm")

# ---------------------------------------------------------------------------- #
# ==== 5. OBJETIVO 1: PREVALÊNCIA DE BAIXA MASSA MAGRA COM IC 95% ==============
# ---------------------------------------------------------------------------- #

criterios_prevalencia <- c(
  "IMMA_Baixo", "IMMA_Newman", "IMMA_Baumgartner", "IMMA_FNIH", "IMMA_PESO_Ind"
)

rotulos_curtos_prevalencia <- c(
  IMMA_Baixo       = "MMEA < 15/20 kg",
  IMMA_Newman      = "Newman",
  IMMA_Baumgartner = "Baumgartner",
  IMMA_FNIH        = "FNIH",
  IMMA_PESO_Ind    = "MMEA/peso"
)

prevalencia <- map_dfr(criterios_prevalencia, function(v) {
  f  <- base[[v]]
  n_obs <- sum(!is.na(f))
  n_ev  <- sum(f == "Reduzido", na.rm = TRUE)
  p     <- n_ev / n_obs
  ic    <- prop.test(n_ev, n_obs, conf.level = 0.95)$conf.int
  tibble(
    var        = v,
    rotulo     = rotulos_curtos_prevalencia[[v]],
    rotulo_tex = tex_safe(rotulos_cat_grupo[[v]]),
    n          = n_obs,
    Eventos    = n_ev,
    pct        = p * 100,
    ic_inf     = ic[1] * 100,
    ic_sup     = ic[2] * 100
  )
}) %>%
  mutate(
    `Prevalência (%)` = fmt_pct(pct),
    `IC 95%`          = sprintf("%s–%s", fmt_pct(ic_inf), fmt_pct(ic_sup))
  )

# Tabela de prevalências com IC
prevalencia %>%
  select(rotulo_tex, n, Eventos, `Prevalência (%)`, `IC 95%`) %>%
  rename(Critério = rotulo_tex) %>%
  kbl(
    format   = "latex",
    booktabs = TRUE,
    escape   = FALSE,
    linesep  = "",
    col.names = tex_safe(c("Critério", "n", "Eventos", "Prevalência (%)",
                           "IC 95%"))
  ) %>%
  kable_styling(latex_options = c("striped", "hold_position", "scale_down")) %>%
  as.character() %>%
  tex_sem_table_env() %>%
  writeLines(file.path(dir_saida, "tabela_prevalencia_IC.tex"))

# Forest plot das prevalências com IC
p_prev <- ggplot(prevalencia, aes(x = reorder(rotulo, pct), y = pct)) +
  geom_pointrange(aes(ymin = ic_inf, ymax = ic_sup),
                  color = "#2C3E50", size = 0.55, linewidth = 0.8) +
  geom_text(
    aes(x = reorder(rotulo, pct), y = ic_sup,
        label = sprintf("%s%% (%s–%s)", fmt_pct(pct), fmt_pct(ic_inf),
                        fmt_pct(ic_sup))),
    hjust = -0.08, size = 3, color = "#2C3E50"
  ) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 100),
                     expand = expansion(mult = c(0.02, 0.12))) +
  labs(x = NULL, y = "Prevalência (%) com IC 95%") +
  tema +
  theme(plot.margin = margin(8, 14, 8, 8))

ggsave(file.path(dir_saida, "figura_prevalencia_IC.pdf"), p_prev,
       device = device_pdf, width = 16, height = 9, units = "cm")

# ---------------------------------------------------------------------------- #
# ==== 6. OBJETIVO 2: CONCORDÂNCIA ENTRE OS AJUSTES DE MMEA ====================
# ---------------------------------------------------------------------------- #

kappa_cohen <- function(x, y) {
  x <- factor(x); y <- factor(y)
  niveis <- union(levels(x), levels(y))
  x <- factor(x, levels = niveis); y <- factor(y, levels = niveis)
  tab <- table(x, y)
  po <- sum(diag(tab)) / sum(tab)
  pe <- sum(rowSums(tab) * colSums(tab)) / sum(tab)^2
  (po - pe) / (1 - pe)
}

kappa_fleiss <- function(mat) {
  n <- nrow(mat); r <- ncol(mat)
  nij <- cbind(rowSums(mat == 0), rowSums(mat == 1))
  P_i <- (rowSums(nij^2) - r) / (r * (r - 1))
  Pbar <- mean(P_i)
  pj <- colSums(nij) / (n * r)
  Pe <- sum(pj^2)
  (Pbar - Pe) / (1 - Pe)
}

ajustes_cont  <- c("Newman", "Baumgartner", "FNIH", "IMMA_PESO")
ajustes_dic   <- c("IMMA_Newman", "IMMA_Baumgartner", "IMMA_FNIH", "IMMA_PESO_Ind")
nomes_ajustes <- c(Newman = "Newman", Baumgartner = "Baumgartner",
                   FNIH = "FNIH", IMMA_PESO = "MMEA/peso")
pares_ajustes <- combn(4, 2)

conc_detalhes <- map(seq_len(ncol(pares_ajustes)), function(j) {
  i1 <- pares_ajustes[1, j]; i2 <- pares_ajustes[2, j]
  x <- base[[ajustes_cont[i1]]]; y <- base[[ajustes_cont[i2]]]
  ok <- complete.cases(x, y)
  x <- x[ok]; y <- y[ok]; n <- length(x)
  d <- y - x

  # 1) teste de médias pareado (conforme normalidade da diferença)
  p_sh <- shapiro_p(d)
  if (!is.na(p_sh) && p_sh >= 0.05) {
    tt <- t.test(y, x, paired = TRUE)
    rot_teste <- "t pareado"; est_med <- tt$statistic; p_med <- tt$p.value
  } else {
    wt <- wilcox.test(y, x, paired = TRUE)
    rot_teste <- "Wilcoxon"; est_med <- wt$statistic; p_med <- wt$p.value
  }

  # 2) correlação > 0
  if (shapiro_p(x) >= 0.05 && shapiro_p(y) >= 0.05) {
    ct <- cor.test(x, y, method = "pearson", alternative = "greater")
    rot_cor <- "Pearson"; r_cor <- ct$estimate; p_cor <- ct$p.value
  } else {
    ct <- cor.test(x, y, method = "spearman", alternative = "greater",
                   exact = FALSE)
    rot_cor <- "Spearman"; r_cor <- ct$estimate; p_cor <- ct$p.value
  }

  # 3) regressão sem intercepto Y = bX, com teste de b = 1
  m <- lm(y ~ 0 + x)
  b <- coef(m)[[1]]; se_b <- summary(m)$coefficients[1, 2]
  t_b <- (b - 1) / se_b
  p_b <- 2 * pt(-abs(t_b), df = n - 1)

  # 4) Bland-Altman
  bias <- mean(d); dp_d <- sd(d)
  lim_inf <- bias - 1.96 * dp_d; lim_sup <- bias + 1.96 * dp_d

  # Kappa de Cohen (versões dicotômicas)
  kc <- kappa_cohen(base[[ajustes_dic[i1]]], base[[ajustes_dic[i2]]])

  par_nome <- sprintf("%s vs %s", nomes_ajustes[i1], nomes_ajustes[i2])

  list(
    conc = tibble(
      Par          = par_nome,
      n            = n,
      `Teste de médias` = sprintf("%s; p %s", rot_teste, fmt_p(p_med)),
      `Correlação`  = sprintf("%s; r = %s (p %s)", rot_cor,
                              fmt_num(r_cor, 2), fmt_p(p_cor)),
      `b (EP)`      = sprintf("%s (%s)", fmt_num(b, 3), fmt_num(se_b, 3)),
      `p (b = 1)`   = fmt_p(p_b),
      `Kappa (Cohen)` = fmt_num(kc, 2),
      `Limites BA`  = sprintf("%s a %s", fmt_num(lim_inf, 2),
                              fmt_num(lim_sup, 2))
    ),
    ba = tibble(par = par_nome, media = (x + y) / 2, dif = d,
                bias = bias, lim_inf = lim_inf, lim_sup = lim_sup),
    sc = tibble(par = par_nome, x = x, y = y, b = b)
  )
})

resultados_conc <- bind_rows(map(conc_detalhes, "conc"))
dados_ba       <- bind_rows(map(conc_detalhes, "ba"))
dados_sc       <- bind_rows(map(conc_detalhes, "sc"))

resultados_conc %>%
  kbl(
    format   = "latex",
    booktabs = TRUE,
    escape   = FALSE,
    linesep  = ""
  ) %>%
  kable_styling(latex_options = c("striped", "hold_position", "scale_down")) %>%
  as.character() %>%
  tex_sem_table_env() %>%
  writeLines(file.path(dir_saida, "tabela_concordancia_ajustes.tex"))

# Kappa de Fleiss (concordância global entre os quatro critérios)
mat_fleiss <- base %>%
  select(all_of(ajustes_dic)) %>%
  mutate(across(everything(), ~ as.integer(.x == "Reduzido"))) %>%
  as.matrix()
kappa_fleiss_geral <- kappa_fleiss(mat_fleiss)

# Validação na subamostra com DHEM (recorte solicitado no objetivo 2)
base_dhem_concord <- base %>% filter(DHEM == "Sim")
mat_fleiss_dhem <- base_dhem_concord %>%
  select(all_of(ajustes_dic)) %>%
  mutate(across(everything(), ~ as.integer(.x == "Reduzido"))) %>%
  as.matrix()
kappa_fleiss_dhem <- kappa_fleiss(mat_fleiss_dhem)
n_dhem_concord <- nrow(base_dhem_concord)

cat(sprintf(
  "  [Concordância] subamostra com DHEM (n = %d): Kappa de Fleiss global = %s\n",
  n_dhem_concord, fmt_num(kappa_fleiss_dhem, 2)
))

# Bland-Altman (painel com os 6 pares)
p_ba <- ggplot(dados_ba, aes(media, dif)) +
  geom_hline(aes(yintercept = bias), color = "#C0392B",
             linetype = "dashed", linewidth = 0.6) +
  geom_hline(aes(yintercept = lim_inf), color = "#2980B9",
             linetype = "dotted", linewidth = 0.5) +
  geom_hline(aes(yintercept = lim_sup), color = "#2980B9",
             linetype = "dotted", linewidth = 0.5) +
  geom_point(alpha = 0.5, size = 1.4, color = "#2C3E50") +
  facet_wrap(~ par, ncol = 2, scales = "free") +
  labs(x = "Média dos dois métodos", y = "Diferença (B − A)") +
  tema +
  theme(strip.text = element_text(size = 8, margin = margin(2, 2, 2, 2)),
        plot.margin = margin(6, 8, 6, 8))

ggsave(file.path(dir_saida, "figura_bland_altman_ajustes.pdf"), p_ba,
       device = device_pdf, width = 19, height = 14, units = "cm")

# Dispersão com reta de identidade e reta ajustada Y = bX
retas_sc <- distinct(dados_sc, par, b)

# Limites por faceta que garantem a visibilidade das retas (identidade e
# ajustada) dentro da área de plotagem, sem distorcer os dados dos pontos.
limites_sc <- dados_sc %>%
  group_by(par) %>%
  summarise(
    xmid = (min(x) + max(x)) / 2,
    ylim_inf = min(c(y, b * min(x), b * max(x), min(x), max(x))),
    ylim_sup = max(c(y, b * min(x), b * max(x), min(x), max(x))),
    .groups = "drop"
  )

p_sc <- ggplot(dados_sc, aes(x, y)) +
  geom_blank(data = limites_sc, aes(x = xmid, y = ylim_inf)) +
  geom_blank(data = limites_sc, aes(x = xmid, y = ylim_sup)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              color = "#7F8C8D", linewidth = 0.6) +
  geom_abline(data = retas_sc, aes(slope = b, intercept = 0),
              color = "#C0392B", linewidth = 0.7) +
  geom_point(alpha = 0.5, size = 1.4, color = "#2C3E50") +
  facet_wrap(~ par, ncol = 2, scales = "free") +
  labs(x = "Método A", y = "Método B") +
  tema +
  theme(strip.text = element_text(size = 8, margin = margin(2, 2, 2, 2)),
        plot.margin = margin(6, 8, 6, 8))

ggsave(file.path(dir_saida, "figura_concordancia_scatter.pdf"), p_sc,
       device = device_pdf, width = 19, height = 14, units = "cm")

# ---------------------------------------------------------------------------- #
# ==== 6.1 DIAGNÓSTICOS DOS MODELOS LINEARES SEM INTERCEPTO (Y = bX) ===========
# ---------------------------------------------------------------------------- #

# Avaliação qualitativa do ajuste com base nos testes de hipótese do
# intercepto (modelo irrestrito Y = a + bX) e de normalidade dos resíduos (m0).
avaliacao_ajuste <- function(p_int, p_sw) {
  falhas <- character(0)
  if (!is.na(p_int) && p_int < 0.05)
    falhas <- c(falhas, "intercepto $\\neq$ 0")
  if (!is.na(p_sw) && p_sw < 0.05)
    falhas <- c(falhas, "resíduos não normais")
  if (length(falhas) == 0) "Adequado"
  else sprintf("Inadequado (%s)", paste(falhas, collapse = "; "))
}

diag_conc <- map(seq_len(ncol(pares_ajustes)), function(j) {
  i1 <- pares_ajustes[1, j]; i2 <- pares_ajustes[2, j]
  x <- base[[ajustes_cont[i1]]]; y <- base[[ajustes_cont[i2]]]
  ok <- complete.cases(x, y)
  x <- x[ok]; y <- y[ok]

  # Modelo sem intercepto Y = bX e modelo irrestrito Y = a + bX
  m0 <- lm(y ~ 0 + x)
  m1 <- lm(y ~ x)

  p_int    <- summary(m1)$coefficients["(Intercept)", 4]  # H0: a = 0
  p_sw     <- shapiro.test(resid(m0))$p.value               # normalidade resíduos
  max_cook <- max(cooks.distance(m0))
  par_nome <- sprintf("%s vs %s", nomes_ajustes[i1], nomes_ajustes[i2])

  cat(sprintf(
    "  [Concordância] %s: R² não centrado = %s, RMSE = %s, SW p = %s, ",
    par_nome, fmt_num(summary(m0)$r.squared, 3),
    fmt_num(sqrt(mean(resid(m0)^2)), 3), fmt_p(p_sw)))
  cat(sprintf("intercepto p = %s, max Cook = %s\n",
              fmt_p(p_int), fmt_num(max_cook, 3)))

  list(
    tab = tibble(
      Par       = par_nome,
      b_ep      = sprintf("%s (%s)", fmt_num(coef(m0)[[1]], 3),
                          fmt_num(summary(m0)$coefficients[1, 2], 3)),
      R2        = fmt_num(summary(m0)$r.squared, 3),
      RMSE      = fmt_num(sqrt(mean(resid(m0)^2)), 3),
      sw_p      = fmt_p(p_sw),
      int_p     = fmt_p(p_int),
      avaliacao = avaliacao_ajuste(p_int, p_sw),
      max_cook  = fmt_num(max_cook, 3)
    ),
    res = tibble(par = par_nome, ajustado = fitted(m0), residuo = resid(m0))
  )
})

dados_diag_conc <- bind_rows(map(diag_conc, "tab"))
dados_res_conc  <- bind_rows(map(diag_conc, "res"))

# Tabela consolidada dos diagnósticos (Apêndice do relatório)
dados_diag_conc %>%
  select(-max_cook) %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    escape    = FALSE,
    linesep   = "",
    align     = "lcccccc",
    col.names = c("Par", "b (EP)", "R$^{2}$", "RMSE",
                  "valor-p (SW Resíduos)",
                  "valor-p (Intercepto $\\neq$ 0)",
                  "Avaliação do Ajuste")
  ) %>%
  column_spec(1, width = "3.2cm") %>%
  column_spec(2, width = "1.9cm") %>%
  column_spec(3, width = "1.5cm") %>%
  column_spec(4, width = "1.5cm") %>%
  column_spec(5, width = "2.7cm") %>%
  column_spec(6, width = "3.1cm") %>%
  column_spec(7, width = "4.0cm") %>%
  kable_styling(latex_options = c("striped", "hold_position", "scale_down")) %>%
  as.character() %>%
  tex_sem_table_env() %>%
  writeLines(file.path(dir_saida, "tabela_diagnosticos_concordancia.tex"))

# Painel de resíduos vs. valores ajustados dos 6 modelos sem intercepto
p_res_conc <- ggplot(dados_res_conc, aes(ajustado, residuo)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#7F8C8D",
             linewidth = 0.5) +
  geom_smooth(method = "loess", se = FALSE, span = 0.8,
              color = "#C0392B", linewidth = 0.6) +
  geom_point(alpha = 0.5, size = 1.4, color = "#2C3E50") +
  facet_wrap(~ par, ncol = 2, scales = "free") +
  labs(x = "Valor ajustado (bX)", y = "Resíduo") +
  tema +
  theme(strip.text = element_text(size = 8, margin = margin(2, 2, 2, 2)),
        plot.margin = margin(6, 8, 6, 8))

ggsave(file.path(dir_saida, "figura_residuos_concordancia.pdf"), p_res_conc,
       device = device_pdf, width = 19, height = 14, units = "cm")

# ---------------------------------------------------------------------------- #
# ==== 7. OBJETIVOS 3 E 4: REGRESSÕES LOGÍSTICAS ===============================
# ---------------------------------------------------------------------------- #

candidatas_modelo <- c(
  "Sexo", "Etnia", "Idade_Elast", "Atividade_Fisica", "Tabagismo",
  "Obesidade_IMC", "Sobrepeso", "Diabetes", "Hipertensao", "IMC",
  "Gordura_Total", "Gordura_Total_Ind", "Massa_Magra_Total", "ALM_IMMA",
  "IMMA_Baixo", "IMMA_Newman", "SARC_Newman", "IMMA_Baumgartner",
  "SARC_Baumgartner", "IMMA_FNIH", "SARC_FNIH", "IMMA_PESO_Ind",
  "Sarcopenia_SARCF", "Sarcopenia_SARCF_CC", "Sarcopenia_Handgrip",
  "Sarcopenia_Elev", "Sarcopenia_Vel"
)

# Teste de associação entre resposta e candidata (numérica ou categórica)
# Mapeia o teste aplicado para um superescrito de nota de rodapé nas tabelas.
sup_teste <- function(teste) {
  mapa <- c("t de Student" = "$^{1}$", "Mann-Whitney" = "$^{2}$",
            "Qui-quadrado" = "$^{3}$", "Fisher (exato)" = "$^{4}$",
            "ANOVA" = "$^{5}$", "Kruskal-Wallis" = "$^{6}$")
  v <- unname(mapa[as.character(teste)])
  v[is.na(v)] <- ""
  v
}

teste_associacao <- function(resposta, v) {
  dados <- base %>% filter(!is.na(.data[[resposta]]), !is.na(.data[[v]]))
  x <- dados[[v]]; y <- dados[[resposta]]
  if (is.numeric(x)) {
    g0 <- x[y == "Não"]; g1 <- x[y == "Sim"]
    if (length(g0) >= 3 && length(g1) >= 3 &&
        shapiro_p(g0) >= 0.05 && shapiro_p(g1) >= 0.05) {
      tt <- t.test(g1, g0)
      tibble(teste = "t de Student", est = tt$statistic, p = tt$p.value)
    } else {
      wt <- wilcox.test(g1, g0)
      tibble(teste = "Mann-Whitney", est = wt$statistic, p = wt$p.value)
    }
  } else {
    tab <- table(y, x)
    esp <- tryCatch(suppressWarnings(chisq.test(tab))$expected,
                    error = function(e) NULL)
    if (is.null(esp) || any(esp < 5)) {
      ft <- fisher.test(tab)
      tibble(teste = "Fisher (exato)", est = NA_real_, p = ft$p.value)
    } else {
      ct <- suppressWarnings(chisq.test(tab))
      tibble(teste = "Qui-quadrado", est = ct$statistic, p = ct$p.value)
    }
  }
}

# Dados da triagem (descritivos por grupo + teste + p) para uma resposta
triagem_dados <- function(resposta) {
  map_dfr(candidatas_modelo, function(v) {
    dados <- base %>% filter(!is.na(.data[[resposta]]), !is.na(.data[[v]]))
    x <- dados[[v]]; y <- dados[[resposta]]
    test <- teste_associacao(resposta, v)
    if (is.numeric(x)) {
      tibble(
        var = v, rotulo = tex_safe(rotulos_var[[v]]), linha = "Média (DP)",
        `Não` = sprintf("%s (%s)", fmt_num(mean(x[y == "Não"]), 1),
                        fmt_num(sd(x[y == "Não"]), 1)),
        `Sim` = sprintf("%s (%s)", fmt_num(mean(x[y == "Sim"]), 1),
                        fmt_num(sd(x[y == "Sim"]), 1)),
        teste = test$teste, est = test$est, p = test$p
      )
    } else {
      map_dfr(levels(x), function(nv) {
        n0 <- sum(y == "Não"); n1 <- sum(y == "Sim")
        f0 <- sum(y == "Não" & x == nv); f1 <- sum(y == "Sim" & x == nv)
        tibble(
          var = v, rotulo = tex_safe(rotulos_cat_grupo[[v]]), linha = nv,
          `Não` = tex_safe(sprintf("%d (%s%%)", f0, fmt_pct(ifelse(n0 == 0, 0, f0 / n0 * 100)))),
          `Sim` = tex_safe(sprintf("%d (%s%%)", f1, fmt_pct(ifelse(n1 == 0, 0, f1 / n1 * 100)))),
          teste = test$teste, est = test$est, p = test$p
        )
      })
    }
  })
}

triagem_tabela <- function(resposta, dados_tri, n0, n1, caption, label) {
  df <- dados_tri %>%
    select(var, rotulo, linha, `Não`, `Sim`, teste, p)

  # Monta o data.frame com 4 colunas (Característica | Não | Sim | p-valor):
  # - variáveis categóricas: linha da variável com o teste (superescrito) e o
  #   p-valor (em negrito com "*" quando p < 0,20); sublinhas das categorias
  #   apenas com as frequências absolutas e relativas n (%);
  # - variáveis numéricas: linha única com média (DP) e p-valor.
  blocos <- list()
  for (v in unique(df$var)) {
    bloco <- df[df$var == v, ]
    p_v   <- bloco$p[1]
    sel   <- !is.na(p_v) && p_v < 0.20
    p_txt <- tex_safe(fmt_p(p_v))
    p_cel <- if (sel) sprintf("\\textbf{%s*}", p_txt) else p_txt
    # rotulo já vem com tex_safe aplicado em triagem_dados
    nome  <- sprintf("\\textbf{%s%s}", bloco$rotulo[1],
                     sup_teste(bloco$teste[1]))
    if (bloco$linha[1] == "Média (DP)") {
      # variável numérica: valores na própria linha
      blocos[[length(blocos) + 1]] <- tibble(
        Característica = nome,
        `Não`          = bloco$`Não`[1],
        `Sim`          = bloco$`Sim`[1],
        `p-valor`      = p_cel
      )
    } else {
      # variável categórica: linha da variável + sublinhas de categoria
      blocos[[length(blocos) + 1]] <- tibble(
        Característica = nome,
        `Não`          = "",
        `Sim`          = "",
        `p-valor`      = p_cel
      )
      sub <- bloco %>%
        mutate(Característica = sprintf("\\hspace{1em}%s", tex_safe(linha)),
               `p-valor`      = "") %>%
        select(Característica, `Não`, `Sim`, `p-valor`)
      blocos[[length(blocos) + 1]] <- sub
    }
  }
  df_montado <- bind_rows(blocos)

  df_montado %>%
    kbl(
      format    = "latex",
      booktabs  = TRUE,
      longtable = TRUE,
      escape    = FALSE,
      linesep   = "",
      align     = c("l", "c", "c", "r"),
      caption   = paste0(tex_safe(caption), " \\label{", label, "}"),
      col.names = c("Característica",
                    sprintf("Não (n = %d)", n0),
                    sprintf("Sim (n = %d)", n1),
                    "p-valor")
    ) %>%
    column_spec(1, width = "6.2cm") %>%
    column_spec(2:3, width = "3.0cm") %>%
    column_spec(4, width = "2.0cm") %>%
    kable_styling(latex_options = c("striped", "hold_position", "repeat_header"),
                  font_size = 9) %>%
    footnote(
      general = "$^{1}$ Teste $t$ de Student; $^{2}$ Teste de Mann--Whitney; $^{3}$ Teste Qui-quadrado; $^{4}$ Teste Exato de Fisher. * $p < 0{,}20$: variável selecionada para a modelagem.",
      general_title = "Nota: ", escape = FALSE
    )
}

# --- Triagem: Fibrose ---
triagem_fibrose <- triagem_dados("Fibrose")
n_fib0 <- sum(base$Fibrose == "Não", na.rm = TRUE)
n_fib1 <- sum(base$Fibrose == "Sim", na.rm = TRUE)

sel_fibrose_20 <- triagem_fibrose %>%
  distinct(var, p) %>%
  filter(p < 0.20) %>%
  pull(var)

triagem_tabela(
  resposta = "Fibrose",
  dados_tri = triagem_fibrose,
  n0 = n_fib0, n1 = n_fib1,
  caption = "Triagem de variáveis candidatas para o modelo de fibrose hepática.",
  label = "tab:triagem_fibrose"
) %>%
  as.character() %>%
  tex_continuacao() %>%
  tex_quebra_nota() %>%
  writeLines(file.path(dir_saida, "tabela_triagem_fibrose.tex"))

# --- Triagem: DHEM ---
triagem_dhem <- triagem_dados("DHEM")
n_dhem0 <- sum(base$DHEM == "Não", na.rm = TRUE)
n_dhem1 <- sum(base$DHEM == "Sim", na.rm = TRUE)

sel_dhem_20 <- triagem_dhem %>%
  distinct(var, p) %>%
  filter(p < 0.20) %>%
  pull(var)

triagem_tabela(
  resposta = "DHEM",
  dados_tri = triagem_dhem,
  n0 = n_dhem0, n1 = n_dhem1,
  caption = "Triagem de variáveis candidatas para o modelo de DHEM.",
  label = "tab:triagem_dhem"
) %>%
  as.character() %>%
  tex_continuacao() %>%
  tex_quebra_nota() %>%
  writeLines(file.path(dir_saida, "tabela_triagem_dhem.tex"))

# --- Ajuste dos modelos logísticos ---
ajustar_modelo_logistico <- function(resposta, selecionadas, tag) {
  dados <- base %>% filter(!is.na(.data[[resposta]]))
  for (v in selecionadas) {
    if (is.factor(dados[[v]])) {
      tab <- table(dados[[v]])
      esparsos <- names(tab)[tab < 5]
      if (length(esparsos) > 0) {
        cat(sprintf("  [%s] níveis esparsos removidos de %s: %s\n",
                    tag, v, paste(esparsos, collapse = ", ")))
        dados <- dados %>% filter(!(.data[[v]] %in% esparsos))
      }
    }
  }
  f <- reformulate(selecionadas, response = resposta)
  # Mantém o nº de linhas constante entre os modelos comparados pelo stepAIC:
  # remove observações com qualquer NA nas variáveis selecionadas.
  dados <- dados %>% drop_na(all_of(selecionadas))
  m0 <- glm(f, data = dados, family = binomial)
  m_final <- stepAIC(m0, direction = "both", trace = 0)
  list(inicial = m0, final = m_final, dados = dados)
}

tabela_modelo_logistico <- function(modelo) {
  tid <- broom::tidy(modelo, exponentiate = TRUE, conf.int = TRUE,
                     conf.method = "wald")
  varnames <- names(modelo$model)[-1]
  varnames <- varnames[order(nchar(varnames), decreasing = TRUE)]

  tid <- tid %>%
    filter(term != "(Intercept)") %>%
    mutate(
      var_base = map_chr(term, function(t) {
        for (v in varnames) {
          if (t == v || startsWith(t, v)) return(v)
        }
        t
      }),
      nivel = map2_chr(term, var_base, function(t, vb) {
        if (t == vb) NA_character_ else substring(t, nchar(vb) + 1)
      }),
      rotulo = map2_chr(term, var_base, function(t, vb) {
        rv <- if (vb %in% names(rotulos_var)) rotulos_var[[vb]] else rotulos_cat_grupo[[vb]]
        if (t == vb) return(rv)
        sprintf("%s: %s", rv, substring(t, nchar(vb) + 1))
      }) %>% tex_safe(),
      rotulo_base = map_chr(var_base, function(vb) {
        if (vb %in% names(rotulos_var)) rotulos_var[[vb]] else rotulos_cat_grupo[[vb]]
      }) %>% tex_safe(),
      OR     = fmt_num(estimate, 2),
      `IC 95%` = sprintf("%s–%s", fmt_num(conf.low, 2), fmt_num(conf.high, 2)),
      `valor-p` = tex_safe(fmt_p(p.value))
    ) %>%
    select(var_base, nivel, rotulo, rotulo_base, OR, `IC 95%`, `valor-p`)

  # Nº de níveis e categoria de referência de cada fator (contrastes de
  # tratamento: 1º nível), para classificar dicotômicas vs. politômicas.
  info <- map_dfr(unique(tid$var_base), function(v) {
    x <- modelo$model[[v]]
    tibble(var_base = v,
           n_niveis = if (is.factor(x)) length(levels(x)) else 0L,
           ref = if (is.factor(x)) tex_safe(levels(x)[1]) else NA_character_)
  })
  tid <- tid %>% left_join(info, by = "var_base")

  # Montagem das linhas:
  # - variáveis contínuas: linha única;
  # - dicotômicas (2 níveis): linha única "Rótulo (Nível vs. Ref.)";
  # - politômicas (≥ 3 níveis): linha principal "Rótulo (Ref.: X)" em negrito
  #   com células vazias e sublinhas indentadas com os níveis.
  linhas <- list()
  for (vb in unique(tid$var_base)) {
    bloco <- tid[tid$var_base == vb, ]
    if (bloco$n_niveis[1] >= 3) {
      linhas[[length(linhas) + 1]] <- tibble(
        rotulo_linha = sprintf("\\textbf{%s (Ref.: %s)}",
                               bloco$rotulo_base[1], bloco$ref[1]),
        OR = "", `IC 95%` = "", `valor-p` = ""
      )
      sub <- bloco %>%
        mutate(rotulo_linha = sprintf("\\hspace{1em}%s", tex_safe(nivel))) %>%
        select(rotulo_linha, OR, `IC 95%`, `valor-p`)
      linhas[[length(linhas) + 1]] <- sub
    } else if (bloco$n_niveis[1] == 2) {
      dic <- bloco %>%
        mutate(rotulo_linha = sprintf("%s (%s vs. %s)",
                                      rotulo_base, tex_safe(nivel), ref)) %>%
        select(rotulo_linha, OR, `IC 95%`, `valor-p`)
      linhas[[length(linhas) + 1]] <- dic
    } else {
      linhas[[length(linhas) + 1]] <- bloco %>%
        select(rotulo_linha = rotulo, OR, `IC 95%`, `valor-p`)
    }
  }
  df_montado <- bind_rows(linhas)

  df_montado %>%
    kbl(
      format   = "latex",
      booktabs = TRUE,
      escape   = FALSE,
      linesep  = "",
      align    = "lccc",
      col.names = tex_safe(c("Variável", "OR", "IC 95%", "p-valor"))
    ) %>%
    column_spec(1, width = "6.5cm") %>%
    column_spec(2:4, width = "3.0cm") %>%
    kable_styling(latex_options = c("striped", "hold_position")) %>%
    as.character() %>%
    tex_sem_table_env()
}

# ============================================================================ #
# Modelos logísticos simples (Fase 3.2) e diagnósticos (Fase 5)               #
# ============================================================================ #

# Roda um glm binomial para CADA covariável selecionada isoladamente.
modelo_simples <- function(resposta, selecionadas, dados) {
  map_dfr(selecionadas, function(v) {
    dados_v <- dados %>% filter(!is.na(.data[[resposta]]), !is.na(.data[[v]]))
    m <- glm(reformulate(v, response = resposta), data = dados_v,
             family = binomial)
    tid <- broom::tidy(m, exponentiate = TRUE, conf.int = TRUE,
                       conf.method = "wald")
    tid$var_base <- v
    tid
  })
}

# Tabela dos modelos simples (estrutura da Tabela 7): Variável | OR | IC 95% | p-valor.
# Destaca em negrito as covariáveis retidas no modelo múltiplo (stepwise/AIC).
# Variáveis contínuas e dicotômicas ocupam linha única; fatores politômicos
# (≥ 3 níveis) ganham linha principal com a categoria de referência e
# sublinhas indentadas. `dados` é o data frame usado no ajuste (para obter
# os níveis e a referência de cada fator).
tabela_modelos_simples <- function(modelos_tidy, retidas, dados) {
  tid <- modelos_tidy %>%
    filter(term != "(Intercept)") %>%
    mutate(
      nivel = map2_chr(term, var_base, function(t, vb) {
        if (t == vb) NA_character_ else substring(t, nchar(vb) + 1)
      }),
      rotulo = map2_chr(term, var_base, function(t, vb) {
        rv <- if (vb %in% names(rotulos_var)) rotulos_var[[vb]] else rotulos_cat_grupo[[vb]]
        if (t == vb) return(rv)
        sprintf("%s: %s", rv, substring(t, nchar(vb) + 1))
      }) %>% tex_safe(),
      rotulo_base = map_chr(var_base, function(vb) {
        if (vb %in% names(rotulos_var)) rotulos_var[[vb]] else rotulos_cat_grupo[[vb]]
      }) %>% tex_safe(),
      OR     = fmt_num(estimate, 2),
      `IC 95%` = sprintf("%s–%s", fmt_num(conf.low, 2), fmt_num(conf.high, 2)),
      valor_p = tex_safe(fmt_p(p.value))
    ) %>%
    mutate(`valor-p` = cell_spec(valor_p, format = "latex",
                                 bold = var_base %in% retidas, escape = FALSE)) %>%
    select(var_base, nivel, rotulo, rotulo_base, OR, `IC 95%`, `valor-p`)

  # Nº de níveis e categoria de referência de cada fator (contrastes de
  # tratamento: 1º nível), para classificar dicotômicas vs. politômicas.
  info <- map_dfr(unique(tid$var_base), function(v) {
    x <- dados[[v]]
    tibble(var_base = v,
           n_niveis = if (is.factor(x)) length(levels(x)) else 0L,
           ref = if (is.factor(x)) tex_safe(levels(x)[1]) else NA_character_)
  })
  tid <- tid %>% left_join(info, by = "var_base")

  # Montagem das linhas (mesma estrutura da tabela do modelo múltiplo):
  # contínuas em linha única; dicotômicas "Rótulo (Nível vs. Ref.)";
  # politômicas com linha principal em negrito e sublinhas indentadas.
  linhas <- list()
  for (vb in unique(tid$var_base)) {
    bloco <- tid[tid$var_base == vb, ]
    if (bloco$n_niveis[1] >= 3) {
      linhas[[length(linhas) + 1]] <- tibble(
        rotulo_linha = sprintf("\\textbf{%s (Ref.: %s)}",
                               bloco$rotulo_base[1], bloco$ref[1]),
        OR = "", `IC 95%` = "", `valor-p` = ""
      )
      sub <- bloco %>%
        mutate(rotulo_linha = sprintf("\\hspace{1em}%s", tex_safe(nivel))) %>%
        select(rotulo_linha, OR, `IC 95%`, `valor-p`)
      linhas[[length(linhas) + 1]] <- sub
    } else if (bloco$n_niveis[1] == 2) {
      dic <- bloco %>%
        mutate(rotulo_linha = sprintf("%s (%s vs. %s)",
                                      rotulo_base, tex_safe(nivel), ref)) %>%
        select(rotulo_linha, OR, `IC 95%`, `valor-p`)
      linhas[[length(linhas) + 1]] <- dic
    } else {
      linhas[[length(linhas) + 1]] <- bloco %>%
        select(rotulo_linha = rotulo, OR, `IC 95%`, `valor-p`)
    }
  }
  df_montado <- bind_rows(linhas)

  df_montado %>%
    kbl(
      format   = "latex",
      booktabs = TRUE,
      escape   = FALSE,
      linesep  = "",
      align    = "lccc",
      col.names = tex_safe(c("Variável", "OR", "IC 95%", "p-valor"))
    ) %>%
    column_spec(1, width = "6.5cm") %>%
    column_spec(2:4, width = "3.0cm") %>%
    kable_styling(latex_options = c("striped", "hold_position")) %>%
    footnote(
      general = "Modelos logísticos simples para cada covariável selecionada na triagem (p < 0,20). Em negrito, as covariáveis retidas no modelo múltiplo por stepwise (critério AIC).",
      general_title = "Nota: ", escape = FALSE
    ) %>%
    as.character() %>%
    tex_sem_table_env()
}

# Diagnósticos completos de um modelo logístico final: HL, resíduos, VIF,
# distância de Cook e ROC/AUC. Retorna a linha-resumo e grava as figuras.
diagnosticos_modelo <- function(modelo, resposta, tag, tag_nome) {
  dados <- modelo$model  # model frame: linhas completas usadas no ajuste
  y <- as.integer(dados[[resposta]] == "Sim")
  yhat <- fitted(modelo)
  n <- length(y)

  # ---- 1) Hosmer-Lemeshow (fallback g = 5 se contagens esperadas < 1) ----
  run_hl <- function(g) {
    tryCatch(
      withCallingHandlers(
        ResourceSelection::hoslem.test(y, yhat, g = g),
        warning = function(w) invokeRestart("muffleWarning")
      ),
      error = function(e) NULL
    )
  }
  hl <- run_hl(10); hl_g <- 10
  if (is.null(hl) || any(hl$expected < 1)) { hl <- run_hl(5); hl_g <- 5 }
  if (is.null(hl)) { hl_chisq <- NA_real_; hl_gl <- NA_integer_; hl_p <- NA_real_ }
  else {
    hl_chisq <- as.numeric(hl$statistic)
    hl_gl    <- as.integer(hl$parameter)
    hl_p     <- hl$p.value
  }

  # ---- 2) Resíduos de Pearson e Deviance ----
  res_pear <- residuals(modelo, type = "pearson")
  res_dev  <- residuals(modelo, type = "deviance")

  # ---- 3) Multicolinearidade (VIF / GVIF) ----
  vif <- tryCatch(car::vif(modelo), error = function(e) NULL)
  vif_plot <- NULL
  if (!is.null(vif)) {
    if (is.matrix(vif) || is.data.frame(vif)) {
      m <- as.data.frame(vif)
      vif_plot <- tibble(term = rownames(m), val = as.numeric(m[[3]]))
    } else {
      vif_plot <- tibble(term = names(vif), val = as.numeric(vif))
    }
    vif_max <- max(vif_plot$val, na.rm = TRUE)
  } else {
    vif_max <- NA_real_
  }

  # ---- 4) Influência (Distância de Cook) ----
  cd <- cooks.distance(modelo)
  cd_thr <- 4 / n
  max_cook <- max(cd, na.rm = TRUE)

  # ---- 5) Discriminação (ROC/AUC) ----
  roc_obj <- pROC::roc(y, yhat, quiet = TRUE)
  auc <- as.numeric(pROC::auc(roc_obj))
  auc_ci <- pROC::ci.auc(roc_obj, method = "delong")
  auc_lo <- auc_ci[1]; auc_hi <- auc_ci[3]

  # ---- Figuras ----
  p_roc <- pROC::ggroc(roc_obj, legacy.axes = TRUE, size = 0.8, color = "#2C3E50") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#7F8C8D") +
    labs(x = "1 − Especificidade", y = "Sensibilidade",
         title = sprintf("Curva ROC — %s (AUC = %s; IC 95%%: %s–%s)",
                         tag_nome, fmt_num(auc, 2), fmt_num(auc_lo, 2), fmt_num(auc_hi, 2))) +
    coord_fixed() + tema
  ggsave(file.path(dir_saida, sprintf("figura_roc_%s.pdf", tag)), p_roc,
         device = device_pdf, width = 14, height = 12, units = "cm")

  brks <- unique(quantile(yhat, probs = seq(0, 1, 0.1), na.rm = TRUE))
  if (length(brks) < 2) brks <- seq(0, 1, 0.1)
  calib <- tibble(pred = yhat, obs = y) %>%
    mutate(dec = cut(pred, breaks = brks, include.lowest = TRUE)) %>%
    filter(!is.na(dec)) %>%
    group_by(dec) %>%
    summarise(pred_m = mean(pred), obs_m = mean(obs), n = n(), .groups = "drop")
  p_cal <- ggplot(calib, aes(pred_m, obs_m)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#7F8C8D") +
    geom_smooth(method = "lm", se = FALSE, linetype = "dotted", color = "#C0392B") +
    geom_point(size = 2.6, color = "#2C3E50") +
    labs(x = "Probabilidade predita (decis)", y = "Proporção observada") +
    coord_fixed() + tema
  ggsave(file.path(dir_saida, sprintf("figura_calibracao_%s.pdf", tag)), p_cal,
         device = device_pdf, width = 14, height = 12, units = "cm")

  res_df <- tibble(obs = seq_along(res_dev), dev = res_dev, pear = res_pear, yhat = yhat)
  p_res <- wrap_plots(
    ggplot(res_df, aes(obs, dev)) +
      geom_point(alpha = 0.5, size = 1.4, color = "#2C3E50") +
      geom_hline(yintercept = c(-2, 2), linetype = "dashed", color = "#C0392B") +
      labs(x = "Índice da observação", y = "Resíduo de deviance") + tema,
    ggplot(res_df, aes(yhat, dev)) +
      geom_point(alpha = 0.5, size = 1.4, color = "#2C3E50") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(x = "Valor ajustado", y = "Resíduo de deviance") + tema,
    ggplot(res_df, aes(sample = dev)) +
      stat_qq(alpha = 0.5, size = 1.4, color = "#2C3E50") +
      stat_qq_line(color = "#C0392B") +
      labs(x = "Quantis teóricos", y = "Resíduo de deviance") + tema,
    ncol = 3
  ) & theme(plot.margin = margin(6, 8, 6, 8))
  ggsave(file.path(dir_saida, sprintf("figura_residuos_%s.pdf", tag)), p_res,
         device = device_pdf, width = 19, height = 7, units = "cm")

  cd_df <- tibble(obs = seq_along(cd), cd = cd)
  p_cook <- ggplot(cd_df, aes(obs, cd)) +
    geom_point(alpha = 0.5, size = 1.4, color = "#2C3E50") +
    geom_hline(yintercept = cd_thr, linetype = "dashed", color = "#C0392B") +
    geom_hline(yintercept = 1, linetype = "dotted", color = "#2980B9") +
    labs(x = "Índice da observação", y = "Distância de Cook") + tema
  ggsave(file.path(dir_saida, sprintf("figura_cook_%s.pdf", tag)), p_cook,
         device = device_pdf, width = 14, height = 9, units = "cm")

  if (!is.null(vif_plot)) {
    p_vif <- ggplot(vif_plot, aes(reorder(term, val), val)) +
      geom_col(fill = "#34495E", alpha = 0.9) +
      geom_hline(yintercept = 5, linetype = "dashed", color = "#C0392B") +
      geom_hline(yintercept = 10, linetype = "dotted", color = "#2980B9") +
      coord_flip() +
      labs(x = NULL, y = "VIF (GVIF^{1/(2·Df)} para fatores)") + tema
    ggsave(file.path(dir_saida, sprintf("figura_vif_%s.pdf", tag)), p_vif,
           device = device_pdf, width = 14, height = 9, units = "cm")
  }

  # ---- Linha-resumo ----
  tibble(
    Modelo         = tag_nome,
    n              = n,
    `HL X²`        = ifelse(is.na(hl_chisq), "—", fmt_num(hl_chisq, 2)),
    `gl (HL)`      = ifelse(is.na(hl_gl), "—", as.character(hl_gl)),
    `valor-p (HL)` = ifelse(is.na(hl_p), "—", tex_safe(fmt_p(hl_p))),
    `HL grupos`    = as.character(hl_g),
    `AUC (IC 95%)` = sprintf("%s (%s–%s)", fmt_num(auc, 2), fmt_num(auc_lo, 2), fmt_num(auc_hi, 2)),
    `VIF máx`      = ifelse(is.na(vif_max), "—", fmt_num(vif_max, 1)),
    `Máx. Cook`    = ifelse(is.na(max_cook), "—", fmt_num(max_cook, 3))
  )
}

# Objetivo 3: resposta = Fibrose (variáveis com p < 0,20 na triagem)
sel_fibrose <- sel_fibrose_20

cat(sprintf("  [Fibrose] selecionadas (p < 0,20): %s\n",
            paste(sel_fibrose, collapse = ", ")))

mod_fibrose <- ajustar_modelo_logistico("Fibrose", sel_fibrose, "Fibrose")

retidas_fibrose <- names(mod_fibrose$final$model)[-1]
cat(sprintf("  [Fibrose] retidas no modelo múltiplo (stepwise): %s\n",
            paste(retidas_fibrose, collapse = ", ")))

tabela_modelo_logistico(
  mod_fibrose$final
) %>%
  tex_tabcolsep() %>%
  writeLines(file.path(dir_saida, "tabela_modelo_fibrose.tex"))

simples_fibrose <- modelo_simples("Fibrose", sel_fibrose_20, mod_fibrose$dados)
tabela_modelos_simples(simples_fibrose, retidas_fibrose, mod_fibrose$dados) %>%
  tex_quebra_nota() %>%
  tex_tabcolsep() %>%
  writeLines(file.path(dir_saida, "tabela_modelo_simples_fibrose.tex"))

diag_fibrose <- diagnosticos_modelo(mod_fibrose$final, "Fibrose", "fibrose", "Fibrose hepática")

# Modelo de regressão linear múltipla para a resposta direta da hipótese do
# objetivo 3: massa magra total ajustada por sexo, idade e IMC, com o indicador
# de fibrose hepática.
mod_linear_fibrose <- lm(Massa_Magra_Total ~ Sexo + Idade_Elast + IMC + Fibrose,
                         data = base)
beta_fibrose_mmt <- coef(mod_linear_fibrose)[["FibroseSim"]]
p_beta_fibrose_mmt <- summary(mod_linear_fibrose)$coefficients["FibroseSim", "Pr(>|t|)"]

cat(sprintf(
  "  [Fibrose] regressão linear ajustada: beta = %s kg (p = %s)\n",
  fmt_num(beta_fibrose_mmt, 2), fmt_p(p_beta_fibrose_mmt)
))

# Objetivo 4: resposta = DHEM (variáveis com p < 0,20 na triagem)
sel_dhem <- sel_dhem_20

cat(sprintf("  [DHEM] selecionadas (p < 0,20): %s\n",
            paste(sel_dhem, collapse = ", ")))

mod_dhem <- ajustar_modelo_logistico("DHEM", sel_dhem, "DHEM")

retidas_dhem <- names(mod_dhem$final$model)[-1]
cat(sprintf("  [DHEM] retidas no modelo múltiplo (stepwise): %s\n",
            paste(retidas_dhem, collapse = ", ")))

tabela_modelo_logistico(
  mod_dhem$final
) %>%
  tex_tabcolsep() %>%
  writeLines(file.path(dir_saida, "tabela_modelo_dhem.tex"))

simples_dhem <- modelo_simples("DHEM", sel_dhem_20, mod_dhem$dados)
tabela_modelos_simples(simples_dhem, retidas_dhem, mod_dhem$dados) %>%
  tex_quebra_nota() %>%
  tex_tabcolsep() %>%
  writeLines(file.path(dir_saida, "tabela_modelo_simples_dhem.tex"))

diag_dhem <- diagnosticos_modelo(mod_dhem$final, "DHEM", "dhem", "DHEM")

# Tabela-resumo consolidada dos diagnósticos dos dois modelos (Apêndice)
bind_rows(diag_fibrose, diag_dhem) %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    escape    = FALSE,
    linesep   = "",
    align     = "lcccccccc",
    col.names = tex_safe(c("Modelo", "n", "HL X²", "gl (HL)", "valor-p (HL)",
                           "Grupos HL", "AUC (IC 95%)", "VIF máx", "Máx. Cook"))
  ) %>%
  column_spec(1, width = "2.8cm") %>%
  column_spec(2:4, width = "1.4cm") %>%
  column_spec(5, width = "1.9cm") %>%
  column_spec(6, width = "1.7cm") %>%
  column_spec(7, width = "2.6cm") %>%
  column_spec(8, width = "1.7cm") %>%
  column_spec(9, width = "1.9cm") %>%
  kable_styling(latex_options = c("striped", "hold_position", "scale_down")) %>%
  as.character() %>%
  tex_sem_table_env() %>%
  writeLines(file.path(dir_saida, "tabela_diagnosticos_modelos.tex"))

# ---------------------------------------------------------------------------- #
# ==== 8. OBJETIVO 5: COMPARAÇÃO DOS TRÊS GRUPOS (DHEM_Comb) ===================
# ---------------------------------------------------------------------------- #

testes_grupos <- map_dfr(candidatas_modelo, function(v) {
  dados <- base %>% filter(!is.na(DHEM_Comb), !is.na(.data[[v]]))
  x <- dados[[v]]; g <- dados$DHEM_Comb
  if (is.numeric(x)) {
    if (shapiro_p(x) >= 0.05) {
      a <- summary(aov(x ~ g))
      tibble(var = v, rotulo = tex_safe(rotulos_var[[v]]),
             Teste = "ANOVA",
             Estatística = fmt_num(a[[1]]$`F value`[1], 2),
             p_num = a[[1]]$`Pr(>F)`[1])
    } else {
      k <- kruskal.test(x ~ g)
      tibble(var = v, rotulo = tex_safe(rotulos_var[[v]]),
             Teste = "Kruskal-Wallis",
             Estatística = fmt_num(k$statistic, 2),
             p_num = k$p.value)
    }
  } else {
    tab <- table(g, x)
    esp <- tryCatch(suppressWarnings(chisq.test(tab))$expected,
                    error = function(e) NULL)
    if (is.null(esp) || any(esp < 5)) {
      ft <- fisher.test(tab)
      tibble(var = v, rotulo = tex_safe(rotulos_cat_grupo[[v]]),
             Teste = "Fisher (exato)", Estatística = "—", p_num = ft$p.value)
    } else {
      ct <- suppressWarnings(chisq.test(tab))
      tibble(var = v, rotulo = tex_safe(rotulos_cat_grupo[[v]]),
             Teste = "Qui-quadrado",
             Estatística = fmt_num(ct$statistic, 2),
             p_num = ct$p.value)
    }
  }
}) %>%
  mutate(
    var = factor(var, levels = candidatas_modelo),
    rotulo = paste0(rotulo, sup_teste(Teste)),
    `valor-p` = cell_spec(tex_safe(fmt_p(p_num)), format = "latex",
                          bold = p_num < 0.05, escape = FALSE)
  ) %>%
  arrange(var)

blocos_grupos <- list(
  "Características sociodemográficas" = c("Sexo", "Etnia", "Idade_Elast", "Atividade_Fisica", "Tabagismo"),
  "Comorbidades" = c("Obesidade_IMC", "Sobrepeso", "Diabetes", "Hipertensao"),
  "Composição corporal e sarcopenia" = c(
    "IMC", "Gordura_Total", "Gordura_Total_Ind", "Massa_Magra_Total", "ALM_IMMA",
    "IMMA_Baixo", "IMMA_Newman", "SARC_Newman", "IMMA_Baumgartner",
    "SARC_Baumgartner", "IMMA_FNIH", "SARC_FNIH", "IMMA_PESO_Ind",
    "Sarcopenia_SARCF", "Sarcopenia_SARCF_CC", "Sarcopenia_Handgrip",
    "Sarcopenia_Elev", "Sarcopenia_Vel"
  )
)

tab_grupos_kbl <- testes_grupos %>%
  select(rotulo, Teste, Estatística, `valor-p`) %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    longtable = TRUE,
    escape    = FALSE,
    linesep   = "",
    caption   = paste0(
      tex_safe(paste0(
        "Testes de hipóteses para comparação dos três grupos de DHEM/fibrose ",
        "(objetivo 5). Numéricas: ANOVA (normalidade) ou Kruskal-Wallis; ",
        "categóricas: qui-quadrado ou Fisher (contagens esperadas < 5). "
      )),
      "\\label{tab:testes_grupos}"
    ),
    col.names = c("Variável", "Teste", "Estatística", "valor-p")
  ) %>%
  column_spec(1, width = "6.5cm") %>%
  column_spec(2, width = "3.0cm") %>%
  column_spec(3, width = "2.5cm") %>%
  column_spec(4, width = "2.0cm") %>%
  kable_styling(latex_options = c("striped", "repeat_header"), font_size = 9) %>%
  footnote(
    general = "$^{1}$ Teste $t$ de Student; $^{2}$ Teste de Mann--Whitney; $^{3}$ Teste Qui-quadrado; $^{4}$ Teste Exato de Fisher; $^{5}$ ANOVA; $^{6}$ Kruskal--Wallis.",
    general_title = "Nota: ", escape = FALSE
  )

pos_grupos <- split(seq_len(nrow(testes_grupos)), testes_grupos$var)
for (bloco in blocos_grupos) {
  linhas <- unlist(pos_grupos[bloco])
  tab_grupos_kbl <- tab_grupos_kbl %>%
    pack_rows(group_label = tex_safe(names(blocos_grupos)[
      vapply(blocos_grupos, function(b) identical(b, bloco), logical(1))
    ]), start_row = min(linhas), end_row = max(linhas),
    bold = TRUE, escape = FALSE)
}

tab_grupos_kbl %>%
  as.character() %>%
  tex_continuacao() %>%
  tex_quebra_nota() %>%
  writeLines(file.path(dir_saida, "tabela_comparacao_grupos.tex"))

# ---- 8.5 Post-hoc das comparações significativas (objetivo 5) ----------------

# Teste de Dunn (comparações pareadas pós Kruskal-Wallis), com correção de Holm
dunn_test <- function(x, g) {
  g <- factor(g)
  dados <- tibble(x = x, g = g) %>% drop_na()
  rk <- rank(dados$x)
  N <- nrow(dados)
  grupos <- levels(dados$g)
  combos <- combn(length(grupos), 2)
  tab_t <- table(dados$x)
  T <- sum(tab_t^3 - tab_t)

  map_dfr(seq_len(ncol(combos)), function(j) {
    i <- combos[1, j]; k <- combos[2, j]
    ni <- sum(dados$g == grupos[i]); nk <- sum(dados$g == grupos[k])
    Ri <- mean(rk[dados$g == grupos[i]]); Rk <- mean(rk[dados$g == grupos[k]])
    v <- (N * (N + 1) / 12 - T / (12 * (N - 1))) * (1 / ni + 1 / nk)
    z <- (Ri - Rk) / sqrt(v)
    tibble(
      Comparação = sprintf("%s vs %s", grupos[i], grupos[k]),
      z = z,
      p = 2 * pnorm(-abs(z))
    )
  }) %>%
    mutate(p_aj = p.adjust(p, method = "holm"))
}

# Fisher exato pareado (categóricas binárias), com correção de Holm
pairwise_fisher_bin <- function(g, x) {
  g <- factor(g); x <- factor(x)
  dados <- tibble(g = g, x = x) %>% drop_na()
  grupos <- levels(dados$g)
  combos <- combn(length(grupos), 2)

  map_dfr(seq_len(ncol(combos)), function(j) {
    i <- combos[1, j]; k <- combos[2, j]
    sub <- dados %>% filter(g %in% c(grupos[i], grupos[k]))
    tab <- table(sub$g, sub$x)
    tibble(
      Comparação = sprintf("%s vs %s", grupos[i], grupos[k]),
      z = NA_real_,
      p = fisher.test(tab)$p.value
    )
  }) %>%
    mutate(p_aj = p.adjust(p, method = "holm"))
}

# Variáveis com p < 0,05 na comparação dos três grupos
vars_posthoc <- as.character(testes_grupos$var[testes_grupos$p_num < 0.05])

posthoc_detalhes <- map_dfr(vars_posthoc, function(v) {
  g <- base$DHEM_Comb
  x <- base[[v]]
  if (is.numeric(x)) {
    dunn_test(x, g) %>%
      mutate(
        var = v,
        rotulo = tex_safe(rotulos_var[[v]]),
        teste = "Dunn (Holm)"
      )
  } else {
    pairwise_fisher_bin(g, x) %>%
      mutate(
        var = v,
        rotulo = tex_safe(rotulos_cat_grupo[[v]]),
        teste = "Fisher (Holm)"
      )
  }
})

posthoc_detalhes %>%
  mutate(
    `Estatística (z)`  = ifelse(is.na(z), "—", fmt_num(z, 2)),
    `valor-p bruto`    = cell_spec(tex_safe(fmt_p(p)), format = "latex",
                                   bold = p < 0.05, escape = FALSE),
    `valor-p ajustado` = cell_spec(tex_safe(fmt_p(p_aj)), format = "latex",
                                   bold = p_aj < 0.05, escape = FALSE)
  ) %>%
  select(rotulo, Comparação, teste, `Estatística (z)`, `valor-p bruto`, `valor-p ajustado`) %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    escape    = FALSE,
    linesep   = "",
    col.names = tex_safe(c("Variável", "Comparação", "Teste",
                           "Estatística (z)", "valor-p bruto", "valor-p ajustado"))
  ) %>%
  kable_styling(latex_options = c("striped", "hold_position", "scale_down")) %>%
  as.character() %>%
  tex_sem_table_env() %>%
  writeLines(file.path(dir_saida, "tabela_posthoc_grupos.tex"))

# ---------------------------------------------------------------------------- #
# ==== 9. MACROS ADICIONAIS (outputs/macros_analises.tex) ======================
# ---------------------------------------------------------------------------- #

macros_analises <- c()
for (v in criterios_prevalencia) {
  tag <- switch(v,
    IMMA_Baixo       = "IMMABaixo",
    IMMA_Newman      = "IMMANewman",
    IMMA_Baumgartner = "IMMABaumgartner",
    IMMA_FNIH        = "IMMAFNIH",
    IMMA_PESO_Ind    = "IMMAPeso"
  )
  d <- prevalencia %>% filter(var == v)
  macros_analises <- c(
    macros_analises,
    sprintf("\\newcommand{\\IC%sInf}{%s}", tag, fmt_pct(d$ic_inf)),
    sprintf("\\newcommand{\\IC%sSup}{%s}", tag, fmt_pct(d$ic_sup))
  )
}
macros_analises <- c(
  macros_analises,
  sprintf("\\newcommand{\\KappaFleiss}{%s}", fmt_num(kappa_fleiss_geral, 2)),
  sprintf("\\newcommand{\\KappaFleissDHEM}{%s}", fmt_num(kappa_fleiss_dhem, 2)),
  sprintf("\\newcommand{\\NDHEMConcord}{%d}", n_dhem_concord),
  sprintf("\\newcommand{\\BetaFibroseMMT}{%s}", fmt_num(beta_fibrose_mmt, 2)),
  sprintf("\\newcommand{\\PBetaFibroseMMT}{%s}", fmt_p(p_beta_fibrose_mmt))
)
writeLines(macros_analises, file.path(dir_saida, "macros_analises.tex"))

# ---------------------------------------------------------------------------- #
# ==== 10. RESUMO FINAL ========================================================
# ---------------------------------------------------------------------------- #

cat("Pipeline concluído!\n")
cat(sprintf("  - %d variáveis numéricas e %d categóricas analisadas\n",
            length(numericas), length(categoricas)))
cat(sprintf("  - Saídas em: %s/\n", dir_saida))
