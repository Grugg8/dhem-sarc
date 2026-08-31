# Associação entre DHEM e Sarcopenia — Projeto I

Projeto da disciplina **GET00109 — Prática Estatística III** (2026.2):
consultoria estatística para o projeto *Disfunções Endócrinas em Pacientes com
DHEM (Sub-projeto: Sarcopenia)*, com dados do Ambulatório de Endocrinologia do
HUAP/UFF, elaborado em **R** (script único e reproduzível) e relatório
científico em **LaTeX**.

## O problema

A doença hepática esteatótica associada à disfunção metabólica (DHEM/MASLD) e a
sarcopenia parecem compartilhar vias metabólicas e inflamatórias, com relação
potencialmente bidirecional. A consultoria respondeu cinco perguntas do
requisitante:

1. Frequência de baixa massa magra apendicular (MMEA) sob diferentes critérios
   de ajuste (bruto, Newman, Baumgartner, FNIH, razão MMEA/peso), com IC 95%;
2. Concordância entre os ajustes de MMEA (testes pareados, correlação,
   regressão `Y = bX`, Bland–Altman e coeficientes Kappa);
3. Diferenças entre pacientes com e sem fibrose hepática (triagem +
   regressão logística múltipla);
4. Diferenças entre pacientes com e sem DHEM (mesma estratégia);
5. Comparação dos três grupos (sem DHEM; DHEM sem fibrose; DHEM com fibrose).

## Estrutura do repositório

| Caminho | Conteúdo |
|---|---|
| `script_principal.R` | Script único em R: pipeline completo e reproduzível |
| `relatorio.tex` / `relatorio.pdf` | Relatório científico (fonte e versão compilada) |
| `outputs/` | Tabelas LaTeX, macros e figuras geradas automaticamente pelo script |

**`base.xlsx` não está no repositório** (dados de pacientes, mesmo
desidentificados, não devem ser publicados). Para replicar, obtenha a base junto
aos responsáveis pelo estudo e posicione-a na raiz do projeto.

## Estrutura esperada da base (`base.xlsx`)

- Aba `Planilha de Dados`: 165 pacientes × ~40 variáveis — demografia
  (idade, sexo, etnia, atividade física, tabagismo), comorbidades
  (obesidade/sobrepeso, diabetes, HAS), doença hepática (MASLD, esteatose por
  FLI/Saadeh/CAP, fibrose) e composição corporal/sarcopenia
  (gordura e massa magra por DXA/BIA, IMC, MMEA e seus ajustes, SARC-F,
  handgrip, elevação de cadeira, velocidade de marcha);
- Aba `Legendas`: dicionário de variáveis e pontos de corte.

## Como replicar

### 1. Requisitos

- **R 4.x** e os pacotes:

```r
install.packages(c("MASS", "tidyverse", "readxl", "kableExtra", "patchwork",
                   "scales", "broom", "ResourceSelection", "car", "pROC"))
```

- **Distribuição LaTeX** com `pdflatex`.

### 2. Executar a análise

Com `base.xlsx` na raiz:

```r
Rscript script_principal.R
```

O script gera todo o conteúdo de `outputs/` (tabelas `booktabs`, macros LaTeX
dinâmicas e figuras vetoriais) usado pelo relatório.

### 3. Compilar o relatório

```bash
pdflatex relatorio.tex
pdflatex relatorio.tex
pdflatex relatorio.tex   # 3 execuções para as referências cruzadas
```

## Destaques dos resultados

- A frequência de baixa massa magra variou substancialmente conforme o critério
  de ajuste (7,3% a 44,8%), e a concordância entre critérios foi fraca a
  moderada (Kappa de Cohen 0,09–0,48) — os critérios não são intercambiáveis;
- Os ajustes de MMEA operam em escalas incomensuráveis: os modelos de conversão
  linear `Y = bX` violaram pressupostos de forma severa e não devem ser usados
  como fatores de conversão;
- No modelo logístico de fibrose (n = 134) ficaram etnia, IMC e alteração no
  teste de elevação da cadeira; no de DHEM (n = 124), idade, atividade física,
  gordura corporal e velocidade de marcha alterada;
- IMC, gordura corporal e obesidade apresentaram gradiente crescente com a
  gravidade da doença hepática.

Detalhes completos (métodos, diagnósticos dos modelos e limitações) no
[`relatorio.pdf`](relatorio.pdf).

## Nota ética

Dados provenientes de estudo com aprovação de Comitê de Ética em Pesquisa.
A base é desidentificada, porém composta por dados reais de saúde; por
precaução, não é distribuída neste repositório.
