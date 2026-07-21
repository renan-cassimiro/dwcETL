#!/usr/bin/env Rscript
# =============================================================================
# main.R
# Ponto de entrada do Pipeline de ETL Darwin Core.
#
# Uso:
#   Rscript main.R
#
# O script:
#   1. Garante que os pacotes necessarios estao instalados.
#   2. Carrega (source) todos os modulos do motor generico em R/.
#   3. Executa run_pipeline(), que varre config/*.yaml e processa cada
#      planilha bruta correspondente em dados/raw/, gravando as saidas em
#      dados/processed/.
# =============================================================================

pacotes_necessarios <- c(
  "readr", "readxl", "dplyr", "stringr", "lubridate",
  "yaml", "tibble", "tools"
)

pacotes_ausentes <- pacotes_necessarios[!vapply(
  pacotes_necessarios, requireNamespace, logical(1), quietly = TRUE
)]

if (length(pacotes_ausentes) > 0) {
  stop(sprintf(
    "Pacotes ausentes: %s.\nInstale com: install.packages(c(%s))",
    paste(pacotes_ausentes, collapse = ", "),
    paste(sprintf("'%s'", pacotes_ausentes), collapse = ", ")
  ))
}

# Carrega o motor generico (ordem importa: utils antes do runner)
modulos <- c(
  "R/01_io_utils.R",
  "R/02_cleaning.R",
  "R/03_date_standardize.R",
  "R/04_dwc_mapper.R",
  "R/04b_verbatim.R",
  "R/05_pipeline_runner.R"
)
invisible(lapply(modulos, source))

# Executa o pipeline para todas as configuracoes em config/
resumo <- run_pipeline(
  config_dir = "config",
  raw_dir = "dados/raw",
  processed_dir = "dados/processed"
)
