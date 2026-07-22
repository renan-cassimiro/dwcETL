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
  "yaml", "tibble", "tools", "jsonlite",
  "rmarkdown", "knitr", "kableExtra", "tinytex"   # <-- novos
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

# Garantir que o TinyTeX está instalado
if (!requireNamespace("tinytex", quietly = TRUE)) {
  message("Instalando tinytex...")
  renv::install("tinytex")
}

# if (!tinytex::is_tinytex()) {
#   message("Instalando TinyTeX (pode levar alguns minutos)...")
#   tinytex::install_tinytex()
# } else {
#   # Atualiza pacotes LaTeX existentes
#   tinytex::tlmgr_update()
# }

# Carrega o motor generico (ordem importa: utils antes do runner)
modulos <- c(
  "R/01_io_utils.R",
  "R/02_cleaning.R",
  "R/03_date_standardize.R",
  "R/04_dwc_mapper.R",
  "R/04b_verbatim.R",
  "R/04c_dwc_derivations.R",        # NOVO
  "R/05_pipeline_runner.R",        # renomeado (era 05)
  "R/06_validation.R",             # renomeado (era 06)
  "R/07_report_pdf.R"              # renomeado (era 07)
)
invisible(lapply(modulos, source))

# Executa o pipeline para todas as configuracoes em config/
resumo <- run_pipeline(
  config_dir = "config",
  raw_dir = "dados/raw",
  processed_dir = "dados/processed"
)
