#' @title Gerar Relatório PDF com Estatísticas e Descrições de Campos
#'
#' @description Cria um relatório em PDF utilizando um template RMarkdown
#'   externo (por padrão, `R/report_template.Rmd`). O template recebe os
#'   dados e as descrições como parâmetros.
#'
#' @param df_raw `tibble` com dados brutos.
#' @param df_cleaned `tibble` com dados limpos.
#' @param df_dwc `tibble` com dados mapeados para DwC.
#' @param cfg Lista de configuração lida do YAML.
#' @param output_dir Diretório onde salvar o PDF.
#' @param base_name Nome base do arquivo (sem extensão).
#' @param template_path Caminho para o arquivo RMarkdown template.
#'
#' @return Caminho do arquivo PDF gerado, ou NULL se falhar.
#'
#' @importFrom rmarkdown render
#' @export
generate_pdf_report <- function(df_raw, df_cleaned, df_dwc, cfg, output_dir, base_name,
                                template_path = "R/report_template.Rmd", clean_intermediate = FALSE) {
  # Garante que o diretório de saída existe
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(output_dir)) {
    warning(sprintf("Não foi possível criar o diretório '%s'. Relatório PDF não gerado.", output_dir))
    return(invisible(NULL))
  }
  
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    warning("Pacote 'rmarkdown' não instalado. Relatório PDF não gerado.")
    return(invisible(NULL))
  }
  
  # Caminho absoluto do template
  template_abs <- normalizePath(template_path, winslash = "/", mustWork = TRUE)
  if (!file.exists(template_abs)) {
    warning(sprintf("Template RMarkdown não encontrado em '%s'. Relatório PDF não gerado.", template_abs))
    return(invisible(NULL))
  }
  
  # Caminho absoluto do diretório de saída
  output_abs <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  
  # Nome do arquivo de saída (apenas o nome, sem caminho)
  output_file_name <- paste0(base_name, "_report.pdf")
  
  # Parâmetros para o template
  params <- list(
    df_raw = df_raw,
    df_cleaned = df_cleaned,
    df_dwc = df_dwc,
    cfg = cfg,
    base_name = base_name
  )
  
  # Renderiza com output_dir e output_file separados
  resultado <- tryCatch(
    {
      rmarkdown::render(
        input = template_abs,
        output_file = output_file_name,
        output_dir = output_abs,
        params = params,
        envir = new.env(parent = globalenv()),
        quiet = FALSE,           # mostra mensagens de erro no console
        clean = clean_intermediate
      )
      pdf_path <- file.path(output_abs, output_file_name)
      message(sprintf("Relatório PDF gerado: %s", pdf_path))
      pdf_path
    },
    error = function(e) {
      warning(sprintf("Falha ao gerar relatório PDF: %s", conditionMessage(e)))
      # Tenta instalar pacotes LaTeX faltantes, se possível
    }
  )
  
  invisible(resultado)
}