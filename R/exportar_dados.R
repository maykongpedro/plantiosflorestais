
#' Exportar bases em formato .xlsx (Excel Files).
#'
#' Exporta as duas bases em planilhas distintas de uma pasta de trabalho excel.
#'
#' @param caminho_para_salvar_bases caminho da pasta para salvar o arquivo, como
#' "C:/documentos/" ou "./" para savar no projeto ativo
#' @param ... parametros adicionais para a exportacao
#'
#' @return invisivel
#' @export
#'
#' @examples
exportar_xlsx <- function(caminho_para_salvar_bases, ...){

    # Bases
    map_muni <- plantiosflorestais::mapeamentos_municipios
    map_uf <- plantiosflorestais::mapeamentos_estados

    # Gerar e nomear lista com as bases
    bases <- list(map_muni, map_uf)
    names(bases) <- c("mapeamentos_municipios", "mapeamentos_estados")


    # Nome padrão do arquivo
    nome_arquivo <- paste0(caminho_para_salvar_bases,
                             "/mapeamentos_plantios_florestais",
                             ".xlsx")

    # Exportar base
    bases %>%
        writexl::write_xlsx(nome_arquivo, ...)

}



