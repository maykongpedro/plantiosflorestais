
#' Mapeamentos/Relatorios disponiveis dentro do pacote
#'
#' Usando as duas bases existentes no pacote, resume quais sao os mapeamentos e
#' relatorios disponiveis no pacote, alem de informar qual a fonte.
#'
#' @return Uma tibble com 3 colunas
#' @export
#'
#' @examples
mapeamentos_disponiveis <- function(){

    municipais <- plantiosflorestais::mapeamentos_municipios %>%
        dplyr::distinct(mapeamento, fonte) %>%
        dplyr::mutate(base = "mapeamentos_municipios")

    estaduais <- plantiosflorestais::mapeamentos_municipios %>%
        dplyr::distinct(mapeamento, fonte) %>%
        dplyr::mutate(base = "mapeamentos_estados")


    tabela <- dplyr::bind_rows(municipais, estaduais) %>%
        dplyr::relocate(base, .before = mapeamento)
    tabela

}

#' Quais os genero
#'
#' Texto descriçao
#'
#' @return
#' @export
#'
#' @examples
generos_plantios_disponiveis <- function(){


}



#' Título
#'
#' Texto descriçao
#'
#' @return
#' @export
#'
#' @examples
estados_com_municipios_mapeados <- function(){


}
