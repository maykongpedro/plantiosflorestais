
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


#' Generos existentes nos mapeamentos disponiveis
#'
#' Resume quais sao os generos dos plantios florestais existentes nas bases utilizadas
#' no pacote.
#'
#' @param exibir_nome_mapeamento padrao = FALSE, se TRUE exibe o nome do mapeamento
#'
#' @return Uma tibble
#' @export
#'
#' @examples
generos_plantios_disponiveis <- function(exibir_nome_mapeamento = FALSE){

    map_muni <- plantiosflorestais::mapeamentos_municipios

        if (exibir_nome_mapeamento == TRUE) {

            map_muni %>%
            dplyr::distinct(genero, mapeamento)

        } else {

            map_muni %>%
            dplyr::distinct(genero)
        }


    map_uf <- plantiosflorestais::mapeamentos_estados

    if (exibir_nome_mapeamento == TRUE) {

        map_uf %>%
            dplyr::distinct(genero, mapeamento)

    } else {

        map_uf %>%
            dplyr::distinct(genero)
    }
}

