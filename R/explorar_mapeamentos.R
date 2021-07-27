
#' Mapeamentos/Relatorios disponiveis dentro do pacote
#'
#' Usando as duas bases existentes no pacote, resume quais sao os mapeamentos e
#' relatorios disponiveis, alem de informar qual a fonte dos mesmos.
#'
#' @return Uma tibble com 3 colunas
#' @export
#'
#' @examples
mapeamentos_disponiveis <- function(){

    municipais <- plantiosflorestais::mapeamentos_municipios %>%
        dplyr::distinct(mapeamento, fonte) %>%
        dplyr::mutate(base = "mapeamentos_municipios")

    estaduais <- plantiosflorestais::mapeamentos_estados %>%
        dplyr::distinct(mapeamento, fonte) %>%
        dplyr::mutate(base = "mapeamentos_estados")


    tabela <- dplyr::bind_rows(municipais, estaduais) %>%
        dplyr::relocate(base, .before = mapeamento)
    tabela

}


# mapeamento_existente_uf  <- function(unidade_federativa = "PR"){
#
#     unidade_federativa = "PR"
#
#     plantiosflorestais::mapeamentos_municipios %>%
#         dplyr::filter(uf == unidade_federativa) %>%
#         dplyr::group_by(mapeamento, ano_base) %>%
#         dplyr::summarise(area_total_ha = sum(area_ha, na.rm = TRUE)) %>%
#         dplyr::mutate(abordagem = "Por município")
#         # tidyr::pivot_wider(names_from = ano_base,
#         #                    values_from = area_total_ha)
#
#
#     plantiosflorestais::mapeamentos_estados %>%
#         dplyr::filter(uf == unidade_federativa) %>%
#         dplyr::group_by(mapeamento, ano_base) %>%
#         dplyr::summarise(area_total_ha = sum(area_ha, na.rm = TRUE)) %>%
#         dplyr::mutate(
#             abordagem = dplyr::case_when(
#                 mapeamento == "AGEFLOR - A indústria de base florestal no Rio Grande do Sul 2017" ~ "Por coredes",
#                 mapeamento == "APRE - Estudo Setorial 2020" ~ "Por região",
#                 TRUE ~ "Estadual"
#             )
#         )
#     # tidyr::pivot_wider(names_from = ano_base,
#     #                    values_from = area_total_ha)
#
# }


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

    # preciso alterar para gerar uma base com tudo e depois decide se exibe o nome
    # do mapeamento
    map_uf <- plantiosflorestais::mapeamentos_estados

    if (exibir_nome_mapeamento == TRUE) {
        map_uf %>%
            dplyr::distinct(genero, mapeamento)

    } else {
        map_uf %>%
            dplyr::distinct(genero)
    }
}

