

#' Title
#'
#' @param mapeamento
#' @param genero
#'
#' @return
#' @export
#'
#' @examples

plotar_historico_brasil <- function(mapeamento = c("IBA", "IBGE"),
                                    generos = c("Todos", "Eucalyptus", "Pinus",
                                               "Outros")
                                    ){
    # Base
    map_uf <- plantiosflorestais::mapeamentos_estados
    n_gen <- length(generos)

    generos <- c("Todos", "Eucalyptus", "Pinus",
                "Outros")


    # Fitrar
    map_uf %>%
        dplyr::mutate(genero = factor(genero,
                                      levels = c("Eucalyptus", "Pinus", "Outros"),
                                      ordered = TRUE
                                      )) %>%
        dplyr::filter(mapeamento == "IB\u00C1 - Relat\u00F3rio Anual 2020",
                      genero %in% generos) %>%
        dplyr::group_by(ano_base, genero) %>%
        dplyr::summarise(area_ha = sum(area_ha, na.rm = TRUE)) %>%
        print(n=100) %>%

        ggplot2::ggplot()+
        ggplot2::geom_col(ggplot2::aes(x = area_ha, y = ano_base))+
        ggplot2::facet_wrap(~genero)+
        ggplot2::theme_bw()






}


# plotar_comparativo_nacional


# plotar_comparativo_estadual


# plotar_serie_historica <- function(genero == Todos, estado == "Todos",){


# plotar um facet_wrap com os gráficos por genero?

# }
