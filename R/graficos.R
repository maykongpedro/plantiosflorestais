
#' Plotar Grafico de Historico com os dados do IBA
#'
#' Realiza um plot simples com os dados de mapeamento do IBA, tanto dos dados
#' disponibilizados pelo SNIF quanto dos dados apresentados no Relatorio Anual 2020.
#' O plot pode ser para um estado especifico ou geral.
#'
#' A funcao pode demorar um pouco para demonstrar o resultado.
#'
#' @param abrangecia_uf Filtro de estado, como "PR", por exemplo. O padrao e NULL,
#' para trazer dados do Brasil inteiro.
#' @param exibir_rotulos TRUE ou FALSE. Se TRUE exibe os rotulos em cima das colunas.
#'
#' @return Um grafico de colunas por ano, da area em milhoes de hectares.
#' @export
#'
#' @examples
plotar_historico_iba <- function(abrangecia_uf = NULL,
                                 exibir_rotulos = FALSE) {

    # Gera um warning por conta d fonte usada, entao suprime com a funcao abaixo
    suppressWarnings({

    # Base
    map_uf <- plantiosflorestais::mapeamentos_estados

    # Definir filtro de regi\u00E3o

    if (is.null(abrangecia_uf)) {

        # aloca o mapeamento do SNIF
        iba_snif_abrangencia <- map_uf %>%
            dplyr::filter(mapeamento == "IB\u00C1 - N\u00E3o identificado")

        # aloca o mapeamento do relat\u00F3rio 2020
        iba_relatorio_abrangecia <- map_uf %>%
            dplyr::filter(mapeamento == "IB\u00C1 - Relat\u00F3rio Anual 2020")


    } else if(stringr::str_detect(abrangecia_uf, "[A-Z]{2}") == FALSE) {

        stop("Por gentileza, preencha uma UF v\u00E1lida.", call. = FALSE)

    } else{

        iba_snif_abrangencia <- map_uf %>%
            dplyr::filter(
                mapeamento == "IB\u00C1 - N\u00E3o identificado",
                uf == abrangecia_uf
            )

        iba_relatorio_abrangecia <- map_uf %>%
            dplyr::filter(
                mapeamento == "IB\u00C1 - Relat\u00F3rio Anual 2020",
                uf == abrangecia_uf
                )

    }

    # Fazer sumariza\u00E7\u00E3o da base do SNIF
    iba_snif <- iba_snif_abrangencia %>%
        dplyr::group_by(ano_base, genero) %>%
        dplyr::summarise(area_ha = sum(area_ha, na.rm = TRUE)) %>%
        dplyr::filter(ano_base %in% c("2006", "2007", "2008"))



    # Fazer sumariza\u00E7\u00E3o da base do relat\u00F3rio 2020
    iba_relatorio <- iba_relatorio_abrangecia %>%
        dplyr::group_by(ano_base, genero) %>%
        dplyr::summarise(area_ha = sum(area_ha, na.rm = TRUE))


    # Juntar as duas bases
    iba <- dplyr::bind_rows(iba_snif, iba_relatorio) %>%
        dplyr::mutate(
            genero = factor(
                genero,
                levels = c("Outros",
                           "Pinus",
                           "Eucalyptus"),
                ordered = TRUE

            )
        )

    # Conferir se encontrou alguma coisa
    linhas_df <- nrow(iba)
    if (linhas_df == 0) {
        stop("Nada encontrado. Por gentileza, tente outra UF.", call.= FALSE)
    }


    # Adicionar fonte de texto necess\u00C1ria
    sysfonts::font_add_google("Crimson Text","Crimson")
    showtext::showtext_auto()


    # Definir configura\u00E7\u00F5es gerais para o gr\u00C1fico
    config_plot <-
        c(
            "eucalipto" = "#35B779",
            "pinus" = "#ED7953",
            "outros" = "#31688E",
            "background" = "#F5F5F2",
            "text" = "#22211D",
            "text_fam" = "Crimson",
            "eixo_x_title" = "Ano-base",
            "eixo_y_title" = "\u00C1rea (Milh\u00F5es de hectares)",
            "title" = " - Hist\u00F3rico de \u00C1rea plantada por g\u00EAnero, considerando informa\u00E7\u00F5es do IB\u00C1",
            "caption" = "**@Dataviz:** *Pacote R - maykongpedro/plantiosflorestais*"
        )

    #Op\u00E7\u00E3o simples
    # subtitulo_iba  <- paste0("S\u00E9rie hist\u00F3rica de 2006 a 2008 baseada nos dados do IB\u00C1",
    #                          " disponilizados pelo SNIF.", " Para os anos de 2009 a 2019",
    #                          " os dados utilizados s\u00E3o referentes ao Relat\u00F3rio Anual 2020.",
    #                          " Fonte: 2006-2008: IB\u00C1 |  2009-2017: P\u00F6yry & IB\u00C1 | 2018-2019: FGV & IB\u00C1"
    # )

    # OP\u00E7\u00E3o com quebra de linha na fonte
    # subtitulo_iba  <- paste0("S\u00E9rie hist\u00F3rica de 2006 a 2008 baseada nos dados do IB\u00C1",
    #                          " disponilizados pelo SNIF.", "<br>Para os anos de 2009 a 2019",
    #                          " os dados utilizados s\u00E3o referentes ao **Relat\u00F3rio Anual 2020**.",
    #                          "<br><br>**Fonte dos dados:** <br>2006-2008: IB\u00C1 <br>2009-2017: P\u00F6yry & IB\u00C1 <br>2018-2019: FGV & IB\u00C1"
    # )

    # # op\u00E7\u00E3o sem quebra de linha na  nem it\u00C1lico
    # subtitulo_iba  <- paste0("S\u00E9rie hist\u00F3rica de 2006 a 2008 baseada nos dados do IB\u00C1",
    #                          " disponilizados pelo SNIF.", "<br>Para os anos de 2009 a 2019",
    #                          " os dados utilizados s\u00E3o referentes ao **Relat\u00F3rio Anual 2020**.",
    #                          "<br><br>**Fonte dos dados:** 2006-2008: IB\u00C1 | ",
    #                          "2009-2017: P\u00F6yry & IB\u00C1 | ",
    #                          "2018-2019: FGV & IB\u00C1"
    # )

    # Definir subtítulo
    #op\u00E7\u00E3o sem quebra de linha na fonte
    subtitulo_iba  <- paste0("S\u00E9rie hist\u00F3rica de 2006 a 2008 baseada nos dados do IB\u00C1",
                             " disponilizados pelo **SNIF**.", "<br>Para os anos de 2009 a 2019",
                             " os dados utilizados s\u00E3o referentes ao **Relat\u00F3rio Anual 2020**.",
                             "<br><br>**Fonte dos dados:** *2006-2008*: IB\u00C1 | ",
                             "*2009-2017*: P\u00F6yry & IB\u00C1 | ",
                             "*2018-2019*: FGV & IB\u00C1"
    )


    # Corrgindo a vari\u00C1vel de localidade/abrang\u00EAncia para o t\u00EDtulo
    if (is.null(abrangecia_uf)){
        localidade <- "Brasil"
    } else{
        localidade <- abrangecia_uf
    }


    # Definindo limite do eixo y
    #options(scipen=999)
    max_lim <- iba %>%
        dplyr::group_by(ano_base) %>%
        dplyr::summarise(area_ha = sum(area_ha)) %>%
        dplyr::summarise(maximo = max(area_ha)) %>%
        dplyr::pull(maximo) %>%
        round(-5) # arrendondar para a pr\u00F3xima casa de milhar

    max_lim <- max_lim/10^6
    max_lim <- max_lim + max_lim * 0.15


    # Gerar totais para as labels
   totais <-

        iba %>%
        dplyr::group_by(ano_base) %>%
        dplyr::summarise(total = sum(area_ha/10^6)) %>%
        dplyr::mutate(
            total = round(total, 2),
            total_label = sprintf("%1.2f M.", total)
            )
   totais




   iba %>%
       dplyr::group_by(ano_base,genero) %>%
       dplyr::summarise(total = sum(area_ha/10^6)) %>%
       dplyr::mutate(
           total = round(total, 2),
           total_label = sprintf("%1.2f M.", total)
       )




    # Gerar plot
    plot <- iba %>%
        dplyr::mutate(area_ha = area_ha / 10 ^ 6) %>%
        ggplot2::ggplot() +
        ggplot2::geom_col(ggplot2::aes(
            x = ano_base,
            y = area_ha,
            fill = genero
        ))+
        ggplot2::scale_y_continuous(
            expand = c(0, 0),
            limits = c(0, max_lim)
        ) +
        ggplot2::scale_fill_manual(
            breaks = c("Outros",
                       "Pinus",
                       "Eucalyptus"),

            values = c(config_plot[["outros"]],
                       config_plot[["pinus"]],
                       config_plot[["eucalipto"]]),
            guide = ggplot2::guide_legend(reverse = TRUE)
        ) +

        ggplot2::ggtitle(
            paste0(localidade,
                   config_plot[["title"]]),
            subtitle = glue::glue(subtitulo_iba)
            ) +

        ggplot2::labs(x = config_plot[["eixo_x_title"]],
                      y =  config_plot[["eixo_y_title"]],
                      fill = NULL,
                      caption = config_plot[["caption"]])+

        ggplot2::theme_minimal(base_family = config_plot[["text_fam"]],  base_size = 16) +
        ggplot2::theme(
            plot.background = ggplot2::element_rect(fill = config_plot[["background"]], color = NA),
            panel.background = ggplot2::element_rect(fill = config_plot[["background"]], color = NA),
            plot.margin = ggplot2::unit(c(1,  1 , 1, 1), "cm"),
            legend.position = "top",
            axis.line = ggplot2::element_line(size = 1),
            axis.ticks = ggplot2::element_line(size = 1),
            text = ggplot2::element_text(color = config_plot[["text"]]),
            axis.title = ggplot2::element_text(face = "bold"),
            plot.title = ggplot2::element_text(face = "bold"),
            plot.subtitle = ggtext::element_markdown(),
            plot.caption = ggtext::element_markdown()
        )



    # Rótulos para cada divisão da barra
    # plot +
    #     ggplot2::geom_text(
    #         ggplot2::aes(
    #             x = ano_base,
    #             y = area_ha,
    #             label = round(area_ha,2),
    #             group = genero
    #         ),
    #         position = ggplot2::position_stack(vjust = .5),
    #     )

    # Plotagem final com rótulos
    if(exibir_rotulos == TRUE){

        plot +
            ggplot2::geom_text(
                ggplot2::aes(
                    x = ano_base,
                    y = total,
                    label = total_label,
                    #label = total,
                    vjust = -1.5
                ),
                size = 4,
                fontface = "bold",
                data = totais
            )

    } else{

        plot

    }

    })

}

