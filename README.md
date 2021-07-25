
<!-- README.md is generated from README.Rmd. Please edit that file -->

# plantiosflorestais

<!-- badges: start -->
<!-- badges: end -->

Este pacote tem como objetivo disponibilizar dados de área de
mapeamentos públicos de plantios florestais no Brasil.

O pacote foi resultado do Trabalho de Conclusão de Curso apresentado ao
curso de Engenharia Florestal da Universidade Federal do Paraná (UFPR),
pelo graduando Maykon G. G. Pedro. O trabalho final pode ser acessado
pelo seguinte link: [Ver trabalho]().

Os mapeamentos disponíveis podem ser verificados no tópico “FONTE DE
DADOS” desse `README`.

## Instalação

Você pode instalar a versão de desenvolvimento desse pacote diratemente
do GitHub usando os seguintes comandos:

``` r
# install.packages("devtools")
devtools::install_github("maykongpedro/plantiosflorestais")
```

## Acesso às bases

## Funções disponíveis

## Fonte de dados

Cada relatório ou mapeamento que foi usado nesse pacote está destacado
na tabela a seguir. A fonte dos dados foi mantida dentro das tibbles
existentes no pacote.

| Responsável | Fonte                                 | Relatório                                                                                                                                                                                            |
|-------------|---------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| IBGE        | IBGE                                  | [Histórico disponível no SNIF](https://dados.gov.br/dataset/sistema-nacional-de-informacoes-florestais-snif/resource/fdf7e4ce-8475-4205-8aad-3f97665b8a41)                                           |
| SFB         | IBGE/PEVS                             | [Florestas do Brasil em resumo 2019](http://www.acr.org.br/uploads/biblioteca/Florestas_Brasil_2019_Portugues.pdf)                                                                                   |
| IBÁ         | IBÁ                                   | [Histórico disponível no SNIF](https://dados.gov.br/dataset/sistema-nacional-de-informacoes-florestais-snif/resource/43251bd6-e2c9-4dc8-93c9-379bf15e29d9)                                           |
| IBÁ         | IBÁ/FGV/Pöyry                         | [IBÁ - Relatório Anual 2020](https://www.iba.org/datafiles/publicacoes/relatorios/relatorio-iba-2020.pdf)                                                                                            |
| APRE        | APRE/UFPR                             | [APRE - Estudo Setorial 2020](https://apreflorestas.com.br/publicacoes/estudo-setorial-apre-2020-2/)                                                                                                 |
| IFPR        | SFB/IFPR                              | [Mapeamentos dos plantios florestais do estado do Paraná](https://apreflorestas.com.br/publicacoes/ifpr-e-sfb-mapeamento-dos-plantios-florestais-do-estado-do-parana/)                               |
| ACR         | ACR/UDESC-CAV                         | [ACR - Anuário Estatístico de Base Florestal para o estado de Santa Catarina 2019](http://www.acr.org.br/uploads/biblioteca/Anuario_ACR_2019_atualizado.pdf)                                         |
| AGEFLOR     | AFUBRA/AGEFLOR, FEPAM, RDK Logs, SEMA | [AGEFLOR - A indústria de base florestal no Rio Grande do Sul 2017](http://www.ageflor.com.br/noticias/wp-content/uploads/2017/08/A-INDUSTRIA-DE-BASE-FLORESTAL-NO-RS-2017.pdf)                      |
| AGEFLOR     | AGEFLOR, FEPAM, Codex, RDK Logs       | [AGEFLOR - O setor de base florestal no Rio Grande do Sul 2020](http://www.ageflor.com.br/noticias/wp-content/uploads/2020/12/O-Setor-de-Base-Florestal-no-Rio-Grande-do-Sul-2020-ano-base-2019.pdf) |
| FAMATO      | IMEA                                  | [Diagnóstico de Florestas Plantadas do Estado do Mato Grosso](http://www.arefloresta.org.br/uploads/downloads/00072201414739.pdf)                                                                    |

``` r
#library(plantiosflorestais)
## basic example code
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/master/examples>.
