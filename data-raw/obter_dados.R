## code to prepare `DATASET` dataset goes here

# Usar pipe
usethis::use_pipe()
devtools::document()


# Fazer download dos mapeamentos faxinados --------------------------------

# obter links
mapeamentos_municipios_url <- "https://github.com/maykongpedro/2021-07-04-extracao-mapeamentos-plantios-florestais/raw/master/data/consolidado/mapeamentos_municipios.rds"
mapeamentos_gerais_url <- "https://github.com/maykongpedro/2021-07-04-extracao-mapeamentos-plantios-florestais/raw/master/data/consolidado/mapeamentos_gerais.rds"

# mapeamento com municípios
download.file(mapeamentos_municipios_url,
              destfile = "data-raw/mapeamentos_municipios.rds")


# mapeamento estadual e nacional
download.file(mapeamentos_gerais_url,
              destfile = "data-raw/mapeamentos_gerais.rds")


# Importar dados para o pacote --------------------------------------------

# carregar mapeamento de municípios
mapeamentos_municipios_raw <- readr::read_rds("./data-raw/mapeamentos_municipios.rds")

# retirar coluna de lati e long da base de muni
mapeamentos_municipios <- mapeamentos_municipios %>%
    dplyr::select(-latitude,
                  -longitude)

# carregar mapeamento geral
mapeamentos_estados <- readr::read_rds("./data-raw/mapeamentos_gerais.rds")


usethis::use_data(mapeamentos_municipios, overwrite = TRUE)
usethis::use_data(mapeamentos_estados, overwrite = TRUE)


