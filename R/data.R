## Load Data
library(magrittr)

centro_2015 <- readr::read_csv2("./inst/centro_2015.csv")

centro_13_15 <- readr::read_csv2("./inst/centro_2013_2015.csv")

itacorubi_2015 <- readr::read_csv2("./inst/itacorubi_2015.csv")

trivelloni_2005 <- readr::read_csv2("./inst/trivelloni_2005.csv")

loteamento <- readr::read_csv("./inst/loteamento_residencial.csv")

renda <- readr::read_csv("./inst/dados_renda.csv")

canasvieiras_97 <- readr::read_csv("./inst/terrenos_canasvieiras_1997.csv")

trindade_ap <- readr::read_csv("./inst/apartamentos_trindade.csv")

terrenos_praia <- readr::read_csv("./inst/terrenos_praia.csv")

trindade <- tibble::tibble(VU = c(427, 458, 510, 511, 528, 545,
                                  564, 574, 574, 590, 601, 602,
                                  602, 609, 620))

jungles <- tibble::tribble(
  ~Meses,  ~a, ~b, ~c, ~d, ~e, ~f, ~g, ~h, ~i, ~j, ~k, ~l, ~m, ~n, ~o, ~p, ~q, ~r, ~s, ~t, ~u, ~v, ~x, ~y,
       1, 100, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
       2,  47, 53, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
       3,  23, 50, 27, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
       4,  14, 33, 37, 16, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
       5,   9, 24, 30, 28,  9, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
       6,   7, 17, 23, 26, 21,  6, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
       7,   5, 12, 19, 23, 20 ,17,  4, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
       8,   4, 10, 15, 17, 20, 18, 13,  3, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
       9,   3,  8, 13, 16, 16, 18, 14,  9,  3, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
      10,   2,  6, 11, 14, 14, 15, 14, 14,  8,  2, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
      11,   2,  5,  8, 12, 13, 13, 14, 15, 12,  4,  2, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
      12,   2,  5,  6, 11, 11, 11, 14, 12, 11, 11,  4,  2, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
      13,   2,  4,  6,  8, 11, 11, 11, 11, 13,  9,  8,  5,  1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
      14,   2,  3,  5,  6, 10, 10, 11, 11, 12,  9,  9,  8,  3,  1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
      15,   2,  2,  4,  7,  8, 10, 10, 10, 10, 10,  9,  8,  6,  3,  1, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
      16,   2,  2,  3,  6,  7,  8,  9,  9, 10, 10,  9,  9,  8,  4,  3,  1, NA, NA, NA, NA, NA, NA, NA, NA, 
      17,   2,  2,  3,  5,  5,  8,  9,  9,  9, 10, 10,  8,  7,  6,  4,  2,  1, NA, NA, NA, NA, NA, NA, NA, 
      18,   2,  2,  3,  4,  4,  8,  8,  8,  8,  9,  9,  9,  8,  6,  6,  3,  2,  1, NA, NA, NA, NA, NA, NA, 
      19,   1,  2,  3,  4,  4,  7,  7,  8,  8,  8,  7,  7,  7,  7,  6,  6,  4,  3,  1, NA, NA, NA, NA, NA,
      20,   1,  2,  3,  3,  3,  4,  6,  7,  9,  9,  8,  8,  7,  6,  6,  6,  6,  3,  2,  1, NA, NA, NA, NA,
      21,   1,  1,  2,  2,  4,  6,  6,  6,  6,  7,  7,  8,  7,  7,  6,  6,  6,  5,  4,  2,  1, NA, NA, NA,
      22,   1,  2,  2,  2,  4,  4,  6,  6,  6,  6,  7,  8,  8,  7,  6,  6,  5,  5,  4,  2,  2,  1, NA, NA,
      23,   1,  2,  2,  2,  4,  4,  5,  5,  6,  6,  7,  6,  6,  6,  6,  6,  6,  6,  5,  4,  2,  2,  1, NA,
      24,   1,  1,  2,  2,  3,  3,  4,  6,  6,  6,  6,  6,  7,  7,  6,  6,  6,  5,  5,  5,  3,  2,  1,  1
)
  
                    

## Tidying data

# centro_2015
centro_2015 <- centro_2015[,-1]
colnames(centro_2015) <- c("valor", "area_total", "quartos", "suites",
                           "garagens", "dist_b_mar", "padrao", "E", "N")

centro_2015$padrao <-
  stringi::stri_trans_general(centro_2015$padrao, "latin-ascii")

padrao_levels <- c("baixo", "medio", "alto")

centro_2015$padrao %<>% readr::parse_factor(padrao_levels)

# centro_13_15
centro_13_15 <- centro_13_15[,-1]

colnames(centro_13_15) <- c("valor", "area_total", "quartos", "suites",
                            "garagens", "dist_b_mar", "padrao", "novo",
                            "data_2015", "E", "N")

centro_13_15$padrao %<>%
  readr::parse_factor(levels = c(1, 2, 3))

centro_13_15$padrao %<>%
  forcats::fct_recode(baixo = "1", medio = "2", alto = "3")

centro_13_15$novo %<>% readr::parse_factor(c(0, 1))
centro_13_15$novo %<>% forcats::fct_recode(usado = "0", novo = "1")

centro_13_15$data_2015 %<>% readr::parse_factor(levels = c(0, 1))

centro_13_15$data_2015 %<>% forcats::fct_recode(nao = "0", sim = "1")

# itacorubi_2015
itacorubi_2015 <- itacorubi_2015[,-1]

colnames(itacorubi_2015) <- c("valor", "area_total", "area_privativa",
                              "quartos", "suites", "garagens", "padrao",
                              "elevador", "novo", "E", "N")

itacorubi_2015$padrao %<>%
  readr::parse_factor(levels = padrao_levels)

itacorubi_2015$elevador <-
  stringi::stri_trans_general(itacorubi_2015$elevador, "latin-ascii")

itacorubi_2015$elevador %<>%
  readr::parse_factor(levels = NULL)

itacorubi_2015$novo %<>%
  readr::parse_factor(levels = NULL)

# trivelloni_2005
colnames(trivelloni_2005) <- c("Obs", "E", "N", "valor", "tipo", "area_total",
                               "area_terreno", "garagens", "novo",
                               "P_2", "P_3", "P_4")

trivelloni_2005$novo %<>%
  readr::parse_factor(levels = c(0, 1))

trivelloni_2005$novo %<>%
  forcats::fct_recode(usado = "0", novo = "1")

trivelloni_2005$garagens %<>%
  readr::parse_factor(levels = c(0, 1))

trivelloni_2005$garagens %<>%
  forcats::fct_recode(nao = "0", sim = "1")

trivelloni_2005$padrao <-
  with(trivelloni_2005, ifelse(P_2 == 1, "alto",
                               ifelse(P_3 == 1, "medio",
                                      ifelse(P_4 == 1, "baixo", NA))))

trivelloni_2005$padrao %<>%
  readr::parse_factor(levels = padrao_levels)

trivelloni_2005 %<>% dplyr::select(-c(P_2, P_3, P_4))

trivelloni_2005$tipo %<>%
  readr::parse_factor(levels = c("Apartame", "Kitinete",
                                 "Comercia", "terreno",
                                 "casa")
  )

trivelloni_2005$tipo %<>%
  forcats::fct_recode(casa = "casa",
                      apartamento = "Apartame", kitinete = "Kitinete",
                      comercial = "Comercia", terreno = "terreno")

trivelloni_2005 %<>%
  dplyr::mutate_at(dplyr::vars(area_terreno), function(x) ifelse(x == 0, 0.1, x))

trivelloni_2005 <- trivelloni_2005[, -1]

## Transform data.frames to SpatialPointsDataFrame

centro_2015 <-
  sp::SpatialPointsDataFrame(coords = centro_2015[c("E", "N")],
                             data = subset(centro_2015, select = -c(E,N)),
                             proj4string = sp::CRS("+proj=utm +zone=22 +south
                                                   +units=m +ellps=aust_SA
                                                   +towgs84=-67.35,3.88,-38.22"))

centro_13_15 <-
  sp::SpatialPointsDataFrame(coords = centro_13_15[c("E", "N")],
                             data = subset(centro_13_15, select = -c(E, N)),
                             proj4string = sp::CRS("+proj=utm +zone=22 +south
                                                   +units=m +ellps=aust_SA
                                                   +towgs84=-67.35,3.88,-38.22"))

itacorubi_2015 <-
  sp::SpatialPointsDataFrame(coords = itacorubi_2015[c("E", "N")],
                             data = subset(itacorubi_2015, select = -c(E,  N)),
                             proj4string = sp::CRS("+proj=utm +zone=22 +south
                                                   +units=m +ellps=aust_SA
                                                   +towgs84=-67.35,3.88,-38.22"))

trivelloni_2005 <-
  sp::SpatialPointsDataFrame(coords = trivelloni_2005[c("E", "N")],
                             data = subset(trivelloni_2005, select = -c(E, N)),
                             proj4string = sp::CRS("+proj=utm +zone=22 +south
                                                   +units=m +ellps=aust_SA
                                                   +towgs84=-67.35,3.88,-38.22"))

devtools::use_data(centro_2015, centro_13_15, itacorubi_2015,
                   trivelloni_2005, loteamento, renda, 
                   canasvieiras_97, trindade_ap, terrenos_praia,
                   trindade, jungles, overwrite = TRUE)

#' Prices of 50 Florianopolis' downtown apartaments
#'
#' A SpatialPointsDataFrame containing a sample of 50 apartaments with prices and other
#' attributes in Florianopolis' downtown
#'
#' @format A tibble with 53 rows (50 samples and 3 apartments to be
#'   appraised) and 7 variables:
#' \itemize{
#'   \item valor: price, in brazilian Reais
#'   \item area_total: Total Area, in squared meters
#'   \item quartos: Rooms
#'   \item suites: Ensuites
#'   \item garagens: Garages
#'   \item dist_b_mar: Distance to the beach
#'   \item padrao: Building Standard - baixo, medio, alto
#'   (i.e. low, normal, high)
#' }
#' @source \strong{HOCHHEIM, Norberto}. \emph{Engenharia de avaliacoes imobiliarias: Modulo Basico}.
#'  Florianopolis: IBAPE/SC, 2015, p.21-22
"centro_2015"

#' Prices of Florianopolis' downtown apartaments in 2013 and 2015
#'
#' A SpatialPointsDataFrame containing two samples in different dates with prices and other
#' attributes in Florianopolis' downtown
#'
#' @format A tibble with 90 rows and 11 variables:
#' \itemize{
#'    \item valor: price, in brazilian Reais
#'    \item area_total: Total Area, in squared meters
#'    \item quartos: Rooms
#'    \item suites: Ensuites
#'    \item garagens: Garages
#'    \item dist_b_mar: Distance to the beach
#'    \item padrao: Building Standard - baixo, medio,
#'     alto (i.e. low, normal, high)
#'    \item novo: boolean for brand new apartments -
#'     sim, nao (TRUE, FALSE)
#'    \item data_2015: boolean to specify if data is from 2015 -
#'     sim, nao (TRUE, FALSE)
#' }
#' @source \strong{IBAPE/SC}. \emph{Curso de Engenharia de Avaliacoes Imobiliarias: modulo basico}.
#' Florianopolis, maio/2015
"centro_13_15"

#' Prices of 63 Florianopolis' apartaments in Itacorubi neighbourhood
#'
#' A SpatialPointsDataFrame containing a sample of 63 apartaments with prices and other
#' attributes in Florianopolis' Itacorubi neighbourhood
#'
#' @format A tibble with 53 rows (50 samples and 3 apartments to be
#'  appraised) and 9 variables:
#' \itemize{
#'    \item valor: price, in brazilian Reais
#'    \item area_total: Total Area, in squared meters
#'    \item area_privativa: Private Area, in squared meters
#'    \item quartos: Rooms
#'    \item suites: Ensuites
#'    \item garagens: Garages
#'    \item padrao: Building Standard - baixo, medio,
#'     alto (i.e. low, normal, high)
#'    \item elevador: boolean checking the existance of elevators -
#'     sim, nao (TRUE, FALSE)
#'    \item novo: boolean for brand new apartaments -
#'     novo, usado (TRUE, FALSE)
#' }
#' @source \strong{IBAPE/SC}. \emph{Curso de Engenharia de Avaliacoes Imobiliarias: modulo basico}.
#' Florianopolis, maio/2015
"itacorubi_2015"

#' Prices of 249 Florianopolis' Real Estate
#'
#' A SpatialPointsDataFrame containing a sample of 249 Real Estate with prices and other
#' attributes in Florianopolis' continental area
#'
#' @format A tibble with 249 rows and 7 variables:
#' \itemize{
#'    \item valor: price, in brazilian Reais
#'    \item tipo: type of property -
#'     casa, apartamento, kitinete, terreno, comercial
#'     (i.e, house, apartament, kitchenette, land, commercial)
#'    \item area_total: Total Area, in squared meters
#'    \item area_terreno: Ground Area, in squared meters (houses only)
#'    \item garagens: Garages
#'    \item novo: boolean for brand new apartments -
#'     novo, usado (TRUE, FALSE)
#'    \item padrao: Building Standard -
#'     baixo, medio, alto (i.e. low, normal, high)
#' }
#' @source \strong{Trivelloni, Carlos Alberto Peruzzo}. 
#' \emph{Metodo para determinacao do valor da localizacao com uso de tecnicas 
#' inferenciais e geoestatisticas na avaliacao em massa de imoveis}. 
#' Florianopolis, 2005.
"trivelloni_2005"

#' Land division data
#'
#' A tibble containing a sample of 20 plots in subdivision in Florianopolis. 
#' Paradigm situation: dry, flat, 15m front width and 30~60m length.
#'
#' @format A tibble with 20 rows and 8 variables:
#' \itemize{
#'   \item valor: price, in brazilian Reais
#'   \item area: land area in squared meters
#'   \item tipo: type: offer or sale -
#'   venda, oferta (i.e. sale, offer)
#'   \item frente: front width of the land in meters
#'   \item profundidade: length of the land in meters
#'   \item topo: topography -
#'   plano, aclive (i.e. flat, slope)
#'   \item inclinacao: slope
#'   \item pedologia: pedology -
#'   seco, pantanoso (i.e. dry, marshy)
#' }
#' @source \strong{HOCHHEIM, Norberto}. \emph{Engenharia de Avaliacoes I}. 
#' Florianopolis: IBAPE/SC, 2005, p.74
"loteamento"

#' Income
#'
#' A tibble containing a sample of 7 apartments to be used to appraise by the 
#' Income Method.
#'
#' @format a tibble with 7 rows and 5 variables:
#' \itemize{
#'   \item area_total: Total Area, in squared meters
#'   \item valor_aluguel: Rental values
#'   \item valor_venda: Sale Value
#'   \item vacancia: Vacancy
#'   \item condominio: condominium fee
#' }
#' @source \strong{HOCHHEIM, Norberto}. \emph{Engenharia de Avaliacoes I}. 
#' Florianopolis: IBAPE/SC, 2005, p.99
"renda"

#' Prices of 15 plots in Canasvieiras (1997)
#' 
#' A tibble containing a sample of 15 unitary values of plots sales prices per 
#' squared meters.
#' @format a tibble with 15 rows and 5 columns
#' \itemize{
#'   \item endereco address.
#'   \item valor_unitario unitary value per squared meter.
#'   \item area area in squared meters.
#'   \item dist_b_mar distance to the sea.
#'   \item zoneamento zone.
#' }
#' @source \strong{HOCHHEIM, Norberto}. \emph{Engenharia de Avaliacoes II}. 
#' Florianopolis: IBAPE/SC, 2005, p.3.4
"canasvieiras_97"


"trindade_ap"

#' Prices of 48 plots at the beach
#' 
#' A tibble containing a sample of 48 unitary values of plots sales prices per 
#' squared meters.
#' @format a tibble with 48 rows and 8 columns
#' \itemize{
#'   \item valor_unitario unitary value per squared meter.
#'   \item area area in squared meters.
#'   \item testada plot front in meters.
#'   \item profundidade land deep in meters.
#'   \item pedologia pedology.
#'   \item frente_mar front of the beach (yes (1) or no (0))
#'   \item dist_b_mar distance to the sea.
#'   \item zoneamento zone.
#' }
#' @source \strong{HOCHHEIM, Norberto}. \emph{Engenharia de Avaliacoes II}. 
#' Florianopolis: IBAPE/SC, 2005, p.E2.2
"terrenos_praia"

#' Prices of 15 Florianopolis' apartaments with 2 rooms in Trindade neighbourhood
#'
#' A tibble containing a sample of 15 unitary values of apartment sales prices 
#' per squared meters.
#' @format a tibble with 15 rows and 1 variable:
#' \itemize{
#'   \item VU unitary value per squared meter.
#' }
#' @source \strong{HOCHHEIM, Norberto}. \emph{Engenharia de Avaliacoes I}. 
#' Florianopolis: IBAPE/SC, 2005, p.18
"trindade"

#' Parametrized optimals costs percentages per period according to Jungles and 
#' Ávila (2009)
#' 
#' A tibble containing optimal costs percentages for each period for projects 
#' with 1 to 24 periods.
#' @format a tibble with 24 rows and 25 columns
#' 
#' @source \strong{JUNGLES, A. E.; AVILA, A. V.}. \emph{Gestao do controle e 
#' planejamento de empreendimentos}. 2009.
"jungles"
