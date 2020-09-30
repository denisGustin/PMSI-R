
library(tidyverse)
library(refpmsi)
library(gt)
library(paletteer)

jeu_week_39 <- readRDS("jeu_week_39.rds")

# jeu de données = jeu_week_39
# jeu_week_39 %>% glimpse()
# Rows: 10
# Columns: 15
# $ ghm       <chr> "02C05J", "11C111", "11M02T", "06M031", "05M13T", "05M093", ...
# $ n_rss     <int> 113, 62, 42, 39, 39, 26, 30, 30, 28, 25
# $ n_rum_ghm <int> 113, 66, 42, 41, 39, 30, 30, 30, 28, 25
# $ CAMB      <dbl> 100.00, 1.52, 0.00, 0.00, 0.00, 0.00, 100.00, 0.00, 17.86, 1...
# $ REAN      <dbl> 0.00, 0.00, 0.00, 0.00, 2.56, 0.00, 0.00, 0.00, 0.00, 0.00
# $ URGE      <dbl> 0.00, 4.55, 59.52, 2.44, 61.54, 13.33, 0.00, 0.00, 0.00, 0.00
# $ MCAR      <dbl> 0.00, 0.00, 0.00, 4.88, 33.33, 56.67, 0.00, 0.00, 0.00, 0.00
# $ CORT      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
# $ MGER      <dbl> 0.00, 0.00, 0.00, 0.00, 2.56, 20.00, 0.00, 3.33, 0.00, 0.00
# $ MPNE      <dbl> 0.00, 1.52, 0.00, 7.32, 0.00, 3.33, 0.00, 0.00, 0.00, 0.00
# $ MGAS      <dbl> 0.00, 0.00, 0.00, 75.61, 0.00, 6.67, 0.00, 96.67, 0.00, 0.00
# $ CORL      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
# $ CURO      <dbl> 0.00, 92.42, 38.10, 0.00, 0.00, 0.00, 0.00, 0.00, 82.14, 0.00
# $ CDIG      <dbl> 0.00, 0.00, 2.38, 9.76, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00

# enrichissement avec les libellés de GHM
# via le package refpmsi::
jeu_week_39 <- jeu_week_39 %>%
  dplyr::left_join(refpmsi("ghs_dgf",2020) %>%
                     dplyr::select(ghm,libelle) %>% unique,
                   by = c("ghm" = "ghm")) %>%
  dplyr::relocate(libelle, .after = ghm)

# création du tableau avec le package gt::
jeu_week_39_gt <- jeu_week_39 %>%
  # mise en évidence de la colonne des GHM
  gt::gt(rowname_col = "ghm") %>%
  # regroupement des UM
  # on ne regroupe pas l'UM URGE
  gt::tab_spanner(label = "Médecine",
                  columns = c("MCAR","MPNE","MGAS","MGER")) %>%
  gt::tab_spanner(label = "Chirurgie",
                  columns = c("CAMB","CORT","CORL","CURO","CDIG","REAN")) %>%
  # formattage de l'affichage des proportions de rum par UM
  gt::fmt_number(
    columns = 5:last_col(),
    drop_trailing_zeros = TRUE,
    dec_mark = ",",
  ) %>%
  gt::fmt_percent(
    columns = 5:last_col(),
    rows = 1,
    scale_values = FALSE,
    dec_mark = ",",
    drop_trailing_zeros = TRUE
  ) %>%
  # affichage en gras de la colonne des urgences
  gt::tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(vars(URGE))
  ) %>%
  # affichage en petit et en italique de la colonne des libellés des GHM
  gt::tab_style(
    style = list(
      cell_text(style = "italic",
                size = "small")
    ),
    locations = cells_body(
      columns = vars(libelle))
  ) %>%
  # repositionnement de la colonne des urgences en 1ere position des colonnes des UM
  gt::cols_move(vars(URGE), after = vars(n_rum_ghm)) %>%
  # alignement à droite des données
  gt::cols_align(align = "right",
                 columns = 3:last_col()) %>%
  # trait noir épais pour séparer les données des UM des autres colonnes
  gt::tab_style(
    style = list(
      cell_borders(
        sides = "left",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = vars(URGE)
      )
    )
  ) %>%
  # renommage des colonnes
  gt::cols_label(
    libelle = "",
    n_rss = "RSS",
    n_rum_ghm = "RUM",
    URGE = "Urgences",
    CAMB = "Ambu",
    CORT = "Ortho",
    CORL = "ORL",
    CURO = "Uro",
    CDIG = "Dig",
    REAN = "Réa",
    MCAR = "Cardio",
    MPNE = "Pneumo",
    MGAS = "Gastro",
    MGER = "Gériatrie"
  ) %>%
  # alignement à gauche des intitulés de colonnes
  gt::tab_style(
    style = list(
      cell_text(weight = "bold",
                align = "left")
    ),
    locations = cells_column_spanners(gt::everything())
  ) %>%
  # dégradé de couleurs de 0 à 100% en fonction des valeurs des proportions de GHM par UM
  # avec le package paletteer::
  gt::data_color(
    columns = 5:last_col(),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::red_material"
      ) %>% as.character(),
      domain = c(0,100)
    ),
    alpha = 0.8
  ) %>%
  # paramétrage des bordures
  gt::tab_options(
    column_labels.border.top.color = "white",
    column_labels.border.top.width = px(3),
    column_labels.border.bottom.color = "black",
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table_body.hlines.color = "#ededed"
  ) %>%
  # source du tableau
  gt::tab_source_note(md("*Dataset : Datamis MCO*")) %>%
  # titre et sous-titre
  gt::tab_header(
    title = md("**Répartition des GHM par UM**"),
    subtitle = "Pour les 10 premiers GHM représentés en RUM"
  ) %>%
  # aligement à gauche du titre et du sous-titre
  gt::opt_align_table_header(align = "left") %>%
  # ajout d'une note sur le calcul des séjours par GHM
  gt::tab_footnote(
    footnote = "RSS sélectionné si au moins 1 RUM du séjour classé dans le GHM",
    locations = cells_column_labels(
      columns = vars(n_rss)
    )
  )

jeu_week_39_gt
