library(readr)
library(purrr)
library(dplyr)
library(tidyr)

# Lee carpeta donde están los datos originales
readRenviron(".projenv")

# Tabla con nombres de ficheros por año y tipo
files <- read_csv("data-raw/files.csv", col_types = "icc")

# Obtiene la ruta a un archivo de datos de un año y tipo
get_file_path <- function(year, type) {
  idx <- files$year == year & files$type == type
  file.path(Sys.getenv("ECV_FOLDER"), year, files[idx, "filename"])
}

# Asegura que existe el directorio de datos
if (!dir.exists("data")) {
  dir.create("data")
}

# Obtiene los nombres de las variables de un fichero de datos
get_vars <- function(year, type) {
  path <- get_file_path(year, type)
  varnames <- read_csv(path, col_types = "c",
                       n_max = 1, col_names = FALSE) |>
    as.character()
  tibble(year = year, type = type, var = varnames)
}

# Variables en cada fichero y año de la ECV
vars_by_year <- map2(files$year, files$type, get_vars) |>
  list_rbind()
save(vars_by_year, file = "data/vars-by-year.Rdata", compress = "xz")

get_na <- function(type) {
  x <- switch(type,
              "c" = character(),
              "i" = integer(),
              "d" = double(),
              NULL
  )
  is.na(x) <- TRUE
  x
}

# Lee un fichero de datos de un año y tipo dados
read_ecv_file <- function(year, type) {
  # Obtiene la ruta al archivo de datos
  idx <- files$year == year & files$type == type
  file_path <-
    file.path(Sys.getenv("ECV_FOLDER"), year, files[idx, "filename"])

  # Lee nombres y tipos de las variables de un tipo de ficheros
  all_vars <- read_csv(file.path("data-raw", paste0(type, ".csv")),
                       col_types = "cc", comment = "#")

  # Nombres de las variables disponibles en un determinados año
  vars_in_year <- vars_by_year |>
    filter(year == .env$year, type == .env$type) |>
    pull(var)

  # Variables disponibles en un año
  vars_available <- all_vars |>
    filter(var %in% vars_in_year)

  # Variables no disponibles en un año
  vars_missing <- all_vars |>
    filter(!var %in% vars_in_year)

  # Lee los datos
  columns <- setNames(as.list(vars_available$type),
                      vars_available$var)
  db <- read_csv(file_path, col_types = do.call(cols_only, columns))

  # Añade variables no disponibles
  if (nrow(vars_missing) > 0) {
    db_missing <- pivot_wider(vars_missing,
                              names_from = var,
                              values_from = type,
                              values_fn = get_na)
    db <- list_cbind(list(db, db_missing))
  }

  db
}

# Lectura de los ficheros de datos
years <- 2004:2024

# Datos básicos de los hogares
ecv_d <- map(years, \(x) read_ecv_file(x, "d")) |>
  list_rbind()
save(ecv_d, file = "data/ecv_d.Rdata", compress = "xz")

# Datos básicos de las personas
ecv_r <- map(years, \(x) read_ecv_file(x, "r")) |>
  list_rbind()
save(ecv_r, file = "data/ecv_r.Rdata", compress = "xz")

# Datos detallados de los hogares
ecv_h <- map(years, \(x) read_ecv_file(x, "h")) |>
  list_rbind()
save(ecv_h, file = "data/ecv_h.Rdata", compress = "xz")

# Datos detallados de los adultos
ecv_p <- map(years, \(x) read_ecv_file(x, "p")) |>
  list_rbind()
save(ecv_p, file = "data/ecv_p.Rdata", compress = "xz")
