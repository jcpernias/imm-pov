library(tidyverse)

dummy <- function(x) {
  x + 0L
}

# Enlaza las bases de datos de hogares
households <- local({
  # Lee datos ECV
  load("data/ecv_d.Rdata")
  load("data/ecv_h.Rdata")

  # Enlaza las bases de datos con el año (DB010 == HB010) y
  # el identificador de las familias (DB030 == HB030)
  left_join(ecv_d, ecv_h,
            by = join_by(DB010 == HB010, DB030 == HB030))
})


# Reemplaza los códigos ECV de las regiones por abreviaturas de
# los nombres de las comunidades y ciudades autónomas.
households <- local({
  reg_codes <- read_csv("data-raw/reg.csv", col_types = "cc") |>
    mutate(value = as_factor(value))

  reg_db <- households |>
    select(DB010, DB030, DB040) |>
    left_join(reg_codes, by = join_by(DB040 == code)) |>
    select(-DB040) |>
    rename(region = value)
  left_join(households, reg_db,
            by = join_by(DB010, DB030))
})

# Lee los datos de individuos
load("data/ecv_r.Rdata")

# Datos básicos de los individuos
individuals <- ecv_r |>
  transmute(ecv_year = RB010,
            indiv_id = RB030,
            hh_id = floor(indiv_id / 100),
            indiv_factor = RB050,
            birth_year = RB080,
            age = ecv_year - birth_year,
            female = dummy(RB090 == 2),
            low_job = if_else(vrLOWJOB == 9, NA_integer_, vrLOWJOB),
            eu2020 = vrEU2020)

# Lee los datos de adultos
load("data/ecv_p.Rdata")

# Los individuos que no aparecen en el fichero de adultos
# son niños
individuals <- local({
  children <-
    anti_join(ecv_r, ecv_p,
              by = join_by(RB010 == PB010, RB030 == PB030)) |>
    select(ecv_year = RB010, indiv_id = RB030) |>
    mutate(child = 1)
  left_join(individuals, children, by = join_by(ecv_year, indiv_id)) |>
    mutate(child = coalesce(child, 0L))
})

# Datos de los adultos
#
# # País de nacimiento: PB210 (hasta 2020) y RB2080 (desde 2021)
# Nacionalidad: PB220A (hasta 2020) y RB2090 (desde 2021)
#
# Nivel de estudios en curso: PE020 (hasta 2020) y PE021 (desde 2021)
# Nivel de estudios terminados: PE040 (hasta 2020) y PE041 (desde 2021)
#
adults <- ecv_p |>
  left_join(ecv_r, by = join_by(PB010 == RB010, PB030 == RB030)) |>
  transmute(
    ecv_year = PB010,
    indiv_id = PB030,
    hh_id = floor(indiv_id / 100),
    adult_factor = PB040,
    indiv_factor = RB050,
    birth_year = RB080,
    age = ecv_year - birth_year,
    female = dummy(RB090 == 2),
    low_job = if_else(vrLOWJOB == 9, NA_integer_, vrLOWJOB),
    eu2020 = vrEU2020,
    partner = dummy(PB190 == 2 | PB200 == 1 | PB200 == 2),
    bad_health = dummy(PH010 == 4 | PH010 == 5),
    country_es = dummy(if_else(ecv_year < 2021, PB210 == 1, RB280 == 1)),
    country_eu = dummy(if_else(ecv_year < 2021, PB210 == 2, RB280 == 2)),
    country_other = dummy(if_else(ecv_year < 2021, PB210 > 2, RB280 > 2)),
    nation_es = dummy(if_else(ecv_year < 2021, PB220A == 1, RB290 == 1)),
    nation_eu = dummy(if_else(ecv_year < 2021, PB220A == 2, RB290 == 2)),
    nation_other = dummy(if_else(ecv_year < 2021, PB220A > 2, RB290 > 2)),
    educ = case_when(ecv_year < 2014 & PE040_F == -2 ~ 0,
                     ecv_year < 2014 ~ PE040,
                     ecv_year < 2021 ~ floor(PE040 / 100),
                     ecv_year >= 2021 ~ floor(PE041 / 100)),
    educ_none = dummy(educ == 0),
    educ_prim = dummy(educ == 1),
    educ_sec_lo = dummy(educ == 2),
    educ_sec_hi = dummy(educ == 3 | educ == 4),
    educ_sup = dummy(educ == 5),
    never_worked = if_else(ecv_year < 2021,
                           dummy(PL015 == 2), dummy(PL016 == 1)),
    exper = if_else(never_worked == 1, 0, PL200),
    white_collar =
      case_when(ecv_year < 2011 ~ dummy(between(PL050, 10, 39)),
                ecv_year < 2021 ~ dummy(between(PL051, 10, 39)),
                ecv_year >= 2021 ~
                  dummy(between(coalesce(PL051A, PL051B), 10, 39))),
  )

# Actividad principal por meses (hasta 2008 / desde 2009):
# - Enero: PL210A / PL211A
# - Febrero: PL210B / PL211B
# - Marzo: PL210C / PL211C
# - Abril: PL210D / PL211D
# - Mayo: PL210E / PL211E
# - Junio: PL210F / PL211F
# - Julio: PL210G / PL211G
# - Agosto: PL210H / PL211H
# - Septiembre: PL210I / PL211I
# - Octubre: PL210J / PL211J
# - Noviembre: PL210K / PL211K
# - Diciembre: PL210L / PL211L
#
# Códigos de actividad (hasta 2008):
# 1   Asalariado (tiempo completo)
# 2   Asalariado (tiempo parcial)
# 3   Trabajador por cuenta propia (tiempo completo)
# 4   Trabajador por cuenta propia (tiempo parcial)
# 5   Parado
# 6   Jubilado o retirado
# 7   Estudiante, escolar o en formación
# 8   Otro tipo de inactividad económica
# 9   Servicio militar obligatorio
#
# Códigos de actividad (a partir de 2009):
# 1	  Asalariado (tiempo completo)
# 2	  Asalariado (tiempo parcial)
# 3	  Trabajador por cuenta propia (tiempo completo)
# 4	  Trabajador por cuenta propia (tiempo parcial)
# 5	  Parado
# 6	  Estudiante, escolar o en formación
# 7	  Jubilado o retirado
# 8	  Incapacitado permanente para trabajar
# 10	Dedicado a las labores del hogar, cuidado de niños, etc
# 11	Otro tipo de inactividad económica

activity <- ecv_p |>
  mutate(
    m01 = if_else(PB010 < 2009, PL210A, PL211A),
    m02 = if_else(PB010 < 2009, PL210B, PL211B),
    m03 = if_else(PB010 < 2009, PL210C, PL211C),
    m04 = if_else(PB010 < 2009, PL210D, PL211D),
    m05 = if_else(PB010 < 2009, PL210E, PL211E),
    m06 = if_else(PB010 < 2009, PL210F, PL211F),
    m07 = if_else(PB010 < 2009, PL210G, PL211G),
    m08 = if_else(PB010 < 2009, PL210H, PL211H),
    m09 = if_else(PB010 < 2009, PL210I, PL211I),
    m10 = if_else(PB010 < 2009, PL210J, PL211J),
    m11 = if_else(PB010 < 2009, PL210K, PL211K),
    m12 = if_else(PB010 < 2009, PL210L, PL211L)) |>
  select(ecv_year = PB010, indiv_id = PB030, m01:m12) |>
  pivot_longer(cols = m01:m12, names_to = "month", values_to = "code") |>
  group_by(ecv_year, indiv_id) |>
  summarise(wage_full = sum(code == 1, na.rm = TRUE),
            wage_part = sum(code == 2, na.rm = TRUE),
            self_full = sum(code == 3, na.rm = TRUE),
            self_part = sum(code == 4, na.rm = TRUE),
            unem = sum(code == 5, na.rm = TRUE),
            retired = sum(if_else(ecv_year < 2009,
                                  code == 6, code == 7),
                          na.rm = TRUE),
            inactive = sum(if_else(ecv_year < 2009,
                                   code %in% 7:9, code %in% c(6, 8:11)),
                           na.rm = TRUE),
            total = sum(!is.na(code)),
            .groups = "drop")

# Identifica el responsable de cada familia
hh_head <- households |>
  select(ecv_year = DB010, hh_id = DB030, head_id = HB080) |>
  left_join(adults, by = join_by(ecv_year, hh_id, head_id == indiv_id)) |>
  left_join(activity, by = join_by(ecv_year, head_id == indiv_id)) |>
  mutate(
    wage_full = wage_full / 12,
    wage_part = wage_part / 12,
    self_full = self_full / 12,
    self_part = self_part / 12,
    unem = unem / 12,
    retired = retired / 12,
    inactive = inactive / 12,
    total = total / 12) |>
  select(-adult_factor) |>
  rename_with(\(x) paste0("head_", x), -c(ecv_year:head_id))

# Miembros de los hogares
hh_by_age <-
  individuals |>
  group_by(ecv_year, hh_id) |>
  summarise(
    hh_mean_age = mean(age),
    hh_n_lt5 = sum(age < 5),
    hh_n_5_14 = sum(age >= 5 & age < 15),
    hh_n_ge_65 = sum(age >= 65),
    .groups = "drop")

# Educación de los miembros del hogar
hh_adult_sums <-
  adults |>
  group_by(ecv_year, hh_id) |>
  summarise(
    hh_n_female = sum(female),
    hh_educ_none = sum(educ_none),
    hh_educ_prim = sum(educ_prim),
    hh_educ_sec_lo = sum(educ_sec_lo),
    hh_educ_sec_hi = sum(educ_sec_hi),
    hh_educ_sup = sum(educ_sup),
    hh_bad_health = sum(bad_health),
    hh_never_worked = sum(never_worked),
    hh_white_collar = sum(white_collar),
    .groups = "drop")

# Actividad de los miembros del hogar
hh_activity <-
  activity |>
  mutate(hh_id = floor(indiv_id / 100)) |>
  group_by(ecv_year, hh_id) |>
  summarise(
    hh_wage_full = sum(wage_full) / 12,
    hh_wage_part = sum(wage_part) / 12,
    hh_self_full = sum(self_full) / 12,
    hh_self_part = sum(self_part) / 12,
    hh_unem = sum(unem) / 12,
    hh_retired = sum(retired) / 12,
    hh_inactive = sum(inactive) / 12,
    hh_total = sum(total) / 12,
    .groups = "drop")

hh_house <- households |>
  mutate(
    ecv_year = DB010,
    hh_id = DB030,
    hh_house_own = case_when(ecv_year <= 2010 ~ dummy(HH020 == 1),
                             ecv_year > 2010 ~ dummy(between(HH021, 1, 2))),
    hh_house_rent = case_when(ecv_year <= 2010 ~ dummy(between(HH020, 2, 3)),
                              ecv_year > 2010 ~ dummy(between(HH021, 3, 4))),
    hh_house_free = case_when(ecv_year <= 2010 ~ dummy(HH020 == 4),
                              ecv_year > 2010 ~ dummy(HH021 == 5)),
    hh_mortgage = case_when(ecv_year <= 2007 ~
                              dummy(hh_house_own == 1 & HS010_F != -2),
                            ecv_year <= 2010 ~
                              dummy(hh_house_own == 1 & HS011_F != -2),
                            ecv_year > 2010 ~ dummy(HH021 == 1)),
  ) |>
  select(ecv_year, hh_id, hh_house_own, hh_house_rent, hh_house_free,
         hh_mortgage)

# Renta y características de los hogares
hhincome <- households |>
  mutate(ecv_year = DB010,
         hh_id = DB030,
         hh_weight = DB090,
         hh_size = HX040,
         hh_cunits = HX240,
         hh_type = factor(sprintf('HH_%02d', HX060),
                          levels = sprintf('HH_%02d', 1:14)),
         hh_tr = if_else(HY022_F != 0, HY020 - HY022, 0),
         hh_trp = if_else(HY022_F != 0, HY020 - HY023, 0),
         hh_inc = vhRentaa,
         hh_region = region,
         hh_urb_hi = dummy(DB100 == 1),
         hh_urb_lo = dummy(DB100 == 3),
         ) |>
  select(ecv_year, hh_id, hh_weight,
         hh_region, hh_urb_hi, hh_urb_lo,
         hh_size, hh_cunits, hh_type,
         hh_inc, hh_tr, hh_trp) |>
  left_join(hh_house, by = join_by(ecv_year, hh_id)) |>
  left_join(hh_adult_sums, by = join_by(ecv_year, hh_id)) |>
  left_join(hh_by_age, by = join_by(ecv_year, hh_id)) |>
  left_join(hh_head, by = join_by(ecv_year, hh_id)) |>
  left_join(hh_activity, by = join_by(ecv_year, hh_id))


# Guarda la base de datos
save(hhincome, file = "data/hhincome.Rdata", compress = "xz")
