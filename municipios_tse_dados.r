library(tidyverse)
library(sf)
library(jsonlite)

urlm <- "https://resultados.tse.jus.br/oficial/ele2022/544/config/mun-e000544-cm.json"
muni_tse <- fromJSON(urlm,
  simplifyDataFrame = TRUE
) %>%
  .[["abr"]] %>%
  unnest("mu", names_repair = "universal") %>%
  select(-c, -z) %>%
  set_names("uf", "estado", "tse", "ibge7", "nm")

muni_tse$ibge7 <- as.integer(muni_tse$ibge7)

muni_tse_sp <- muni_tse %>%
  filter(uf == "SP")


# DDD
urlddd <- "https://raw.githubusercontent.com/diegoheyy/cidades-ao-redor/31b8349b67eb2ca7a88c42af68418fdc4364e3e8/src/data/ddd.json"
ddd <- fromJSON(urlddd,
  simplifyDataFrame = TRUE
) %>%
  unnest("ddd", names_repair = "universal") %>%
  set_names("ibge", "ddd")

# Filtrar municipios de SP com DDD 18
muni_tse_sp_ddd <- muni_tse_sp %>%
  inner_join(ddd, by = c("ibge7" = "ibge")) %>%
  filter(ddd == "18")

write.csv(muni_tse_sp_ddd, "municipios_ddd18.csv", row.names = FALSE)

# Dados das eleições dos municipios
tse_mun_2022 <- paste0(
  "https://resultados.tse.jus.br/oficial/ele2022/544/dados/", str_to_lower(muni_tse_sp_ddd$uf),
  "/", str_to_lower(muni_tse_sp_ddd$uf), muni_tse_sp_ddd$tse, "-c0001-e000544-v.json"
)

rate <- rate_backoff(pause_base = 0.1, pause_min = 0.005, max_times = 100)
fs <- function(x, y) {
  x <- fromJSON(x, simplifyDataFrame = T)
  print(y)
  return(x)
}

insistent <- insistently(fs,
  rate,
  quiet = FALSE
)

resultado_mun_2022 <- imap(tse_mun_2022, insistent)

final_2022 <- map_df(resultado_mun_2022, function(x) {
  x %>%
    .[["abr"]] %>%
    tbl_df() %>%
    filter(tpabr == "MU") %>%
    unnest(cand, names_repair = "universal") %>%
    select(cdabr, vap, pvap, n)
})

write.csv(final_2022, "resultado_mun_2022_ddd18.csv", row.names = FALSE)
