library(tidyverse)

# cdabr = codigo tse
# vap = votos
# pvap = percentual
# n = numero do candidato
dados <- read.csv("resultado_mun_2022_ddd18.csv", header = TRUE, sep = ",")

# Ajustar dados
dados$vap <- as.integer(dados$vap)


# dados <- dados %>%
#     filter(cdabr == "61557") %>%
#     select(cdabr, n, vap, pvap)


# Agrupar dados por numero do candidato
dados_agrupados <- dados %>%
    group_by(n) %>%
    summarise(
        vap = sum(vap)
    ) %>%
    arrange(desc(vap))

# Calculo do percentual
dados_agrupados$pvap <- dados_agrupados$vap / sum(dados_agrupados$vap) * 100

total_votos <- sum(dados_agrupados$vap)

print(dados_agrupados)
write.csv(dados_agrupados, "resultado_mun_2022_ddd18_agrupado.csv", row.names = FALSE)
