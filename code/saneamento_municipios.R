library(tidyr)
library(dplyr)
library(sidrar)
library(ggplot2)

##Construção da tabela =========================================================

dados <- get_sidra(api="/t/5938/n6/all/p/2021/v/37,498,513,517,6575,525") %>%
  select(
    `Valor`,
    `Município`,
    `Variável`,
    `Município (Código)`
  ) %>%
  pivot_wider(
    names_from = `Variável`,
    values_from = `Valor`
  ) %>%
  rename(
    "PIB" = "Produto Interno Bruto a preços correntes",
    "VAB" = "Valor adicionado bruto a preços correntes total",
    "VAB agro" = "Valor adicionado bruto a preços correntes da agropecuária",
    "VAB indústria" = "Valor adicionado bruto a preços correntes da indústria",
    "VAB serviços" = "Valor adicionado bruto a preços correntes dos serviços, exclusive administração, defesa, educação e saúde públicas e seguridade social",
    "VAB gov" = "Valor adicionado bruto a preços correntes da administração, defesa, educação e saúde públicas e seguridade social") %>%
  left_join(
    get_sidra(api="/t/4709/n6/all/p/2022/v/93") %>%
      select(
        `Valor`,
        `Município (Código)`
      ),
    by = "Município (Código)"
  ) %>%
  rename(População = Valor) %>%
  left_join(
    get_sidra(api = "/t/6803/n6/all/p/2022/v/381/c1821/72129,72144") %>%
      select(
        `Valor`,
        `Existência de ligação à rede geral de distribuição de água e principal forma de abastecimento de água`,
        `Município (Código)`
      ) %>%
      pivot_wider(
        names_from = `Existência de ligação à rede geral de distribuição de água e principal forma de abastecimento de água`,
        values_from = `Valor`
      ) %>%
      rename("Número de domicílios" = Total,
             "Domicílios água" = "Possui ligação à rede geral e a utiliza como forma principal"),
    by = "Município (Código)"
  ) %>%
  left_join(
    get_sidra(api = "/t/6805/n6/all/p/2022/v/381/c11558/72110,46292") %>%
      select(
        `Valor`,
        `Tipo de esgotamento sanitário`,
        `Município (Código)`
      ) %>%
      pivot_wider(
        names_from = `Tipo de esgotamento sanitário`,
        values_from = `Valor`
      ) %>%
      rename("Domicílios esgotamento" = "Rede geral ou pluvial",
             "TotalEsg" = Total),
    by = "Município (Código)"
  ) %>%
  left_join(
    get_sidra(api = "/t/6892/n6/all/p/2022/v/381/c67/2520,10972") %>%
      select(
        `Valor`,
        `Destino do lixo`,
        `Município (Código)`
      ) %>%
      pivot_wider(
        names_from = `Destino do lixo`,
        values_from = `Valor`
      ) %>%
      rename("Domicílios lixo" = Coletado,
             "TotalLixo" = Total),
    by = "Município (Código)"
  ) %>%
  mutate(
    UF = substr(`Município`, nchar(`Município`) - 1, nchar(`Município`)),
    Município = substr(`Município`, 1, nchar(`Município`) - 4)
  ) %>%
  relocate(
    `UF`,
    .before=`Município`
  ) %>%
  mutate(
    Região = case_when(
      UF %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO") ~ "Norte",
      UF %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
      UF %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
      UF %in% c("ES", "MG", "RJ", "SP") ~ "Sudeste",
      UF %in% c("PR", "RS", "SC") ~ "Sul"
    )
  ) %>%
  relocate(
    `Região`,
    .before = `UF`
  )

# Tanto coleta de lixo, como abastecimento de água e a coleta de esgoto possuem um dado de número de domicílios que é igual.
# Aqui garantimos a igualdade e excluímos as colunas redundantes.
if(all(dados$`TotalEsg` == dados$`Número de domicílios`) && all(dados$`TotalLixo` == dados$`Número de domicílios`)){
  dados <- dados %>%
    select(
      -`TotalEsg`,
      -`TotalLixo`
      )
}

##Razões, proporções etc. ======================================================

dados <- dados %>%
  mutate(
    "pc água" = `Domicílios água`/`Número de domicílios`,
    "pc esgotamento" = `Domicílios esgotamento`/`Número de domicílios`,
    "pc lixo" = `Domicílios lixo`/`Número de domicílios`,
    "PIB pc" = `PIB`/`População`,
    "pc VAB agro" = `VAB agro`/`VAB`,
    "pc VAB indústria" = `VAB indústria`/`VAB`,
    "pc VAB serviços" = `VAB serviços`/`VAB`,
    "pc VAB gov" = `VAB gov`/`VAB`,
    "Especialização" = case_when(
      `pc VAB agro` == pmax(`pc VAB agro`, `pc VAB indústria`, `pc VAB gov`, `pc VAB serviços`) ~ "Agro",
      `pc VAB indústria` == pmax(`pc VAB agro`, `pc VAB indústria`, `pc VAB gov`, `pc VAB serviços`) ~ "Indústria",
      `pc VAB gov` == pmax(`pc VAB agro`, `pc VAB indústria`, `pc VAB gov`, `pc VAB serviços`) ~ "Gov",
      `pc VAB serviços` == pmax(`pc VAB agro`, `pc VAB indústria`, `pc VAB gov`, `pc VAB serviços`) ~ "Serviços"
    )
  )

##Gráficos e análises ==========================================================

##Histogramas dos percentuais municipais de cobertura dos serviços

ggplot(dados, aes(x = `pc água`)) +
  geom_histogram(bins = 75) +
  labs(
    title = "Percentual de domincílios água",
    x = "%",
    y = "Frequência"
  )
  
ggplot(dados, aes(x = `pc esgotamento`)) +
  geom_histogram(bins = 75) +
  labs(
    title = "Percentual de domincílios esgotamento",
    x = "%",
    y = "Frequência"
  )

  
ggplot(dados, aes(x = `pc lixo`)) +
  geom_histogram(bins = 75) +
  labs(
    title = "Percentual de domincílios esgotamento",
    x = "%",
    y = "Frequência"
  )

#Histograma por região
estat_esgoto <- dados %>%
  group_by(Região) %>%
  summarise(
    média = mean(`pc esgotamento`, na.rm = TRUE),
    mediana = median(`pc esgotamento`, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(dados, aes(x = `pc esgotamento`)) +
  geom_histogram(bins = 50, fill = "grey70", color = "white") +
  geom_vline(
    data = estat_esgoto,
    aes(xintercept = média, color = "Média"),
    linewidth = 1
  ) +
  geom_vline(
    data = estat_esgoto,
    aes(xintercept = mediana, color = "Mediana"),
    linewidth = 1,
    linetype = "dashed"
  ) +
  facet_wrap(~ Região, ncol = 3) +
  scale_color_manual(
    name = "Estatística",
    values = c("Média" = "red", "Mediana" = "blue")
  ) +
  labs(
    title = "Distribuição do percentual de esgotamento por região",
    x = "Domicílios com esgotamento sanitário (%)",
    y = "Número de municípios"
  ) +
  theme_minimal()

#Histograma por especialização
estat_esgoto_setor <- dados %>%
  group_by(`Especialização`) %>%
  summarise(
    média = mean(`pc esgotamento`, na.rm = TRUE),
    mediana = median(`pc esgotamento`, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(dados, aes(x = `pc esgotamento`)) +
  geom_histogram(bins = 50, fill = "grey70", color = "white") +
  geom_vline(
    data = estat_esgoto_setor,
    aes(xintercept = média, color = "Média"),
    linewidth = 1
  ) +
  geom_vline(
    data = estat_esgoto_setor,
    aes(xintercept = mediana, color = "Mediana"),
    linewidth = 1,
    linetype = "dashed"
  ) +
  facet_wrap(~ `Especialização`, ncol = 2) +
  scale_color_manual(
    name = "Estatística",
    values = c("Média" = "red", "Mediana" = "blue")
  ) +
  labs(
    title = "Distribuição do percentual de esgotamento por especialização",
    x = "Domicílios com esgotamento sanitário (%)",
    y = "Número de municípios"
  ) +
  theme_minimal()

##Regressão

dados
modelo1 <- lm(log(`Domicílios esgotamento`)~log(`PIB pc`)+log(`População`), data=dados)
summary(modelo1)

ggplot(dados, aes(x = log(`PIB`) + log(`População`), y = log(`Domicílios esgotamento`)))+
  geom_point()+
  geom_smooth(method="lm")

modelo2 <- lm(`pc esgotamento`~log(`VAB`), data=dados)
summary(modelo2)

modelo3 <- lm(
  `pc esgotamento` ~
    log(`VAB`) +
    I(`VAB agro`/`VAB`) +
    I(`VAB indústria`/`VAB`),
  data = dados
)

summary(modelo3)

ggplot(dados, aes(x = log(`PIB`), y = `pc esgotamento`))+
  geom_point()+
  geom_smooth(method="lm")

lm(log(`Domicílios esgotamento`)~log(`VAB`), data=dados)

lm(log(`Domicílios esgotamento`)~log(`VAB indústria`), data=dados)

lm(log(`Domicílios esgotamento`)~log(`VAB agro`), data=dados)