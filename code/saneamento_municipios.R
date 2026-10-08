library(tidyr)
library(dplyr)
library(sidrar)
library(ggplot2)

##Construção da tabela =========================================================

dados <- get_sidra(api="/t/5938/n6/all/p/2021/v/37,498,513,517,6575") %>%
  select(
    `Valor`,
    `Município`,
    `Variável`
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
    "VAB serviços" = "Valor adicionado bruto a preços correntes dos serviços, exclusive administração, defesa, educação e saúde públicas e seguridade social") %>%
  left_join(
    get_sidra(api="t/4709/n6/all/p/2022/v/93") %>%
      select(
        `Município`,
        `Valor`
      ),
    by = "Município"
  ) %>%
  rename(População = Valor) %>%
  left_join(
    get_sidra(api = "/t/6803/n6/all/p/2022/v/381/c1821/72129,72144") %>%
      select(
        `Município`,
        `Valor`,
        `Existência de ligação à rede geral de distribuição de água e principal forma de abastecimento de água`
      ) %>%
      pivot_wider(
        names_from = `Existência de ligação à rede geral de distribuição de água e principal forma de abastecimento de água`,
        values_from = `Valor`
      ) %>%
      rename("Número de domicílios" = Total) %>%
      rename("Domicílios água" = "Possui ligação à rede geral e a utiliza como forma principal"),
    by = "Município"
  ) %>%
  left_join(
    get_sidra(api = "/t/6805/n6/all/p/2022/v/381/c11558/72110") %>%
      select(
        `Município`,
        `Valor`,
        `Tipo de esgotamento sanitário`
      ) %>%
      pivot_wider(
        names_from = `Tipo de esgotamento sanitário`,
        values_from = `Valor`
      ) %>%
      rename("Domicílios esgotamento" = "Rede geral ou pluvial"),
    by = "Município"
  ) %>%
  left_join(
    get_sidra(api = "/t/6892/n6/all/p/2022/v/381/c67/2520") %>%
      select(
        `Município`,
        `Valor`
      ) %>%
      rename("Domicílios lixo" = Valor),
    by = "Município"
  ) %>%
  mutate(
    "pc água" = `Domicílios água`/`Número de domicílios`,
    "pc esgotamento" = `Domicílios esgotamento`/`Número de domicílios`,
    "pc lixo" = `Domicílios lixo`/`Número de domicílios`
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

##Regressão

modelo1 <- lm(log(`Domicílios esgotamento`)~log(`PIB`) + log(`População`), data=dados)
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