library(flexmix)
library(tidyverse)
library(factoextra)

# ETAPA 1 - AGRUPAMENTO PENGUINS
df_penguin_scale = readr::read_csv('./files/penguins.csv') |>
  filter(
    is.na(culmen_length_mm) == F
    ) |>
  select(
    where(is.numeric)
    ) |>
  mutate(
    across(
      where(is.numeric),
      ~scale(.)
      )
    ) 
  

df_penguin_scale

## AGRUPAMENTO HIERARQUICO
hclust_penguin = df_penguin_scale |>
  factoextra::get_dist(method = 'euclidian') |>
  hclust()

summary(hclust_penguin)

hclust_penguin |>
  plot()

df_penguin_scale = df_penguin_scale[-9,]
df_penguin_scale = df_penguin_scale[-13,]

hclust_penguin = df_penguin_scale |>
  factoextra::get_dist(method = 'euclidian') |>
  hclust()

summary(hclust_penguin)

hclust_penguin |>
  plot()

fviz_nbclust(df_penguin_scale, FUN = hcut, method = "wss")
fviz_nbclust(df_penguin_scale, FUN = hcut, method = "silhouette")



# k = 4
hclust_penguin |>
  plot()
rect.hclust(hclust_penguin, k=4)

cutree(hclust_penguin, k = 4)

fviz_cluster(list(data = df_penguin_scale, cluster = cutree(hclust_penguin, k = 5)))

## AGRUPAMENTO N HIERARQUICO VIA K-MEANS
df_penguin_scale |> 
  kmeans(centers = 4)


## AGRUPAMENTO VIA MISTURAS



df_penguin_scale %>%
  flexmix::flexmix()







# SIMULAÇÃO DE GRUPOS DIFERENTES EM MÉDIA
set.seed(4390)

## 1.1 - DIFERE EM 10, 5 E 1 UNIDADE DA MÉDIA
simu_near_cluster1 = 
  tibble::tibble(c1 = rnorm(100, mean = 0, sd = 1),
                 c2 = rnorm(100, mean = 10, sd = 1),
                 c3 = rnorm(100, mean = 20, sd = 1))


simu_near_cluster1 |>
  tidyr::pivot_longer(
    where(
      is.numeric
      )
    ) %>%
  flexmix::flexmix(value ~ 1, data = ., k = 3)

simu_near_cluster2 = 
  tibble::tibble(c1 = rnorm(100, mean = 0, sd = 1),
                 c2 = rnorm(100, mean = 5, sd = 1),
                 c3 = rnorm(100, mean = 10, sd = 1))


simu_near_cluster2 |>
  tidyr::pivot_longer(
    where(
      is.numeric
    )
  ) %>%
  flexmix::flexmix(value ~ 1, data = ., k = 3)

simu_near_cluster3 = 
  tibble::tibble(c1 = rnorm(100, mean = 0, sd = 1),
                 c2 = rnorm(100, mean = 1, sd = 1),
                 c3 = rnorm(100, mean = 2, sd = 1))


simu_near_cluster3 |>
  tidyr::pivot_longer(
    where(
      is.numeric
    )
  ) %>%
  flexmix::flexmix(value ~ 1, data = ., k = 3)


## 1.2 - DIFERE EM 10, 5 E 1 UNIDADE DO DESVIO

simu_sd_cluster1 = 
  tibble::tibble(c1 = rnorm(100, mean = 0, sd = 1),
                 c2 = rnorm(100, mean = 0, sd = 10),
                 c3 = rnorm(100, mean = 0, sd = 20))


simu_sd_cluster1 |>
  tidyr::pivot_longer(
    where(
      is.numeric
    )
  ) %>%
  flexmix::flexmix(value ~ 1, data = ., k = 3)

simu_sd_cluster2 = 
  tibble::tibble(c1 = rnorm(100, mean = 0, sd = 1),
                 c2 = rnorm(100, mean = 0, sd = 5),
                 c3 = rnorm(100, mean = 0, sd = 10))


simu_sd_cluster2 |>
  tidyr::pivot_longer(
    where(
      is.numeric
    )
  ) %>%
  flexmix::flexmix(value ~ 1, data = ., k = 3)

simu_sd_cluster3 = 
  tibble::tibble(c1 = rnorm(100, mean = 0, sd = 1),
                 c2 = rnorm(100, mean = 1, sd = 2),
                 c3 = rnorm(100, mean = 2, sd = 3))


simu_sd_cluster3 |>
  tidyr::pivot_longer(
    where(
      is.numeric
    )
  ) %>%
  flexmix::flexmix(value ~ 1, data = ., k = 3)
