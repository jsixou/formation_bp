rm(list = ls())

# ENVIRONNEMENT --------------------
## Import packages -----------------

if (!require("ggplot2")) install.packages("ggplot2")
if (!require("stringr")) install.packages("stringr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("MASS")) install.packages("MASS")


library(tidyverse)
library(MASS)
library(dplyr)
library(forcats)

api_token <- yaml::read_yaml("secrets.yaml")$api_key



## Import données -----------------------

df <- readr::read_csv2("individu_reg.csv")

## Fonctions ----------------

source("functions.R", encoding = "UTF-8")


# Traitement de données -----------------------


df2 <- df %>% 
  dplyr::select(c("region", "dept", "aemm", "aged", "anai", "catl", "cs1", "cs2", "cs3", "couple", "na38", "naf08", "pnai12", "sexe", "surf", "tp", "trans", "ur"))
print(df2, 20)

df2 <- assign_na(df2,"na38","ZZ")
df2 <- assign_na(df2,"trans","Z")
df2 <- assign_na(df2,"tp","Z")

df2[endsWith(df2$naf08, "ZZ"), "naf08"] <- NA


str(df2)
df2 <- df2 %>%
  mutate(across(
    c(-region, -aemm, -aged, -anai),
    as.factor)
  )

df2$sexe <-
  fct_recode(df2$sexe, "Homme" = "1", "Femme" = "2")

df2 <- df2 %>%
  mutate(age = as.numeric(aged))



# Stats descriptives ------------------------


unique(df2$cs3) %>% length()
unique(df2$cs2) %>% length()

stats_agregees(rnorm(10))
stats_agregees(rnorm(10), "ecart type")
stats_agregees(rnorm(10), "variance")


## STATISTIQUES AGE ======================

summarise(group_by(df2, age), n())


df2 %>%
  dplyr::select(age) %>%
  ggplot(.) + geom_histogram(aes(x = 5 * floor(age / 5)),
                             stat = "count")

ggplot(df2[as.numeric(df2$aged) > 50,],
       aes(x = as.numeric(aged),
           y = ..density..,
           fill = factor(decennie_a_partir_annee(as.numeric(aemm)))
       ),
       alpha = 0.2) + geom_histogram()



## part d'homme dans chaque cohorte ===================

temp <- table_pour_plot(df2,"age","sexe") %>% 
  dplyr::filter(sexe== "Homme")


ggplot(temp) +
  geom_bar(aes(x = as.numeric(age),
               y = sh_var), stat = "identity") +
  geom_point(aes(x = as.numeric(age),
                 y = sh_var), stat = "identity", color = "red") +
  coord_cartesian(c(0, 100))


## stats surf par statut ==================

df3 <- table_pour_plot(df2,"couple","surf")

ggplot(df3) +
  geom_bar(aes(x = surf, y = sh_var, color = couple),
           stat = "identity", position = "dodge")

## stats trans par statut ===================

df3 <- table_pour_plot(df2,"couple","trans")

ggplot(df3) + geom_bar(aes(x = trans, y = sh_var, color = couple),
                       stat = "identity", position = "dodge")


# STATS AGREGEES =================

stats_agregees(df2 %>%
                 filter(sexe == "Homme") %>%
                 mutate(aged = as.numeric(aged)) %>%
                 pull(aged), na.rm = TRUE)
stats_agregees(df2 %>%
                 filter(sexe == "Femme") %>%
                 mutate(aged = as.numeric(aged)) %>%
                 pull(aged), na.rm = TRUE)
stats_agregees(df2 %>%
                 filter(sexe == "Homme" & couple == "2") %>%
                 mutate(aged = as.numeric(aged)) %>%
                 pull(aged), na.rm = TRUE)
stats_agregees(df2 %>%
                 filter(sexe == "Femme" & couple == "2") %>%
                 mutate(aged = as.numeric(aged)) %>%
                 pull(aged), na.rm = TRUE)


# MODELISATION ----------------------------


df3 <- df2 %>%
  dplyr::select(surf, cs1, ur, couple, age) %>%
  filter(surf != "Z")

polr(surf ~ cs1 + factor(ur),
     df3 %>%
       filter(
         couple == 2 &
           age > 40 &
           age < 60)
)
