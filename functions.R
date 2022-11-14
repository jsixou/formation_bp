# Définition de fonctions --------------------

stats_agregees <- function(a, stat = "moyenne",
                           ...) {
  #' Statistiques agregees
  #' 
  #' @param statistique Statistique désirée. Les valeurs autorisées
  #'  sont 'moyenne', 'variance', 'ecart-type', 'sd', 'ecart type'
  #' @inheritParams mean
  #' 
  #' @examples
  #' stats_agregees(rnorm(10))
  #' stats_agregees(rnorm(10), "cart type")
  #' stats_agregees(rnorm(10), "ecart type")
  #' stats_agregees(rnorm(10), "variance")
  match.arg(stat,
            c("moyenne",
              "variance",
              "ecart-type",
              "sd",
              "ecart type")
  )
  
  switch(stat,
         moyenne = mean(a, ...),
         variance = var(a, ...),
         sd(a, ...)
  )
  
}



decennie_a_partir_annee <- function(ANNEE) {
  #' Transforme annee en decennie
  #' 
  #' @param annee Annee en question
  #' @return La décennie en question
  #' @example 
  #' decennie_a_partir_annee(2011)
  #' decennie_a_partir_annee(2010)
  return(ANNEE - ANNEE %%
           10)
}

assign_na <- function(base,variable,valeur_bad){
  #' Recode some values as NAs
  #' 
  #' @param df `data.frame` we should start from
  #' @param var_name Variable name
  #' @param value Value that should be replaced
  #' @seealso dyplr::na_if
  base <- base %>% mutate(
    !!rlang::sym(variable):=na_if(!!rlang::sym(variable),valeur_bad)
  )
  return(base)
}


table_pour_plot <- function(base,var,groupe){
  t <- base %>% group_by(!!!rlang::syms(c(var,groupe))) %>% 
    summarise(sh_var=n()) %>% 
    group_by(!!rlang::sym(var)) %>% 
    mutate(sh_var=sh_var / sum(sh_var))
  return(t)
}