
#' Contabiliza observações válidas
#'
#' @param x vetor numérico
#'
#' @return total de observações não faltantes em x.
#'
nvalid <- function(x) {
  # if(all(is.na(x))) return(0)
  sum(!is.na(x))
}

#' Média com tratamento para caso de todos dados faltantes
#'
#' @param x um vetor numérico
#'
#' @return valor médio de x
#'

mean_wise <- function(x) {
  if (all(is.na(x))) {
    return(NA)
  }
  mean(x, na.rm = TRUE)
}


#' Calcula a moda 
#' 
#' @param x um vetor numérico
#' 
#' @return um inteiro correspondente a moda do vetor numérico
#' 
moda <- function(x){
  which.max(tabulate(x))
}


#' Seleciona dados de um posto
#'
#' @param df um tibble ou data frame
#' 
#' @param station código da estação
#' 
#' @return tibble ou data frame da estação selecionada
#'
sel_station <- function(df, station) {
  df %>%
    dplyr::filter(code_stn == station)
}

#' Filtra os meses completos dos dados
#' 
#'  @param df um tibble ou data frame
#'  
#'  @param ndays_thresh  um inteiro
#'  
#'  @return um tibble ou data frame com os meses completos
#'  
apply_cmonth <- function(df, ndays_thresh = 28) {
  df <- df %>%
    dplyr::group_by(date = floor_date(date, "month"), code_stn) %>%
    dplyr::summarise(
      qnat_obs = mean_wise(qnat),
      valid = nvalid(qnat),
      N = n(),
      .groups = "drop"
    ) %>%
    dplyr::filter(valid >= ndays_thresh) %>%
    select(date, code_stn, qnat_obs)
  return(df)
}


#' Identifica anos completos, ou seja, com 12 meses de observações válidas
#'  
#' @param df um tibble ou data frame
#' 
#' @return vetor com os anos que apresentam 12 meses de observações válidas
#' 
get_cyears <- function(df) {
  cyrs <- df %>%
    group_by(ano = lubridate::year(date)) %>%
    tally() %>%
    filter(n == 12) %>%
    pull(ano)
  return(cyrs)
}

#' Filtragem dos dados com anos completos
#'  
#' Usar depois de agrupar os dados caso selecionado mais de um posto
#'  
#'  @param df um tibble ou data frame
#'  
#'  @param ndays_thresh  um inteiro
#'  
#'  @return df um tibble ou data frame com os meses completos
#'  
apply_cyears <- function(df) {
  cyrs <- get_cyears(df)
  df_cyrs <- df %>%
    filter(lubridate::year(date) %in% cyrs)
  return(df_cyrs)
}


#' Filtragem dos dados com meses e anos completos para uso no modelo de médias
#'  
#' Usar depois de agrupar os dados caso selecionado mais de um posto
#'  
#'  @param df um tibble ou data frame
#'  
#'  @param ndays_thresh  um inteiro
#'  
#'  @return df um tibble ou data frame com meses e anos completos
#'  
preprocess_mlg <- function(df, ndays_thresh = 28) {
  df_cmonth <- df %>%
    dplyr::group_by(date = floor_date(date, "month")) %>%
    dplyr::summarise(
      qnat_obs = mean_wise(qnat),
      valid = nvalid(qnat),
      N = n(),
      .groups = "drop"
    ) %>%
    dplyr::filter(valid >= ndays_thresh) %>%
    select(date, qnat_obs)
  cyrs <- get_cyears(df_cmonth)
  df_cyrs <- df %>%
    filter(lubridate::year(date) %in% cyrs) %>% 
    filter(!is.na(qnat))
}

#' Filtragem dos dados para utilização na função mprev_lt
#'  
#' Caso o número de linhas não é divisível por 12, retira a primeira linha e 
#' assim por diante
#' 
#'  @param df um tibble ou data frame
#'  
#'  @return df um tibble ou data frame com nrows %% 12 == 0
#'  

datprep_prevlt <- function(df) {
  rows <- nrow(df)
  rows.aux <- nrow(df) # número de linhas dos dados qnat_obs
  while (rows.aux %% 12 != 0) {
    rows.aux <- rows.aux - 1 # retorna linha que é divisível por 12
  }
  start_row <- (rows - rows.aux) + 1
  return(df[start_row:(rows), ])
}


#' Seleciona dados de treinamento para aplicação do PSF
#'
#' @param df data frame com série mensal dos dados de vazão
#' 
#' @param yrs número de anos que serão removidos das observações para teste do 
#' PSF.
#' Valor pré-definido como 2 anos.
#' 
get_traindt <- function(df, yrs = 2) {
  cyrs <- get_cyears(df)
  leave_out <- tail(cyrs, n = yrs)
  data <- df %>%
    filter(!lubridate::year(date) %in% leave_out)
  return(data)
}

#' Selecionar dados de teste para avaliação do PSF
#'
#' @param df data frame com dados de vazão mensal
#' 
#' @param n número de meses à frente. Pré-definido como 24.
#'
#' @return data frame com dados do período de teste
#' 
#' @export
#'
#' @examples
#' 
get_testdt <- function(df, n = 24) {
  inds <- (nrow(df) - n + 1):(nrow(df))
  df <- df[inds, ] %>%
    select(date, qnat_obs)
}


#' Aplica PSF e retorna o modelo ou as previsões feitas com o modelo
#'
#' @param df um tibble ou data frame com dados de vazão mensal
#' 
#' @param n número de meses à frente. Pré-definido como 24.
#' 
#' @param predict valor lógico pré-definido para retornar as previsões
#' 
#'
#' @return vetor numérico com as previsões ou objeto de classe psf com 
#' informações do modelo
#' 
#' @export
#'
#' @examples
#' 
psf_reprod <- function(df, n = 24, predict = TRUE) {
  
  set.seed(1) # p/ reprodutibilidade
  
  model <- psf(df[, "qnat_obs"], cycle = 12)
  preds <- predict(model, n.ahead = n)
  
  if (predict) return(preds) 
  
  model
}


#' Aplica o PSF e retorna o modelo ou as previsões
#' 
#' Adaptado para uso no ensemble
#'
#' @param df um tibble ou data frame com dados de vazão mensal
#' 
#' @param n número de meses à frente. Pré-definido como 24.
#' 
#' @param predict valor lógico pré-definido para retornar as previsões
#'
#' @return vetor numérico com as previsões ou objeto de classe psf com 
#' informações do modelo 
#' 
#' @export
#'
#' @examples

psf4ensemble <- function(df, n = 24, predict = TRUE) {
  model <- psf(df[, "qnat_obs"], cycle = 12)
  preds <- predict(model, n.ahead = n)
  if (predict) {
    return(preds)
  } else {
    return(model)
  }
}


#' Aplica o PSF um número de vezes e retorna os modelos ou as 
#' previsões
#' 
#' @param df um tibble ou data frame com dados de vazão mensal
#' 
#' @param niter iterações que definem número de modelos ou de previsões
#' 
#' @param predict valor lógico pré-definido para retornar as previsões
#' 
#' @return lista das previsões ou objetos de classe psf com 
#' informações do modelos 
#' 
#' @export
#' 
#' @examples
#' 
ensemble_psf <- function(df, niter = 5, predict = TRUE) {
  list_preds <- list()
  list_model <- list()
  if (predict) {
    for (i in 1:niter) {
      set.seed(i)
      list_preds[[i]] <- psf4ensemble(df)
    }
    return (list_preds)
  } else {
    for (i in 1:niter) {
      set.seed(i)
      list_model[[i]] <- psf4ensemble(df, predict = FALSE)
    }
    return(list_model)
  }
}

#' Aplica o PSF utilizando os parâmetros k e w selecionados
#' 
#' @param df um tibble ou data frame com dados de vazão mensal
#' 
#' @param params lista de tibbles contendo os valores k e w
#' 
#' @param predict valor lógico pré-definido para retornar as previsões
#'
#' @return lista das previsões ou objetos de classe psf com 
#' informações do modelos 
#' 
#' @export
#'
#' @examples
#' 
ensemble_mpar<- function(df, params, n = 24) {
  set.seed(1)
  model <- psf(df[, "qnat_obs"],
               k = params[[1]]["k"][[1]],
               w = params[[1]]["w"][[1]],
               cycle = 12
  )
  pred <- predict(model, n.ahead = n)
  return(pred)
}

#' Calcula a moda dos parâmetros k e w 
#' 
#' @param modelo listas aninhadas que contém o objeto de classe psf com 
#' informações do modelo
#'
#' @return tibble com a moda dos parâmetros k e w
#' 
#' @export
#'
#' @examples
#' 
get_modpar <- function(modelo) {
  tibble(
    k = map_dbl(modelo, "k"),
    w = map_dbl(modelo, "w")
  ) %>%
  #map_dfr(~.x) %>%
  summarise(across(c(k, w), moda))
}

#' Calcula a média os parâmetros k e w 
#' 
#' @param modelo listas aninhadas com objetos de classe psf com informações do 
#' modelo
#' 
#' @return tibble com a média dos parâmetros k e w
#' @export
#'
#' @examples
#' 
get_meanpar <- function(modelo){
  tibble(
    k = map_dbl(modelo,"k"),
    w = map_dbl(modelo,"w")
  ) %>% 
    summarize(
      mean = across(c(k,w), mean_wise),
      mean = round(mean,0)
    )
}

#' Calcula a média das previsões do ensemble
#' 
#' @param lista com as previsões gerados por cada modelo do ensemble
#' 
#' @return lista com a previsão média dos modelos do ensemble
#' 
#' @export
#'
#' @examples
#' 
get_mpred <- function(lista) {
  pred_mean <- list(
    rowMeans(as.data.frame(lista))
  )
}

#' Seleciona os parâmetros k e w dos objetos de classe psf gerados na validação
#' cruzada 
#' 
#' @param model objeto de classe psf com informações do modelo
#' 
#' @return vetor de inteiros com os parâmetros k e w
#' 
#' @export
#'
#' @examples
#' 
get_cvpar <- function(model, lt = FALSE) {
  unl_model <- unlist(model, recursive = FALSE)
  if (lt) return(tibble(k = unl_model$k, w = unl_model$w)) 
  params_psf <- c(k = unl_model$k, w = unl_model$w)
}

# get_cvpar <- function(model) {
#   unl_model <- unlist(model, recursive = FALSE)
#   params_psf <- c(k = unl_model$k, w = unl_model$w)
# }




#' Aplica o PSF utilizando os parâmetros selecionados na validação cruzada
#' 
#' @param df um tibble ou data frame com dados de vazão mensal
#' 
#' @param params vetor de inteiros com os parâmetros selecionados na validação 
#' cruzada
#' 
#' @return vetor numérico com as previsões 
#' 
#' @export
#
#' @examples
#' 
psf_cvparam <- function(df, n = 12, params = NULL) {
  # df = train287_qmly; n = 12; params = cvm_params
  set.seed(1)
  model <- psf(df[["qnat_obs"]],
    k = params[[1]],
    w = params[[2]],
    cycle = 12
  )
  preds <- predict(model, n.ahead = n)
}


# psfr_lt <- function(df, n = 24, predict = TRUE, model = FALSE) {
#   set.seed(1) # p/ reprodutibilidade
#   model <- psf(df[, "qnat_obs"], cycle = 1)
#   preds <- predict(model, n.ahead = n)
#   
#   # Retorna apenas a previsão para o horizonte de previsão n
#   if (predict) return(preds[length(preds)]) 
#   # Retorna o modelo contendo os parâmetros k e w
#   model
# }


# getl_mtrain <- function(dat, ini_mon, nmonths = 12, yr_ref = 2017){
#   # Starting time for the predictions
#   ref_dates <- as.Date(paste0(yr_ref, "-", ini_mon, "-1")) %>%
#     as_tibble() %>%
#     rename(S = value)
#   # Return tibble with the starting date and the filtered data (using S)
#   qnat_ref <- tibble(ref_dates, data = list(dat)) %>%
#     mutate(
#       data = map2(data, S, ~ filter(..1, date <= ..2))
#       # L1 = map(
#       #   dados_filtrados,
#       #   ~ psf_reprod(.x, n = 1, predict = TRUE)
#       # )
#     )
# }


# Modelo de previsões médias
# meanlg <- function(df,year = 2016){
#   
#   longterm_monthly_qnat <- calc_longterm_monthly_stats(
#     data = df,
#     dates = "date",
#     values = "qnat",
#     start_year = "1969",
#     end_year = year
#   )
#   
#   longterm_monthly_qnat["Month"] <- seq(1, 13)
#   
#   longterm_monthly_qnat <- longterm_monthly_qnat %>%
#     slice(1:n() - 1) %>%
#     select(Month, Mean) %>%
#     pivot_wider(
#       names_from = Month,
#       values_from = Mean,
#       names_prefix = "L"
#     ) %>% 
#     pivot_longer(
#       cols = starts_with("L"),
#       names_to = "L",
#       values_to = "qnat_mean"
#     )
#   
#   
#   
#   
# 
# }

meanlg <- function(df,year = 2016){
  
  longterm_monthly_qnat <- calc_longterm_monthly_stats(
    data = df,
    dates = "date",
    values = "qnat",
    #start_year = "1969",
    end_year = year,
    ignore_missing = TRUE
  )
  
  longterm_monthly_qnat["Month"] <- seq(1, 13)

  longterm_monthly_qnat <- longterm_monthly_qnat %>%
    slice(1:n() - 1) %>%
    select(Month, Mean) %>%
    pivot_wider(
      names_from = Month,
      values_from = Mean,
      names_prefix = "L"
    ) %>%
    pivot_longer(
      cols = starts_with("L"),
      names_to = "L",
      values_to = "qnat_mean"
    )
}



# mprev_lt <- function(dat, ini_mon, nmonths = 12, yr_ref = 2017) {
#   # Starting time for the predictions
#   ref_dates <- as.Date(paste0(yr_ref, "-", ini_mon, "-1")) %>%
#     as_tibble() %>%
#     rename(S = value)
#   # Return tibble with the starting date and the filtered data (using S)
#   qnat_ref <- tibble(ref_dates, data = list(dat)) %>%
#     mutate(
#       data = map2(data, S, ~ filter(..1, date <= ..2))
#       # L1 = map(
#       #   dados_filtrados,
#       #   ~ psf_reprod(.x, n = 1, predict = TRUE)
#       # )
#     )
#   
#   util_lt <- tibble(lead_time = 1:nmonths) %>%
#     pivot_wider(
#       names_from = lead_time,
#       values_from = lead_time,
#       names_prefix = "L"
#     )
#   
#   qnat_lt <- cbind(qnat_ref, util_lt)
#   
#   qnat_model <- qnat_lt %>% 
#     mutate(across(-c(data,S),
#                   ~ map2(.x,
#                          data,
#                          ~ psf_reprod_teste(.y, 
#                                             n = .x, 
#                                             predict = FALSE))))
#   
#   qnat_params <- qnat_model %>% 
#     mutate(across(-c(data,S),
#                   ~ map(.x,
#                         ~ get_cvpar(.))))
#   
#   vars_lt <- names(qnat_params)[3:14]
#   for (i in vars_lt){
#     qnat_params  <- qnat_params %>% 
#       unnest_wider(i, names_sep = "_")
#   }  
#   
#   
#   qnat_params <- qnat_params %>%
#     rowwise() %>% 
#     mutate(
#       k =  moda(c_across(ends_with("k"))), 
#       w = moda(c_across(ends_with("w")))
#     ) %>% 
#     select(S,k,w)
#   
#   qnat_mprev <- qnat_lt %>% 
#     mutate(across(-c(data,S),
#                   ~ map2_dbl(.x,
#                              data,
#                              ~ psf_reprod_teste(.y, n = .x)))) %>% 
#     select(-data) %>% 
#     group_by(S) %>% 
#     nest() %>% 
#     rename(qnat_prev = data) %>% 
#     left_join(.,qnat_params)
# }


mprev_lt <- function(dat, ini_mon, nmonths = 12, yr_ref = 2017) {
  # Starting time for the predictions
  ref_dates <- as.Date(paste0(yr_ref, "-", ini_mon, "-1")) %>%
    as_tibble() %>%
    rename(S = value)
  # Return tibble with the starting date and the filtered data (using S)
  qnat_ref <- tibble(ref_dates, data = list(dat)) %>%
    mutate(
      data = map2(data, S, ~ filter(..1, date <= ..2))
      # L1 = map(
      #   dados_filtrados,
      #   ~ psf_reprod(.x, n = 1, predict = TRUE)
      # )
    )
  qnat_preprocess <- qnat_ref %>%
    mutate(
      across(
        data,
        ~ map(
          .x,
          ~ datprep_prevlt(df = .x)
        )
      )
    )
  
  qnat_prev <- qnat_preprocess %>%
    mutate(
      prev = map(
        data,
        ~ psf_reprod(.x, n = 12, predict = TRUE)
      )
    )
  
  
  cleanqnat_prev <- qnat_prev %>%
    unnest(prev) %>%
    group_by(S) %>%
    mutate(
      key = row_number(),
      L = paste0("L")
    ) %>%
    unite(L, c(L, key), sep = "", remove = FALSE) %>%
    select(-key) %>%
    spread(L, prev) %>%
    relocate(S, data, L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12)
  
  
  cleanqnat_prevparams <- cleanqnat_prev %>%
    mutate(
      model = map(
        data,
        ~ psf_reprod(.x, n = 12, predict = FALSE)
      ),
      params = map(model, ~ get_cvpar(., lt = TRUE))
    ) %>%
    unnest(params) %>%
    select(-c(data, model)) %>% 
    group_by(S) %>% 
    nest(qnatpsf_prev = -c(S,k,w)) 
  
  cleanqnat_final <- cleanqnat_prevparams %>%
    mutate(
      qnatpsf_prev = map(
        qnatpsf_prev,
        ~ .x %>%
          pivot_longer(
            cols = starts_with("L"),
            names_to = "L",
            values_to = "qnatpsf_prev"
          )
      )
    )
}

getl_mteste <- function(dat, ini_mon, nmonths = 12, yr_ref = 2017) {
  # Starting time for the predictions
  ref_dates <- as.Date(paste0(yr_ref, "-", ini_mon, "-1")) %>%
    as_tibble() %>%
    rename(S = value)
  
  util_lt <- tibble(lead_time = 1:nmonths) %>%
    pivot_wider(
      names_from = lead_time,
      values_from = lead_time,
      names_prefix = "L"
    )
  # Return tibble with the starting date and the filtered data (using S)
  qnat_ref <- tibble(ref_dates, data = list(dat)) %>%
    cbind(., util_lt)
  
  qnat_ref2 <- qnat_ref %>%
    mutate(
      data = map2(data, S, ~ filter(..1, (date > ..2) & date <= ..2 %m+% months(12)))
    )
  
  qnat_ref2 %>%
    mutate(
      across(
        -c(data, S),
        ~ map2_dbl(
          .x,
          data,
          ~ .y$qnat_obs[.x]
        )
      )
    ) %>%
    nest(qnat_obs = L1:L12) %>%
    select(-data) %>%
    # Para fazer a avaliação com as métricas  
    mutate( 
      qnat_obs = map(
        qnat_obs,
        ~ .x %>%
          pivot_longer(
            cols = starts_with("L"),
            names_to = "L",
            values_to = "qnat_obs"
          )
      )
    )
  
}



