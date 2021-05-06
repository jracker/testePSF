

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

# Calcula a moda 
moda <- function(x){
  which.max(tabulate(x))
}


#' Média com tratamento para caso de todos dados faltantes
#'
#' @param x a numeric vector
#'
#' @return mean value of x
#'

mean_wise <- function(x) {
  if (all(is.na(x))) {
    return(NA)
  }
  mean(x, na.rm = TRUE)
}

#' Seleciona dados de um posto
#'
#' @param df a tibble or data frame
#' @param station station code
#' @return tibble or data frame of the selected station
#'
sel_station <- function(df, station) {
  # Função para selecionar um posto específico via código
  df %>%
    dplyr::filter(code_stn == station)
}

#' Filtra os meses completos dos dados
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

#' Identifica anos completos, ou seja com 12 meses de observações válidas.
get_cyears <- function(df) {
  cyrs <- df %>%
    group_by(ano = lubridate::year(date)) %>%
    tally() %>%
    filter(n == 12) %>%
    pull(ano)
  return(cyrs)
}

#' Filtragem dos dados com anos completos
apply_cyears <- function(df) {

  # Usar depois de agrupar os dados caso selecionado mais de um posto
  cyrs <- get_cyears(df)
  df_cyrs <- df %>%
    filter(lubridate::year(date) %in% cyrs)
  return(df_cyrs)
}

#' Seleciona dados de treinamento para aplicação do PSF
#'
#' @param df data frame com série mensal dos dados de vazão
#' @param yrs número de anos que serão removidos das observações para teste do PSF.
#' Valor pré-definido como 2 anos.
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
#' @param n número de meses à frente. Pré-definido como 24.
#'
#' @return data frame com dados do período de teste
#' @export
#'
#' @examples
get_testdt <- function(df, n = 24) {
  inds <- (nrow(df) - n + 1):(nrow(df))
  df <- df[inds, ] %>%
    select(date, qnat_obs)
}


#' Aplica PSF e retorna o modelo ou suas previsões
#'
#' @param df 
#' @param n 
#' @param predict 
#'
#' @return
#' @export
#'
#' @examples
psf_reprod <- function(df, n = 24, predict = TRUE) {
  
  set.seed(1) # p/ reprodutibilidade
  
  model <- psf(df[, "qnat_obs"], cycle = 12)
  preds <- predict(model, n.ahead = n)
  
  if (predict) return(preds) 
  
  model
}

psf4ensemble <- function(df, n = 24, predict = TRUE) {
  # Função para aplicar o psf para ensemble
  model <- psf(df[, "qnat_obs"], cycle = 12)
  preds <- predict(model, n.ahead = n)
  if (predict) {
    return(preds)
  } else {
    return(model)
  }
}



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



ensemble_preds <- function(df, niter = 5) {
  # Retorna uma lista com as predições do psf com difs seeds
  list_preds <- list()
  # list_model <- list()
  for (i in 1:niter) {
    set.seed(i)
    list_preds[[i]] <- psf4ensemble(df)
  }
  return(list_preds)
}



ensemble_models <- function(df, niter = 5) {
  # Retorna uma lista com os modelos do psf com difs seeds
  list_model <- list()
  for (i in 1:niter) {
    set.seed(i)
    model <- psf(df[, "qnat_obs"], cycle = 12)
    list_model[[i]] <- model
  }
  return(list_model)
}

ensemble_mpar<- function(df, params, n = 24) {
  # Retorna predições feitas com os valores médios de k e w
  # extraídos usando a função get_mpar
  set.seed(1)
  model <- psf(df[, "qnat_obs"],
               k = params[[1]]["k"][[1]],
               w = params[[1]]["w"][[1]],
               cycle = 12
  )
  pred <- predict(model, n.ahead = n)
  return(pred)
}


get_mpar <- function(modelo, niter = 5) {
  
  # modelo <- pobqmly_esb$models[[1]]; niter = 5
  
  # Média dos parâmetros k e w dos modelos
  # list_k <- list()
  # list_w <- list()
  # for (i in 1:niter) {
  #   list_k[[i]] <- modelo[[i]]$k
  #   list_w[[i]] <- modelo[[i]]$w
  # }
  # Usar apenas números inteiros no modelo
  #k_mean <- round(Reduce("+", list_k) / length(list_k), 0)
  #w_mean <- round(Reduce("+", list_w) / length(list_w), 0)
  
  tibble(
    k = map_dbl(modelo, "k"), 
    w = map_dbl(modelo, "w")
  ) %>%
    #map_dfr(~.x) %>%
    summarise(across(c(k, w), moda))
  
  #list_param <- list()
  #list_param[[1]] <- c(k_mean, w_mean)
}


get_mpred <- function(lista) {
  # Faz a média das predições dos modelos retornados pelo ensemble
  pred_mean <- list(
    rowMeans(as.data.frame(lista))
  )
}


# For time series cross validation -----------------------------

get_cvpar <- function(model) {
  # Parâmetros após validação cruzada
  unl_model <- unlist(model, recursive = FALSE)
  params_psf <- c(k = unl_model$k, w = unl_model$w)
}

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
