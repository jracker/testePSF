
#' Count valid data
#'
#' @param x a numeric vector
#'
#' @return total non-missing values of x.
#'
nvalid <- function(x) {
  # if(all(is.na(x))) return(0)
  sum(!is.na(x))
}


#' Calculate the data mean
#'
#' @param x a numeric vector
#'
#' @return mean value of x
#'

mean_wise <- function(x){
  if(all(is.na(x))) return(NA)
  mean(x, na.rm = TRUE)
}

#' Select a hydroelectric station
#'
#' @param df a tibble or data frame
#' @param station station code
#' @return tibble or data frame of the selected station
#'

sel_station <- function(df,station){
  # Função para selecionar um posto específico via código
  df %>% 
    dplyr::filter(code_stn == station) 
}


apply_cmonth <- function(df){
  # Função para filtrar os meses completos dos dados 
  df <- df %>% 
    dplyr::group_by(date = floor_date(date, "month"), code_stn) %>% 
    dplyr::summarise(
      qnat_obs = mean_wise(qnat), 
      valid = nvalid(qnat), 
      N = n(),
      .groups = "drop"
    ) %>%
    dplyr::filter(valid >= 28) %>% 
    select(date,code_stn,qnat_obs)
  return(df)
}

get_cyears <- function(df){
  # Função para pegar os anos completos dos dados
  cyrs <- df %>% 
    group_by(ano = lubridate::year(date)) %>% 
    tally() %>% 
    filter(n == 12) %>% 
    pull(ano)
  return(cyrs)
}

apply_cyears  <- function(df){
  # Função para filtrar os dados com anos completos
  # Usar depois de agrupar os dados caso selecionado mais de um posto
  cyrs <- get_cyears(df)
  df_cyrs <- df %>% 
    filter(lubridate::year(date) %in% cyrs)
  return(df_cyrs)
}


get_traindt <- function(df, yrs = 2){
  # Função para pegar os dados de treinamento
  # Padrão: 2 últimos anos de observações são removidos
  cyrs <- get_cyears(df)
  leave_out <- tail(cyrs,n = yrs) # retira 2 últ. anos por padrão
  data <- df %>% 
    filter(!lubridate::year(date) %in% leave_out)
  return(data)
}


get_testdt <- function(df,n = 24){
  # Função para pegar os dados de teste
  # Padrão: 2 últimos anos de observações são usados como teste
  inds <- (nrow(df) - n + 1):(nrow(df))
  df <- df[inds,] %>% 
    select(date,qnat_obs)
}


psf_reprod <- function(df, n = 24, ret = "preds"){
  # Função para aplicar o psf, retorna as predições do modelo
  set.seed(1) # p/ reprodutibilidade
  model <- psf(df[,"qnat_obs"],cycle = 12) 
  preds <- predict(model,n.ahead = n) 
  if (ret == "preds") return(preds) else return(model)
}

psf4ensemble <- function(df,n = 24){
  # Função para aplicar o psf para ensemble
  model <- psf(df[,"qnat_obs"],cycle = 12)
  preds <- predict(model,n.ahead = n) 
  return(preds)
}



ensemble_preds <- function(df){
  # Retorna uma lista com as predições do psf com difs seeds
  list_preds <- list()
  # list_model <- list()
  for(i in 1:5){
    set.seed(i)
    list_preds[[i]] <- psf4ensemble(df)
  }
  return(list_preds)
}



ensemble_models <- function(df){
  # Retorna uma lista com os modelos do psf com difs seeds
  list_model <- list()
  for(i in 1:5){
    set.seed(i)
    model <- psf(df[,"qnat_obs"],cycle = 12)
    list_model[[i]] <- model
  }
  return(list_model)
  
}

ensemble_mpar <- function(df,params,n = 24){
  # Retorna predições feitas com os valores médios de k e w
  # extraídos usando a função get_mpar
  set.seed(1)
  model <- psf(df[,"qnat_obs"],
               k = params[[1]][[1]],
               w = params[[1]][[2]],
               cycle = 12)
  pred <- predict(model,n.ahead = n)
  return(pred)
}

get_mpar <- function(modelo){
  # Média dos parâmetros k e w dos modelos
  list_k <- list()
  list_w <- list()
  teste <- for(i in 1:5){
    list_k[[i]] <- modelo[[i]]$k
    list_w[[i]] <- modelo[[i]]$w
  }
  # Usar apenas números inteiros no modelo
  k_mean <-  round(Reduce("+",list_k)/length(list_k),0)
  w_mean <- round(Reduce("+",list_w)/length(list_w),0)
  list_param <- list()
  list_param[[1]] <- c(k_mean,w_mean)
}


get_mpred <- function(lista){
  # Faz a média das predições dos modelos retornados pelo ensemble
  pred_mean <- list(
    rowMeans(as.data.frame(lista))
  )
}


# For time series cross validation -----------------------------

get_cvpar <- function(model){
  # Parâmetros após validação cruzada
  unl_model <- unlist(model,recursive = FALSE)
  params_psf <- c(unl_model$k,unl_model$w)
}

getcv_mpar <-  function(df){
  # Extrai parâmetros médios k e w após a validação cruzada
  # Returna um vetor atômico 
  param_values <- list()
  for(i in 1:1){
    teste <- round(rowMeans(as.data.frame(df[[i]])),0)
    param_values[[i]] <-  unlist(teste, recursive = FALSE)
  }
  return(unlist(param_values,recursive = FALSE))
}




