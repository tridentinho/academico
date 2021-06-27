# MARGEM DE ERRO
# Parametros
# n: Quantidade de elementos da amostra
# sigma: Desvio padrao (obs.: é a raiz quadrada da variância)
# conf: Confianca

IC.me <- function(n, sigma, conf=0.95){
  conf <- qnorm(1-(1-conf)/2)
  me <- (conf*(sigma/sqrt(n)))
  return(me)
}

# IC PARA A MEDIA
# Parametros
# xbarra: Média
# n: Quantidade de elementos da amostra
# sigma: Desvio padrao (obs.: é a raiz quadrada da variância)
# conf: Confianca

IC.media <- function(xbarra, n, sigma, conf=0.95){
  res <- c((xbarra-IC.me(n, sigma, conf)),(xbarra+IC.me(n, sigma, conf)))
  return(res)
}

# questao 8
q8_ic_media = IC.media(36000, 100, 5000)
q8_ic_media

q8_ic_me = IC.me(100, 5000, 0.95)
q8_ic_me

# questao 9
q9_ic_media = IC.media(36000, 1000, 5000)
q9_ic_media

q9_ic_me = IC.me(1000, 5000, 0.95)
q9_ic_me
