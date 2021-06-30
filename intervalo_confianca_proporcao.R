# MARGEM DE ERRO
# Parametros
# n: Quantidade de elementos da amostra
# pchapeu: sucessos/n
# conf: Confianca

IC.me <- function(pchapeu, n , conf=0.95){
  conf <- qnorm(1-(1-conf)/2)
  print(paste(c('conf: ',conf), collapse=''))
  me <- (conf*(sqrt((pchapeu*(1-pchapeu))/n)))
  return(me)
}

# IC PARA A MEDIA
# Parametros
# n: Quantidade de elementos da amostra
# sucessos: numero de casos de sucesso
# conf: Confianca

IC.prop <- function(n, sucessos, conf=0.95){
  pchapeu <- sucessos/n
  print(paste(c('pchapeu: ',pchapeu), collapse=''))
  qchapeu <- 1-pchapeu
  print(paste(c('qchapeu: ',qchapeu), collapse=''))
  if((n*pchapeu) >= 5 && (n*qchapeu)>=5){
    print('Passou nos requisitos')
    me <- IC.me(pchapeu, n, conf)
    print(paste(c('margem de erro: ',me), collapse=''))
    res <- c((pchapeu-me),(pchapeu+me)) 
  }else{
    res <- 'erro'
    if((n*pchapeu) < 5){
       res <- c(res, 'n*pchapeu resultou em um numero menor que 5')
    }else if((n*qchapeu) < 5){
      res <- c(res, 'n*qchapeu resultou em um numero menor que 5')
    }
  }
  return(res)
}

q10_ic_prop <- IC.prop(132, 44, conf=0.90)
print(q10_ic_prop)
