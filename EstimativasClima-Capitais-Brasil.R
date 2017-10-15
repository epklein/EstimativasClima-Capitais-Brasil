library(RCurl)
library(XML)
library(glue)

funcGetCitiesInfo <- function() {
  
  list(
    
    #Distrito Federal
    "Brasília" = list(WikiName="Brasília",
                      Region="Centro-Oeste", State="Distrito Federal"),
    
    #Centro-Oeste
    "Campo Grande" = list(WikiName="Campo Grande (Mato Grosso do Sul)",
                          Region="Centro-Oeste", State="Mato Grosso do Sul"),
    "Cuiabá" = list(WikiName="Cuiabá",
                    Region="Centro-Oeste", State="Mato Grosso"),
    "Goiânia" = list(WikiName="Goiânia",
                     Region="Centro-Oeste", State="Goiás"),
    
    #Nordeste
    "Aracaju" = list(WikiName="Aracaju",
                     Region="Nordeste", State="Sergipe"),
    "Fortaleza" = list(WikiName="Fortaleza",
                       Region="Nordeste", State="Ceará"),
    "João Pessoa" = list(WikiName="João Pessoa",
                         Region="Nordeste", State="Paraíba"),
    "Maceió" = list(WikiName="Maceió",
                    Region="Nordeste", State="Alagoas"),
    "Natal" = list(WikiName="Natal (Rio Grande do Norte)",
                   Region="Nordeste", State="Rio Grande do Norte"),
    "Recife" = list(WikiName="Recife",
                    Region="Nordeste", State="Pernambuco"),
    "Salvador" = list(WikiName="Salvador (Bahia)",
                      Region="Nordeste", State="Bahia"),
    "São Luís" = list(WikiName="São Luís (Maranhão)",
                      Region="Nordeste", State="Maranhão"),
    "Teresina" = list(WikiName="Teresina",
                      Region="Nordeste", State="Piauí"),
    
    #Norte
    "Belém" = list(WikiName="Belém (Pará)",
                   Region="Norte", State="Pará"),
    "Boa Vista" = list(WikiName="Boa Vista (Roraima)",
                       Region="Norte", State="Roraima"),
    "Macapá" = list(WikiName="Macapá",
                    Region="Norte", State="Amapá"),
    "Manaus" = list(WikiName="Manaus",
                    Region="Norte", State="Amazonas"),
    "Palmas" = list(WikiName="Palmas (Tocantins)",
                    Region="Norte", State="Tocantins"),
    "Porto Velho" = list(WikiName="Porto Velho",
                         Region="Norte", State="Rondônia"),
    "Rio Branco" = list(WikiName="Rio Branco",
                        Region="Norte", State="Acre"),
    
    #Sudeste
    "Belo Horizonte" = list(WikiName="Belo Horizonte",
                            Region="Sudeste", State="Minas Gerais"),
    "Rio de Janeiro" = list(WikiName="Rio de Janeiro (cidade)",
                            Region="Sudeste", State="Rio de Janeiro"),
    "São Paulo" = list(WikiName="São Paulo (cidade)",
                       Region="Sudeste", State="São Paulo"),
    "Vitória" = list(WikiName="Vitória (Espírito Santo)",
                     Region="Sudeste", State="Espírito Santo"),
    
    # Sul
    "Curitiba" = list(WikiName="Curitiba",
                      Region="Sul", State="Paraná"),
    "Florianópolis" = list(WikiName="Florianópolis",
                           Region="Sul", State="Santa Catarina"),
    "Porto Alegre" = list(WikiName="Porto Alegre",
                          Region="Sul", State="Rio Grande do Sul")
  )
}

# função para formatar texto em numérico
funcCharToNum <- function(vctAsChar) {
  # remove separador de milhar, espaços
  t <- gsub("(*UCP)[\\s\\p{L}]+|\\.|\\W+$", "", vctAsChar, perl=TRUE)
  # substitui separador de ponto flutuante para adequação ao formato en
  t <- gsub(",", ".", t, fixed=T)
  # retorna como tipo numérico
  return(as.numeric(t))
}

# função que avalia se uma dada tabela é de dados climatológicos
funcIsWeatherInfoTable <- function(table) {
  
  return (!is.null(table) &
            all(table[1,] ==
                  c("Mês", "Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul",
                    "Ago", "Set", "Out", "Nov", "Dez", "Ano")))
}

# coleta os dados de uma cidade na wikipedia
funcCollectCityWeatherInfoFromWikipedia <- function (cityInfo) {
  
  print(glue("Processando \"{cityInfo$WikiName}\" ..."))
  
  wikiUrl <- glue("https://pt.wikipedia.org/wiki/", gsub(" ", "_", cityInfo$WikiName))
  tablesFromPageWiki <- readHTMLTable(getURL(wikiUrl))
  
  desiredTableIndex <- which(sapply(tablesFromPageWiki, funcIsWeatherInfoTable))
  
  if (length(desiredTableIndex) != 1) {
    
    errMsg <- glue("* Problema encontrado! ", length(desiredTableIndex), " entradas!")
    
    cityInfo[["WeatherTable"]] <- errMsg
    
    print(errMsg)
    
  }
  else {
    
    weatherTable <- tablesFromPageWiki[[desiredTableIndex]]
    
    # remove a coluna de rodapé
    weatherTable <- weatherTable[-nrow(weatherTable),]
    
    # transformoa as colunas de fatores para tipo caractere
    weatherTable <- data.frame(lapply(weatherTable, as.character), stringsAsFactors=FALSE)
    
    # guarda nomes das colunas e linhas antes da transformação numérica
    cnames <- weatherTable[1,]
    rnames <- weatherTable[,1]
    
    # conversão dos valores da tabela em numérico
    weatherTable <- data.frame(apply(weatherTable, 2, funcCharToNum))
    
    # transforma primeira coluna e primeira linha em títulos
    colnames(weatherTable) <- cnames
    rownames(weatherTable) <- rnames
    weatherTable <- weatherTable[-1,-1]
    
    cityInfo[["WeatherTable"]] <- weatherTable
  }
  
  return(cityInfo)
}

cities <- funcGetCitiesInfo()

citiesWithWeatherInfo <- lapply(cities, funcCollectCityWeatherInfoFromWikipedia)

# save(citiesWithWeatherInfo, file="data.RData")

# load(file="data.RData")