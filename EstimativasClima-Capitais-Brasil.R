library(RCurl)
library(XML)

# função para formatar texto em numérico
funcCharToNum <- function(vctAsChar) {
  # remove separador de milhar, espaços
  t <- gsub("(*UCP)[\\s\\p{L}]+|\\.|\\W+$", "", vctAsChar, perl=TRUE)
  # substitui separador de ponto flutuante para adequação ao formato en
  t <- gsub(",", ".", t, fixed=T)
  # retorna como tipo numérico
  return(as.numeric(t))
}

# link da wikipedia para busca da tabela
wikiPOA <- getURL("https://pt.wikipedia.org/wiki/Porto_Alegre")

# captura de todas as tabelas com a biblioteca XML
tablesWikiPOA <- readHTMLTable(wikiPOA)

# a tabela de Dados Climatológicos é a quarta da página 
weatherTablePOA <- tablesWikiPOA[[4]]
# remove a coluna de rodapé
weatherTablePOA <- weatherTablePOA[-nrow(weatherTablePOA),]

# transformoa as colunas de fatores para tipo caractere
weatherTablePOA <- data.frame(lapply(weatherTablePOA, as.character), stringsAsFactors=FALSE)

# guarda nomes das colunas e linhas antes da transformação numérica
cnames <- weatherTablePOA[1,]
rnames <- weatherTablePOA[,1]

# conversão dos valores da tabela em numérico
weatherTablePOA <- data.frame(apply(weatherTablePOA, 2, funcCharToNum))

# transforma primeira coluna e primeira linha em títulos
colnames(weatherTablePOA) <- cnames
rownames(weatherTablePOA) <- rnames
weatherTablePOA <- weatherTablePOA[-1,-1]