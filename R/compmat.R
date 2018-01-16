#' @import epiR
pcmatrix = function(df){
  res = df %>% group_by(group) %>% do({
    X = acast(., rowSeq ~ colSeq, value.var = "value")
    arrayNames = acast(., rowSeq ~ colSeq, value.var = "arrayNames")[1,]
    cm = cor(X)
    dimnames(cm) = list(X = arrayNames, Y = arrayNames)
    cordf = melt(cm)
  })
  return(res)
}

ccc.mat = function(X){
  m = matrix(nrow = dim(X)[2], ncol = dim(X)[2])
  for (i in 1:dim(m)[1]){
    for(j in 1:dim(m)[1]){
      m[i,j] = as.numeric(epi.ccc(X[,i], X[,j])$rho.c[1])
    }
  }
  return(m)
}

lccmatrix = function(df){
  res = df %>% group_by(group) %>% do({
    X = acast(., rowSeq ~ colSeq, value.var = "value")
    arrayNames = acast(., rowSeq ~ colSeq, value.var = "arrayNames")[1,]
    cm = ccc.mat(X)
    dimnames(cm) = list(X = arrayNames, Y = arrayNames)
    cordf = melt(cm)
  })
  return(res)
}
