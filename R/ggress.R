
#' @export
ggr = function(loopVec, expr) {
  cat("<ul class='build ggress'>\n")
  #print(substitute(expr))
  #print(eval(substitute(expr)))
  for (iggr__ in loopVec) {
    cat("  <li>\n")
    #cat(iggr__,"\n")
    l = list(ggr__ = iggr__)
    print(eval(substitute(expr),l))
    cat(" blah\n")
    cat("  <br/><b>hi</b>\n")
    cat("  <\\li>\n")
  }
  cat("<\\ul>\n")
}

dputToString = function (obj) {
  con <- textConnection(NULL,open="w")
  tryCatch({dput(obj,con);
            textConnectionValue(con)},
           finally=close(con))
}


#' @export
stages = function(...) {
  stageList = list(...)
  stageNames = names(stageList)
  ggr_ = get("ggr__", parent.frame())
  #cat("ggr_:",ggr_,"\n")
  
  #first, check "o3" format
  onlyName = paste("o",ggr_,sep="")
  if (onlyName %in% stageNames) {
    return(stageList[[onlyName]])
  }
  
  #now, "s4" format
  l = length(stageNames)
  #cat("l:",dputToString(l),"\n")
  #cat("   stageList:",dputToString(stageList),"\n")
  #cat("stageNames:",dputToString(stageNames),"\n")
  #cat("sStages:",dputToString(sStages),"\n")
  if (l>0) {
    sStages = as.integer(sub("^(s([0-9]*))|.*","\\2",stageNames))
    
    #cat("sStages:",dputToString(sStages),"\n")
    
    loc = val = -1
    for (i in 1:l) {
      #cat("sStages[i]:",dputToString(sStages[i]),"\n")
      if (!is.na(sStages[i]) & val < sStages[i] & sStages[i] <= ggr_) {
        loc = i
        val = sStages[i]
      }
    }
      
    if (loc > -1) {
      #cat("loc:",dputToString(loc),"\n")
      return(stageList[[loc]])
    }
  }
  
  #now, bare arguments
  if (l > 0) {
    bareStages = grep(".",stageNames,value=TRUE,invert=TRUE)
    numBare = length(bareStages)
  } else {
    numBare = length(stageList)
  }
  if (numBare >= ggr_) {
    #cat("bare1")
    return(stageList[[ggr_]])
  } else if (numBare == 0) {
    #cat("bare2",stageNames)
    return(NULL)
  }
  #cat("bare3")
  return(stageList[[numBare]])
}

#ggnull = annotate("text")

#ggr(1:2, ggplot(mtcars,aes(y=mpg,x=wt)) + geom_point() + stages(ggnull,s2=geom_smooth()))

