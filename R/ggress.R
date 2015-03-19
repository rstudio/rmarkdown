
#' Incremental graphs 
#' 
#' Use this function to "wrap" a ggplot graph which will appear in incremental
#' stages in your presentation.
#' 
#' Usually, you want to begin the presentation with:
#' output: 
#'   ioslides_presentation:
#'     incremental: true
#'     self_contained: false
#'     
#' ... and the block with:
#' \`\`\`\{r, results='asis', error=FALSE, message=FALSE, warning=FALSE, echo=FALSE\}
#' 
#' @param loopVec vector of names or numbers for the incremental stages of the 
#'    graph. Usually, this will just be 1:n, where n is the number of stages.
#' @param expr the graphing command. Usually, but not necessarily, a (composite) 
#'    ggplot command.
#' 
#' @export
#' 
#' @examples
#' \donttest{
#' gginc(1:2, ggplot(mtcars,aes(y=mpg,x=wt)) + geom_point() + stages(geom_blank(),s2=geom_smooth()))
#' }
#' 
gginc = function(loopVec, expr, print.expr=TRUE) {
  cat("<ul class='build gginc'>\n")
  for (igginc__ in loopVec) {
    cat("  <li>\n")
    l = list(gginc__ = igginc__)
    output = eval(substitute(expr),l)
    if (print.expr) {
      print(output)
    }
  }
}

dputToString = function (obj) {
  con <- textConnection(NULL,open="w")
  tryCatch({dput(obj,con);
            textConnectionValue(con)},
           finally=close(con))
}


#' Variable portion within incremental graphs 
#' 
#' Use this function for the portions which change inside a gginc expression.
#' 
#' @param [positional_parameters] correct output for each stage, in sequential order.
#' @param [named_parameters_beginning_with_s] For instance, s5=0 says that beginning with
#'    stage 5 (and until otherwise specified), the output should be 0.
#' @param [named_parameters_beginning_with_o] For instance, o5=0 says that for stage 5
#'    only, the output should be 0.
#'    
#' 
#' @export
#' 
#' @examples
#' \donttest{
#' #4-stage plot: first points only, then add loess, then try linear fit, then return to loess.
#' gginc(1:4, ggplot(mtcars,aes(y=mpg,x=wt)) + geom_point() + stages(geom_blank(),s2=geom_smooth(),o3=geom_smooth(method="lm")))
#' 
#' }
#' 
stages = function(...) {
  stageList = list(...)
  stageNames = names(stageList)
  gginc_ = get("gginc__", parent.frame())
  
  #first, check "o3" format
  onlyName = paste("o",gginc_,sep="")
  if (onlyName %in% stageNames) {
    return(stageList[[onlyName]])
  }
  
  #now, "s4" format
  l = length(stageNames)
  if (l>0) {
    sStages = as.integer(sub("^(s([0-9]*))|.*","\\2",stageNames))
    
    loc = val = -1
    for (i in 1:l) {
      if (!is.na(sStages[i]) & val < sStages[i] & sStages[i] <= gginc_) {
        loc = i
        val = sStages[i]
      }
    }
      
    if (loc > -1) {
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
  if (numBare >= gginc_) {
    return(stageList[[gginc_]])
  } else if (numBare == 0) {
    return(NULL)
  }
  return(stageList[[numBare]])
}

#ggnull = annotate("text")

#gginc(1:2, ggplot(mtcars,aes(y=mpg,x=wt)) + geom_point() + stages(ggnull,s2=geom_smooth(method="lm")))


