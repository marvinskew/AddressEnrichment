replaceNESW_withNA1= function(x){
  if(!"stringr" %in% installed.packages())install.packages(stringr, dependencies = TRUE)
  x<-ifelse(str_detect(x, "[[:space:]]N$"),gsub("[[:space:]]N$", "", x),
            ifelse(str_detect(x, "[[:space:]]E$"), gsub("[[:space:]]E$", "", x),
                   ifelse(str_detect(x, "[[:space:]]S$"), gsub("[[:space:]]S$", "", x),
                          ifelse(str_detect(x, "[[:space:]]W$"),gsub("[[:space:]]W$", "", x),x))))
  x<-trimws(x)
  return(x)
}

replaceNESW_withNA2= function(x){
  if(!"stringr" %in% installed.packages())install.packages(stringr, dependencies = TRUE)
  x<-ifelse(str_detect(x, "[[:space:]]N.$"),gsub("[[:space:]]N.$", "", x),
            ifelse(str_detect(x, "[[:space:]]E.$"), gsub("[[:space:]]E.$", "", x),
                   ifelse(str_detect(x, "[[:space:]]S.$"), gsub("[[:space:]]S.$", "", x),
                          ifelse(str_detect(x, "[[:space:]]W.$"),gsub("[[:space:]]W.$", "", x),x))))
  x<-trimws(x)
  return(x)
}

replaceNESW_withNA3= function(x){
  if(!"stringr" %in% installed.packages())install.packages(stringr, dependencies = TRUE)
  x<-ifelse(str_detect(x, "[[:space:]]NE$"),gsub("[[:space:]]NE$", "", x),
            ifelse(str_detect(x, "[[:space:]]SE$"), gsub("[[:space:]]SE$", "", x),
                   ifelse(str_detect(x, "[[:space:]]SW$"), gsub("[[:space:]]SW$", "", x),
                          ifelse(str_detect(x, "[[:space:]]NW$"),gsub("[[:space:]]NW$", "", x),x))))
  x<-trimws(x)
  return(x)
}

replaceNESW_withNA4= function(x){
  if(!"stringr" %in% installed.packages())install.packages(stringr, dependencies = TRUE)
  x<-ifelse(str_detect(x, "[[:space:]]NE.$"),gsub("[[:space:]]NE.$", "", x),
            ifelse(str_detect(x, "[[:space:]]SE.$"), gsub("[[:space:]]SE.$", "", x),
                   ifelse(str_detect(x, "[[:space:]]SW.$"), gsub("[[:space:]]SW.$", "", x),
                          ifelse(str_detect(x, "[[:space:]]NW.$"),gsub("[[:space:]]NW.$", "", x),x))))
  x<-trimws(x)
  return(x)
}



replaceNESW_withNA5= function(x){
  if(!"stringr" %in% installed.packages())install.packages(stringr, dependencies = TRUE)
  x<-ifelse(str_detect(x, "[[:space:]]+N[[:space:]]+$"),gsub("[[:space:]]+N[[:space:]]+$", "", x),
            ifelse(str_detect(x, "[[:space:]]+E[[:space:]]+$"), gsub("[[:space:]]+E[[:space:]]+$", "", x),
                   ifelse(str_detect(x, "[[:space:]]+S$[[:space:]]+"), gsub("[[:space:]]+S$[[:space:]]+", "", x),
                          ifelse(str_detect(x, "[[:space:]]+W[[:space:]]+$"),gsub("[[:space:]]+W[[:space:]]+$", "", x),x))))
  x<-trimws(x)
  return(x)
}

replaceNESW_withNA6= function(x){
  if(!"stringr" %in% installed.packages())install.packages(stringr, dependencies = TRUE)
  x<-ifelse(str_detect(x, "[[:space:]]+N.[[:space:]]+$"),gsub("[[:space:]]+N.[[:space:]]+$", "", x),
            ifelse(str_detect(x, "[[:space:]]+E.[[:space:]]+$"), gsub("[[:space:]]+E.[[:space:]]+$", "", x),
                   ifelse(str_detect(x, "[[:space:]]+S.[[:space:]]+$"), gsub("[[:space:]]+S.[[:space:]]+$", "", x),
                          ifelse(str_detect(x, "[[:space:]]+W.[[:space:]]+$"),gsub("[[:space:]]+W.[[:space:]]+$", "", x),x))))
  x<-trimws(x)
  return(x)
}

replaceNESW_withNA7= function(x){
  if(!"stringr" %in% installed.packages())install.packages(stringr, dependencies = TRUE)
  x<-ifelse(str_detect(x, "[[:space:]]+NE[[:space:]]+$"),gsub("[[:space:]]+NE[[:space:]]+$", "", x),
            ifelse(str_detect(x, "[[:space:]]+SE[[:space:]]+$"), gsub("[[:space:]]SE[[:space:]]+$", "", x),
                   ifelse(str_detect(x, "[[:space:]]SW[[:space:]]+$"), gsub("[[:space:]]SW[[:space:]]+$", "", x),
                          ifelse(str_detect(x, "[[:space:]]NW[[:space:]]+$"),gsub("[[:space:]]NW[[:space:]]+$", "", x),x))))
  x<-trimws(x)
  return(x)
}

replaceNESW_withNA8= function(x){
  if(!"stringr" %in% installed.packages())install.packages(stringr, dependencies = TRUE)
  x<-ifelse(str_detect(x, "[[:space:]]+NE.[[:space:]]+$"),gsub("[[:space:]]+NE.[[:space:]]+$", "", x),
            ifelse(str_detect(x, "[[:space:]]+SE.[[:space:]]+$"), gsub("[[:space:]]SE.[[:space:]]+$", "", x),
                   ifelse(str_detect(x, "[[:space:]]SW.[[:space:]]+$"), gsub("[[:space:]]SW.[[:space:]]+$", "", x),
                          ifelse(str_detect(x, "[[:space:]]NW.[[:space:]]+$"),gsub("[[:space:]]NW.[[:space:]]+$", "", x),x))))
  x<-trimws(x)
  return(x)
}



