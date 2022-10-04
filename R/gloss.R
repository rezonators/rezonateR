###These functions will be made available to the public later, but will remain unexported for now

copyTokenLinkers = function(rezrObj, fromCol, toCol){
  rezrObj$tokenDF = rezrObj$tokenDF %>% rez_mutate(across(all_of(toCol), function(col) case_when(
    substring(col, 1, 1) == "-" ~ "-" %+% eval(parse_expr(fromCol)),
    substring(col, 1, 1) == "=" ~ "=" %+% eval(parse_expr(fromCol)),
    T ~ col
  )))
  rezrObj
}

removeTokenSpaces = function(rezrObj, fromCol){
  rezrObj$tokenDF = rezrObj$tokenDF %>% rez_mutate(across(all_of(fromCol), function(col) col %>% chompSuffix(" ") %>% str_replace_all(" ", ".")))
  rezrObj
}

