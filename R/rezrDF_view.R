#A group of functions for viewing rezrDFs
#1)


viewCorePlus = function(df, plus){
  fields = getKey(df) %>% c(getFieldsOfType(df, "core"), plus)
  df %>% select(all_of(fields))
}


viewKeyPlus = function(df, plus){
  fields = getKey(df) %>% c(plus)
  df %>% select(all_of(fields))
}
