`%+%` = function(a, b) paste0(a, b)

flatten_expr = function(x){
  x_list = as.list(x)
  result = character(0)
  for(i in 1:length(x_list)){
    item = x[[i]]
    if(length(as.list(item)) > 1){
      #print("Still some ways to go!")
      #print(item)
      result = c(result, flatten_expr(item))
    } else {
      #print("Reached a bottom!")
      #print(item)
      result = c(result, deparse(item))
    }
  }
  result
}
