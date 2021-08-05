`%+%` = function(a, b) paste0(a, b)

flatten_expr = function(x, includeFunct = T){
  x_list = as.list(x)
  result = character(0)

  if(!includeFunct){
    start = 2
  } else {
    start = 1
  }
  for(i in start:length(x_list)){
    item = x[[i]]
    if(length(as.list(item)) > 1){
      #print("Still some ways to pgo!")
      #print(item)p
      result = c(result, flatten_expr(item))
    } else {
      #print("Reached a bottom!")
      #print(item)
      result = c(result, deparse(item))
    }
  }
  result
}
