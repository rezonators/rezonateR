#Generic functions defined in this package
#1) Functions that apply to rezrDF, rezrObj:
#  a) reload
#  b) addFieldLocal, addFieldForeign
#  c) changeFieldLocal, changeFieldForeign

#' Functions for reloading auto and foreign fields.
#'
#' @rdname reload
#' @param df The rezrDF you want to modify.
#' @param rezrObj The entire rezrObj.
#' @param fields The fields of the rezrDF you want to modify.
#'
#' @return The modified rezrDF. Foreign fields are always updated before auto fields. If the fields parameter is not available, I will find the best update order; otherwise, I will use the order you provide except for the rule where foreign fields are updated first.
#' @export
reload = function(x, ...){
  UseMethod("reload")
}

addField = function(x, ...){
  UseMethod("addFieldLocal")
}

addFieldLocal = function(x, ...){
  UseMethod("addFieldLocal")
}

addFieldForeign = function(x, ...){
  UseMethod("addFieldForeign")
}

