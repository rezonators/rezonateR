#Generic functions defined in this package
#1) Functions that apply to rezrDF, rezrObj:
#  a) reload
#  b) addFieldLocal, changeFieldLocal, addFieldForeign, changeFieldForeign, addField, changeField

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

#' Generic functions for easy editing
#'
#' @rdname easygeneric
#' @param x The object to be modified.
#' @param ... Argumnts to be passed to the specific functions.
#'
#' @return
#' @note For details of these functions, see [rezonateR::addFieldLocal.rezrDF] / [rezonateR::addFieldForeign.rezrDF] for rezrDFs and [rezonateR::addFieldLocal.rezrObj] / [rezonateR::addFieldForeign.rezrObj] for rezrObjs.
#' @export
addFieldLocal = function(x, ...){
  UseMethod("addFieldLocal")
}

#' @rdname easygeneric
#' @export
changeFieldLocal = function(x, ...){
  UseMethod("changeFieldLocal")
}

#' @rdname easygeneric
#' @export
addFieldForeign = function(x, ...){
  UseMethod("addFieldForeign")
}

#' @rdname easygeneric
#' @export
changeFieldForeign = function(x, ...){
  UseMethod("changeFieldForeign")
}

#' @rdname acField
#' @export
addField = function(x, ..., foreign = F){
  UseMethod("addField")
}

#' @rdname acField
#' @export
changeField = function(x, ..., foreign = F){
  UseMethod("changeField")
}

#' @rdname addIsWordField
#' @export
addIsWordField = function(x, ...){
  UseMethod("addIsWordField")
}
