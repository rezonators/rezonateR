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
#' @return The modified rezrDF. reloadLocal reloads only auto fields; reloadForeign reloads only foreign fields. Foreign fields are always updated before auto fields. If the fields parameter is not available, I will find the best update order; otherwise, I will use the order you provide except for the rule where foreign fields are updated first.
#' @note reload.rezrObj is coming soon.
#' @export
reload = function(x, ...){
  UseMethod("reload")
}

#' @rdname acFieldLocal
#' @param x The object (rezrDF or rezrObj) to be modified.
#' @param ... Arguments to be passed to the four specific functions.
#' @export
addFieldLocal = function(x, ...){
  UseMethod("addFieldLocal")
}

#' @rdname acFieldLocal
#' @export
changeFieldLocal = function(x, ...){
  UseMethod("changeFieldLocal")
}

#' @rdname acFieldForeign
#' @export
addFieldForeign = function(x, ...){
  UseMethod("addFieldForeign")
}

#' @rdname acFieldForeign
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

#' @rdname rmField
#' @export
removeField = function(x, ...){
  UseMethod("removeField")
}

#' @rdname addIsWordField
#' @export
addIsWordField = function(x, ...){
  UseMethod("addIsWordField")
}

#' @rdname addRow
#' @export
addRow = function(x, ...){
  UseMethod("addRow")
}

