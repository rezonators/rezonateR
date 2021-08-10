#Generic functions defined in this package
#1) reload (applies to rezrDF, rezrObj)

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
