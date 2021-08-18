#A series of functions for handling export to and from CSVs.

#' Read and write rezrDFs as CSV files
#'
#' @rdname rw
#' @param df The rezrDF to be exported.
#' @param path The path from which a CSV is to be imported / exported.
#' @param inclCols Columns to be included from the import / export.
#' @param exclCols Columns to be excluded from the import / export.
#' @param lubridate Are you using the lubridate package?
#' @param origDF The rezrDF that originally produced this CSV, used to identify data types of the columns.
#' @param ... Arguments passed onto read_csv and write_csv.
#'
#' @return For rez_read_csv, the CSV being imported.
#' @export
rez_write_csv = function(df, path, inclCols = character(0), exclCols = character(0), ...){
  if(length(inclCols) > 0){
    if(length(exclCols) > 0){
      warning("You specified columns to both include and exclude. I will ignore the exclude parameter.")
    }
  } else {
    inclCols = setdiff(colnames(df), exclCols)
  }

  write_excel_csv(df %>% select(all_of(inclCols)), path, ...)
}

#' @rdname rw
#' @export
rez_read_csv = function(path, origDF = NULL, lubridate = F, inclCols = character(0), exclCols = character(0), ...){
  inImport = strsplit(read_lines(path, n_max = 1), ",")[[1]]
  if(length(inclCols) > 0){
    if(length(exclCols) > 0){
      warning("You specified columns to both include and exclude. I will ignore the exclude parameter.")
    }
    inclCols = intersect(inImport, inclCols)
  } else {
    inclCols = setdiff(inImport, exclCols)
  }

  if(!("col_types" %in% names(list(...))) & !is.null(origDF)){
    oldNames = colnames(origDF)
    colTypes = sapply(inclCols, function(x){
      result = col_guess()
      if(x %in% oldNames){
        if(is.character(origDF[[x]])) result = col_character()
        else if(is.logical(origDF[[x]])) result = col_logical()
        else if(is.integer(origDF[[x]])) result = col_integer()
        else if(is.double(origDF[[x]])) result = col_double()
        else if(is.numeric(origDF[[x]])) result = col_number()
        else if(is.date(origDF[[x]])) result = col_date()
        else if(is.time(origDF[[x]])) result = col_time()
        else if(lubridate){
          if(lubridate::is.instant(origDF[[x]]) | lubridate::is.timepoint(origDF[[x]])){
            result = col_datetime()
          }
        }
      }
      result
    })
  } else {
    colTypes = sapply(inclCols, function(x) col_guess())
  }

  newDF = read_csv(path, col_types = exec("cols_only", !!!colTypes), ...)
  newDF
}

updateFromDF = function(targetDF, changeDF, changeCols = NULL, changeType = "flex", renameCols = F, colCorr = list(), delRows = F, addRows = F, addCols = F, reloadAfterCorr = T){
  #TODO: Test column name changes
  if(length(colCorr) > 1){
    if(renameCols){
      argList = c(list(df = targetDF), colCorr)
      print(argList)
      targetDF = exec("rez_rename", !!!argList)
    } else {
      argList = c(list(df = changeDF), colCorr)
      print(argList)
      changeDF = exec("rez_rename", !!!argList)
    }
  }

  if(is.null(changeCols)){
    if(!addCols){
      changeCols = colnames(changeDF) %>% intersect(colnames(targetDF)) %>% setdiff(getKey(targetDF))
    } else {
      changeCols = colnames(changeDF) %>% setdiff(getKey(targetDF)) %>% intersect(getFieldsOfType(targetDF, changeType))
    }
  } else {
    if(getKey(targetDF) %in% changeCols) stop("You cannot change a key field.")
  }

  targetIDs = targetDF[[getKey(targetDF)]]
  changeIDs = changeDF[[getKey(targetDF)]]
  if(delRows){
    targetDF = targetDF %>% filter(!!parse_expr(getKey(targetDF)) %in% changeIDs)
  }
  if(addRows){
    targetDF = targetDF %>% rez_add_row(!!parse_expr(getKey(targetDF)) := setdiff(changeIDs, targetIDs))
  }

  joinDF = suppressMessages(rez_left_join(targetDF %>% select(all_of(c(getKey(targetDF)))), changeDF %>% select(all_of(c(getKey(targetDF), changeCols))), by = getKey(targetDF)))

  result = targetDF
  for(col in changeCols) result = result %>% mutate(!!parse_expr(col) := joinDF[[col]])
  result
}
