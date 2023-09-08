#A series of functions for handling export to and from CSVs.

#' Read and write `rezrDF`s as CSV files
#'
#' Replacement of readr [readr::read_csv()] and [readr::write_csv()], but contains added functionality for dealing with `rezrDF`s more easily.
#'
#' @rdname rw
#' @param df The `rezrDF` to be exported.
#' @param path The path from which a CSV is to be imported / exported.
#' @param inclCols Columns to be included from the import / export.
#' @param exclCols Columns to be excluded from the import / export.
#' @param lubridate Are you using the lubridate package?
#' @param origDF The `rezrDF` that originally produced this CSV, used to identify data types of the columns.
#' @param ... Arguments passed onto [readr::read_csv()] and [readr::write_csv()]
#'
#' @return For `rez_read_csv()`, a data.frame for the CSV being imported. This is NOT a `rezrDF`, so please do not assign it to a `rezrObj`; use [rezonateR::updateFromDF()] to update an existing rezrDF with the imported data.frame.
#' @examples
#' #rez_write_csv(sbc007$trackDF$default, "rez007_refexpr.csv", c("id", "tokenOrderLast", "text", "name"))
#' inpath = system.file("extdata", "rez007_refexpr_edited.csv", package = "rezonateR")
#' changeDF = rez_read_csv(inpath, origDF = rez007$trackDF$default)
#' @note Includes a UTF-8 byte mark by default, so you can open the CSV directly in Excel. Whenever you export a .csv with the intention of importing it back, you must export the `id` column so that the resulting table can be merged back.
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

  #Handling cases with commas in column names
  inString = F
  currPrevQuote = 0
  i = 1
  for(item in inImport){
    numQuotes = str_count(item, "\"")
    if(numQuotes/2 != round(numQuotes/2)){
      if(!inString){
        currPrevQuote = i
      } else {
        inImport[[currPrevQuote]] = paste(inImport[currPrevQuote:i], collapse = ",") %>% str_remove_all("\\\"")
        inImport = c(inImport[1:currPrevQuote],inImport[(i+1):length(inImport)])
        i = currPrevQuote
      }
      inString = !inString
    }
    i = i + 1
  }
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

#' Update a rezrDF using data from another data frame
#'
#' @param targetDF The target rezrDF.
#' @param changeDF A data frame, not necessarily a rezrDF, from which changes will be pulled.
#' @param changeCols Columns to be changed. This should include names of columns to be added.
#' @param changeType Which types of columns (in field access terms) will you change?
#' @param renameCols Will you rename columns according to the new data frame?
#' @param colCorr If renameCols = T, then a list where names are the new names and values are the old names. If renameCols = F, then the opposite.
#' @param delRows Will you delete rows from targetDF if not present in changeDF?
#' @param addRows Will you add rows to targetDF if not present in targetDF?
#' @param addCols Will you add columns present in the changeDF but not in the targetDF?
#' @param reloadAfterCorr Would you like to do a local reload on the rezrDF afterwards (if a rezrObj is not supplied) or a full reload (if a rezrObj is supplied)?
#' @param rezrObj The rezrObj, if you would like to do a full reload.
#'
#' @return The updated rezrDF.
#' @export
#' @examples
#' inpath = system.file("extdata", "rez007_refexpr_edited.csv", package = "rezonateR")
#' changeDF = rez_read_csv(inpath, origDF = rez007$trackDF$default)
#' sbc007$trackDF$default = sbc007$trackDF$default %>% updateFromDF(changeDF, addCols = T)
#' @note Most often used for updating a rezrDF using data from a CSV used for manual annotation.
updateFromDF = function(targetDF, changeDF, changeCols = NULL, changeType = "flex", renameCols = F, colCorr = list(), delRows = F, addRows = F, addCols = F, reloadAfterCorr = T, rezrObj = NULL){
  #TODO: Test column name changes
  if(length(colCorr) > 1){
    if(renameCols){
      argList = c(list(df = targetDF), colCorr)
      targetDF = exec("rez_rename", !!!argList)
    } else {
      argList = c(list(df = changeDF), colCorr)
      changeDF = exec("rez_rename", !!!argList)
    }
  }

  if(is.null(changeCols)){
    if(!addCols){
      changeCols = colnames(changeDF) %>% intersect(colnames(targetDF)) %>% setdiff(getKey(targetDF))
    } else {
      changeCols = colnames(changeDF) %>% setdiff(getKey(targetDF)) %>% intersect(getFieldsOfType(targetDF, changeType)) %>% union(setdiff(colnames(changeDF), colnames(targetDF)))
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
  } else {
    if(getKey(targetDF) %in% colnames(changeDF)){
      changeDF = changeDF %>% filter(changeDF[[getKey(targetDF)]] %in% targetDF[[getKey(targetDF)]])
    }
  }

  joinDF = suppressMessages(rez_left_join(targetDF %>% select(all_of(c(getKey(targetDF)))), changeDF %>% select(all_of(c(getKey(targetDF), changeCols))), by = getKey(targetDF)))

  result = targetDF
  for(col in changeCols) result = result %>% mutate(!!parse_expr(col) := joinDF[[col]])
  if(reloadAfterCorr)
    if(is.null(rezrObj)) result = reloadLocal(result) else result = reload(result, rezrObj)
  result

}
