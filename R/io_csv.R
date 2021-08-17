#A series of functions for handling export to and from CSVs.

#' Read and write rezrDFs as CSV files
#'
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
