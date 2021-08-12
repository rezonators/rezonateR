#A group of functions for dealing with issues like coreference resolution, reference production, referentiality, and more!
#1) Functions related to previous context:
#  a) (Distance to) last mention: lastMention, distToLastMention


#' Functions related to previous context in track chains.
#'
#' @rdname trackPrevContext
#' @param unitSeq The vector of units where the mentions appeared.
#' @param chain The chain that each mention belongs to.
#' @export
lastMention = function(unitSeq = NULL, chain = NULL){
  #Get the default column names from the rezrDF environment
  df_env = env_parent(caller_env())
  if(is.null(unitSeq)) unitSeq = df_env[["unitSeqLast"]]
  if(is.null(chain)) chain = df_env[["chain"]]

  result = numeric(length(unitSeq))
  for(currChain in unique(chain)){
    chainUnits = unitSeq[chain == currChain] #Grab all the PRE's units
    unitsOrdered = sort(chainUnits) #Put them in the right order
    unitsOrderedLagged = lag(unitsOrdered) #Get the previous unit
    result[chain == currChain] = unitsOrderedLagged[rank(chainUnits)] #Put it back in the wrong order
  }
  result
}

#' @rdname trackPrevContext
#' @export
distToLastMention = function(unitSeq = NULL, chain = NULL){
  #Get the default column names from the rezrDF environment
  df_env = env_parent(caller_env())
  if(is.null(unitSeq)) unitSeq = df_env[["unitSeqLast"]]
  if(is.null(chain)) chain = df_env[["chain"]]

  unitSeq - lastMention(unitSeq, chain)
}


