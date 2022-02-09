defaultScheme = c(turnStart = "", turnEnd = "turnEnd", turn = "turnEnd",
                  bcStart = "bcStart", bcEnd = "bcEnd", bc = "bc",
                  siorInitiate = "SIORInit", sior = "SIOR", siorStart = "SIORStart", siorEnd = "SIOREnd",
                  noTurn = "noTurn", noTurnStart = "noTurnStart", noTurnEnd = "noTurnEnd")

#' Get turn IDs from turn position annotations
#'
#' @param unitDF
#' @param annoScheme A character vector specifying the annotations used for the start and end of turns, backchannels, (self-initiated) other-repairs, and extra-turn content, as well as single-line instances of such. The default scheme is as follows: c(turnStart = "", turnEnd = "turnEnd", turn = "turnEnd", bcStart = "bcStart", bcEnd = "bcEnd", bc = "bc", siorInitiate = "SIORInit", sior = "SIOR", siorStart = "SIORStart", siorEnd = "SIOREnd", noTurn = "noTurn", noTurnStart = "noTurnStart", noTurnEnd = "noTurnEnd"). If you wish to deviate from this default, simply enter a vector with the deviating entries, e.g. c(turnStart = "turnStart", turn = "turn") if you want to annotate turn starts and single-unit turns directly.
#' @param sior If new, then self-initiated other-repairs are treated as a new turn.
#' @param bc If 'noTurn', then backchannels are treated as turnless. If 'turn', then backchannels belong to the previous turn.
#' @param turnAnno The field name of the turn annotations in the Rez file.
#'
#' @return
#' @export
#'
#' @examples
getTurnFromAnnos = function(unitDF, annoScheme = defaultScheme, sior = "new", bc = "noTurn", turnAnno = "turnPos"){
  for(cat in names(defaultScheme)){
    if(!(cat %in% annoScheme)) annoScheme[[cat]] = defaultScheme[[cat]]
  }

  turnEndCats = c(annoScheme[["turnEnd"]])
  if(sior == "new") turnEndCats = c(turnEndCats, annoScheme[["siorInitiate"]], annoScheme[["sior"]], annoScheme[["siorEnd"]])

  noTurnCats = c(annoScheme[["noTurn"]], annoScheme[["noTurnStart"]])
  if(bc == "noTurn") noTurnCats = c(noTurnCats, annoScheme[["bcStart"]], annoScheme[["bc"]])

  noTurnStatus = c("noTurn")
  if(bc == "noTurn") noTurnStatus = c(noTurnStatus, "noTurn")
  if(sior == "old") noturnStatus = c(noTurnStatus, "sior")


  turnList = list()
  for(currPart in unique(unique(unitDF$participant))){
    currDF = unitDF %>% filter(participant == currPart)
    currTurn = 1
    currStatus = "turn"
    for(i in 1:nrow(currDF)){
      currAnno = currDF %>% slice(i) %>% pull(turnAnno)

      #bcStart and siorStart
      if(currAnno == annoScheme[["bcStart"]]){
        currStatus = "bc"
      } else if(currAnno == annoScheme[["siorStart"]]){
        currStatus = "sior"
      } else if(currAnno == annoScheme[["noTurnStart"]]){
        currStatus = "noTurn"
      } else if(currStatus == "bc" & currAnno == annoScheme[["bcEnd"]]){
        currStatus = "turn"
      } else if(currStatus == "sior" & currAnno == annoScheme[["siorEnd"]]){
        currStatus = "turn"
      } else if(currStatus == "noTurn" & currAnno == annoScheme[["noTurnEnd"]]){
        currStatus = "turn"
      }

      if(!(currAnno %in% noTurnCats) & !(currStatus %in% noTurnStatus)){
        turnList[[currDF %>% slice(i) %>% pull(id)]] = currTurn
      } else {
        print("hi")
        turnList[[currDF %>% slice(i) %>% pull(id)]] = 0
      }

      #If turn end, increase the turn no
      if(currAnno %in% turnEndCats) currTurn = currTurn + 1
    }
  }

  unitDF = unitDF %>% mutate(turnProvisional = sapply(unitDF$id, function(id) turnList[[id]]))
  print(unitDF$turnProvisional)
  allTurns = unitDF %>% filter(turnProvisional != 0) %>% group_by(participant, turnProvisional) %>% summarise(firstUnit = min(as.numeric(unitId))) %>% arrange(as.numeric(firstUnit)) %>% ungroup
  allTurns = allTurns %>% mutate(turnFinal = 1:nrow(allTurns))
  unitDF = unitDF %>% left_join(allTurns, by = c("turnProvisional", "participant"))
  unitDF$turnFinal
}
