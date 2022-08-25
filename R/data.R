#' A Tree's Life - preliminary annotation
#'
#' A `rezrObj` that comes from a preliminary annotation of part of the document 'A Tree's Life' from the
#' Santa Barbara Corpus of Spoken American English (SBC). It has been annotated for resonance and coreference.
#' Coreference chains include referential expressions along with non-referring expressions that have been otherwise
#' referred back to (e.g. discourse deixis). Two-layered trees indicate argument structure of each verb. The verbal complex (including
#' auxiliaries and adverbs that intervene between the auxiliary and verb, but not arguments that intervene between auxiliary
#' and verb) is annotated as blank chunks (with `chunkType = "verb"`) and serves as the root of each tree. Tree links are annotated as `Relation = "Subject"`
#' when it indicates a subject-verb relation; otherwise `Relation` is left blank.
#'
#' @format A `rezrObj` with the following elements: `nodeMap`, `chunkDF`, `docDF`, `entryDF`, `linkDF`, `mergedDF`, `resonanceDF`, `rezDF`,
#' `tokenDF`, `trackDF`, `trailDF`, `treeDF`, `treeEntryDF`, `treeLinkDF`, `unitDF`
"sbc007"

