#' @title Summary for hypr contrasts
#'
#' @description Extract the estimates and the inferential tests for a contrasts
#'              defined by \code{\link[hypr::hyper]{hypr::hyper}}
#'
#'
#' @param model a model of class \code{\link[stats::lm]{stats::lm}}, \code{\link[stats::lm]{stats::glm}}
#'       or \code{\link[lme4::lmer]{lmer::lmer}}
#'
#' @return A results table of class `hypr_contrasts_results`.
#'
#' @examples
#'
#' \dontrun{

#' }
#' @rdname contrast_summary
#' @export

contrasts_summary<-function(x,...) UseMethod("contrasts_summary")

#' @rdname contrast_summary
#' @export

contrasts_summary.default<-function(model) {

  .vars        <-  attr(terms(model),"dataClasses")
  .facts       <-  names(.vars[.vars=="factor"])
  .data        <-  model$model
  .hyp_factors <-  sapply(.facts,function(x) "hypr_cmat" %in% class(contrasts(.data[[x]])))
  .hyp_factors <-  names(.hyp_factors[.hyp_factors==TRUE])
  .summary    <-  summary(model)
  results<-sapply(.hyp_factors,function(f) {
    .cont    <-  contrasts(.data[[f]])
    .cols    <-  colnames(.cont)
    if (is.null(.cols)) .cols<-1:ncol(.cont)
    .names   <-  paste0(f,.cols)
    .hyp     <-  cmat2eqs(.cont)
    .res     <-  subset(.summary$coefficients,rownames(.summary$coefficients) %in% .names)
    .fillers <-  attr(.cont,"which_filler")
    if (!is.null(.fillers)) {
      .index   <-  (1:nrow(.res)!=.fillers)
      .res     <-  subset(.res,.index)
      .hyp     <-  .hyp[-.fillers]
    }
    rownames(.res)  <-  .hyp
    .res
  },simplify = FALSE)
  if (length(results)==0) warning("No hypr contrast found")
  class(results)<-c("hypr_contrasts_results",class(results))
  results
}

#' @rdname contrast_summary
#' @export

contrasts_summary.merMod<-function(model) {

   message("Contrasts for the mixed models")
  .vars        <-  attr(terms(model),"term.labels")
  .data        <-  model@frame
  .hyp_factors <-  sapply(.vars,function(x) {
                    if (is.factor(.data[[x]]))
                        return("hypr_cmat" %in% class(contrasts(.data[[x]])))
                    else
                        return(FALSE)
                    })

  .hyp_factors <-  names(.hyp_factors[.hyp_factors==TRUE])
  .summary    <-  summary(model)
  results<-sapply(.hyp_factors,function(f) {
    .cont    <-  contrasts(.data[[f]])
    .cols    <-  colnames(.cont)
    if (is.null(.cols)) .cols<-1:ncol(.cont)
    .names   <-  paste0(f,.cols)
    .hyp     <-  cmat2eqs(.cont)
    .res     <-  subset(.summary$coefficients,rownames(.summary$coefficients) %in% .names)
    .fillers <-  attr(.cont,"which_filler")
    if (!is.null(.fillers)) {
      .index   <-  (1:nrow(.res)!=.fillers)
      .res     <-  subset(.res,.index)
      .hyp     <-  .hyp[-.fillers]
    }
    rownames(.res)  <-  .hyp
    .res
  },simplify = FALSE)
  if (length(results)==0) warning("No hypr contrast found")
  class(results)<-c("hypr_contrasts_results",class(results))
  results
}

#' @rdname contrast_summary
#' @export

contrasts_summary.glmerMod<-function(model) contrasts_summary.merMod(model)

#' @title Print for hypr contrasts
#'
#' @description Pretty print the estimates and the inferential tests for a contrasts
#'              defined by \code{\link[hypr::hyper]{hypr::hyper}}
#'
#'
#' @param an object of class `hypr_contrasts_results`
#'
#' @return A results table
#'
#' @examples
#'
#' \dontrun{

#' }

#' @export

print.hypr_contrasts_results<-function(obj) {

  lapply(names(obj),function(x) {
    cat("\nContrasts results for factor:",x,"\n")
    print(obj[[x]])
    cat("\n")
  })
}

