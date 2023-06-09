

#' use curatedTCGAData to produce survival time structure and mutation matrix for a TCGA tumor type
#' @import survival
#' @import curatedTCGAData
#' @importFrom TCGAutils TCGAprimaryTumors
#' @param project character(1), defaults to "BRCA"
#' @param min.numevents numeric(1), defaults to 10; fail if there are fewer than this number of events overall
#' @return a list with components surv, coldata, and mutdata
#' @note observations lacking positive follow up time are silently omitted
#' @examples
#' requireNamespace("survival")
#' br = build_surv_for_mut("BRCA")
#' br
#' has_TTN = apply(br$mutdata, 2, function(x) any(x == "TTN", na.rm=TRUE))
#' fi = survival::survfit(br$surv ~ has_TTN)
#' plot(fi, lwd=2, col=c("blue", "orange"), xlab = "t = Years from diagnosis", ylab="S(t) = Prob(survive beyond t)")  # KM-plot
#' legend(.1, .3, lwd=2, col=c("blue", "orange"), lty=1, legend=c("TTN wild-type", "TTN-mutant"), bty="n")
#' title("TCGA BRCA survival")
#' @export
build_surv_for_mut = function(project = "BRCA", min.numevents=10) {
  proj = curatedTCGAData(project, assays = "mutation", version="2.0.1", dry.run=FALSE)
  proj = TCGAprimaryTumors(proj)
  bmut = experiments(proj)[[1]]
  cd = colData(proj)
  bcn = substr(colnames(bmut),1,12)
  ok = intersect(bcn, rownames(cd))
  cc = compactAssay(bmut)
  cc2 = cc
  colnames(cc2) = substr(colnames(cc2),1,12)
  cc3 = cc2[,ok]
  cdok = cd[ok,]
  chkbad = which(is.na(cdok$days_to_death)&is.na(cdok$days_to_last_followup))
  posfu = cdok
  if (length(chkbad)>0) posfu = cdok[-chkbad,]
  ok = intersect(rownames(posfu), colnames(cc3))
  stopifnot(length(ok)>1)
  cc4 = cc3[, ok] #rownames(posfu)]
  time = pmax(posfu$days_to_death, posfu$days_to_last_followup, na.rm=TRUE)
  posfu = posfu[which(time>0),]
  numev = sum(posfu$vital_status)
  if (numev < min.numevents) stop(paste0("only ", numev, " events, less than min.numevents"))
  cc5 = cc4[,rownames(posfu)]
  library(survival)
  time2 = pmax(posfu$days_to_death, posfu$days_to_last_followup, na.rm=TRUE)
  ans = list(surv=Surv(time2/365.25, posfu$vital_status), coldata=cdok, mutdata=cc5, project=project)
  class(ans) = c("TCGAmutsurv", "list")
  ans
}

#' print for TCGAmutsurv
#' @param x instance of TCGAmutsurv
#' @param digits not used
#' @param quote not used
#' @param na.print not used 
#' @param print.gap not used 
#' @param right not used 
#' @param max not used 
#' @param width not used 
#' @param useSource not used 
#' @param \dots not used
#' @note Individuals may harbor multiple mutations in genes; all events are counted.
#' @export
print.TCGAmutsurv = function(x, digits = NULL, quote = TRUE, na.print = NULL, print.gap = NULL, 
    right = FALSE, max = NULL, width = NULL, useSource = TRUE, 
    ...) {
 cat(sprintf("TCGAmutsurv instance for project %s, %d samples.\n", x$project, nrow(x$coldata)))
 cat("Most commonly mutated genes:\n")
 print(tail(sort(table(as.character(x$mutdata)))))
}

#' survival fit for TCGAmutsurv
#' @param x instance of TCGAmutsurv
#' @param genesym character(1) presence or absence of mutation in this gene is used as stratifier
#' @examples
#' sk = build_surv_for_mut("SKCM")
#' sk = survfitByGene(sk, "BRAF")
#' plot( sk$survfit, lty=1:2, lwd=2, col=c("blue", "orange"))
#' title("BRAF mutation in SKCM")
#' @export
survfitByGene = function(x, genesym) {
 stopifnot(inherits(x, "TCGAmutsurv"))
 has_mut = apply(x$mutdata, 2, function(x) any(x == genesym, na.rm=TRUE))
 x$survfit = survival::survfit(x$surv ~ has_mut)
 x
}

#' compare multiple tumor times with respect to survival
#' @param types character() TCGA project codes
#' @param \dots passed to `build_surv_for_mut`
#' @return a list of lists of two elements: survlist (Surv objects) and
#' mutlist (mutation matrices)
#' @examples
#' cmp = compare_tumors()
#' plot(survfit(cmp$survlist[[1]]~1), conf.int=FALSE, xlab="years", ylab="Survival prob.", main="TCGA survival", lwd=2)
#' lines(survfit(cmp$survlist[[2]]~1), lwd=2, lty=2, conf.int=FALSE)
#' legend(15, .98, lty=c(1,2), lwd=2, legend=c("BRCA", "GBM"), bty="n")
#' @export
compare_tumors = function( types = c("BRCA", "GBM"), ...) {
  tmp = lapply(types, function(x, ...) build_surv_for_mut(x, ...))
  list(survlist = lapply(tmp, "[[", "surv"),
       mutlist = lapply(tmp, "[[", "mutdata"))
}

#sbrca = build_surv_for_mut()
#hasttn = apply(sbrca$mutdata, 2, function(x) any(x=="TTN", na.rm=TRUE))
#plot(survfit(sbrca$surv~hasttn))

#  hasttn = apply(cc5, 2, function(x) any(x=="TTN", na.rm=TRUE))
#  plot(survfit(ss~hasttn))


