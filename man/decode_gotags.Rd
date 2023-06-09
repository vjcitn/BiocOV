% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/decode_gotags.R
\name{decode_gotags}
\alias{decode_gotags}
\title{use TERM component of GO.db to give interpretations of tags}
\usage{
decode_gotags(x, join = TRUE)
}
\arguments{
\item{x}{vector or data.frame with GO tags; if the latter, column name must be "GO"}

\item{join}{logical(1) join result back to input data.frame if TRUE, otherwise return simple table of decodings}
}
\description{
use TERM component of GO.db to give interpretations of tags
}
\note{
When join is TRUE, a many-to-many join is produced.
}
\examples{
requireNamespace("org.Hs.eg.db")
ttnanno = AnnotationDbi::select(org.Hs.eg.db::org.Hs.eg.db, keys="TTN", keytype="SYMBOL",
  columns=c("GENENAME", "GO"))
interp = decode_gotags(ttnanno)
head(interp)
}
