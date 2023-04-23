#' use TERM component of GO.db to give interpretations of tags
#' @param x vector or data.frame with GO tags; if the latter, column name must be "GO"
#' @param join logical(1) join result back to input data.frame if TRUE, otherwise return simple table of decodings
#' @note When join is TRUE, a many-to-many join is produced.
#' @examples
#' requireNamespace("org.Hs.eg.db")
#' ttnanno = AnnotationDbi::select(org.Hs.eg.db::org.Hs.eg.db, keys="TTN", keytype="SYMBOL",
#'   columns=c("GENENAME", "GO"))
#' interp = decode_gotags(ttnanno)
#' head(interp)
#' @export
decode_gotags = function (x, join = TRUE)
{
    if (inherits(x, "data.frame"))
        tagvec = x$GO
    else tagvec = x
    stopifnot
    dec = AnnotationDbi::select(GO.db::GO.db, keys = tagvec, columns = "TERM")
    if (!join)
        return(dec)
    dplyr::left_join(x, dplyr::mutate(dec, GO = GOID), relationship="many-to-many")
}

