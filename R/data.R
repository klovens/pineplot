#' Log2 gene expression data for kidney and liver "specific" genes.
#'
#' A dataset containing the normalized gene expression data from
#' mouse, chicken, and macaque RNA-seq data.
#'
#' @format A matrix with 15 rows and 37 variables. The rows of the matrix
#' represent genes that are predicted as being liver (first 10 rows) specific
#' or kidney (last 5 rows) specific.
#' Mouse samples are stored in columns 1-12, chicken in columns 13-24, and
#' macaque in columns 25-37.
#' The 37 variables are labelled as follows:
#' \describe{
#'   \item{B}{Brain samples}
#'   \item{K}{Kidney samples}
#'   \item{L}{Liver samples}
#' }
#'
#' @examples
#' data(tissue_specific_genes)
#' ch_grp <- c("B", "B", "K", "K", "L", "L", "B", "K", "B", "K", "L", "K")
#' mu_grp <- c("B", "B", "K", "K", "L", "L", "B", "K", "L", "B", "K", "L")
#' maca_grp <- c("B", "B", "K", "K", "L", "L", "L", "B", "K", "L", "B", "K", "L")
#' mouse <- 1:12
#' chicken <- 13:24
#' macaque <- 25:37
#'
#' chicken.profile <- tissue_specific_genes[, chicken][, which(ch_grp == "L")]
#' mouse.profile <- tissue_specific_genes[, mouse][, which(mu_grp == "L")]
#' macaque.profile <- tissue_specific_genes[, macaque][, which(maca_grp == "L")]
"tissue_specific_genes"

#' Lists of example symmetric matrices.
#'
#' A dataset containing the mutual expression values between
#' 15 genes that are either predicted as liver or kidney specific.
#' Each species mouse, chicken, and macaque have a symmetric matrix
#' stored in each list contain in layers.
#'
#' @format Contains 3 lists with:
#' \describe{
#'   \item{layers_B}{symmetric matrices for brain tissue}
#'   \item{layers_K}{symmetric matrices for kidney tissue}
#'   \item{layers_L}{symmetric matrices for liver tissue}
#'   ...
#' }
"layers"

#' Mergin and Brawand gene expression dataset
#'
#' A description.
"merkin_brawand_rnaseq"

#' Cleveland heart disease dataset
#'
#' A description.
"heart"
