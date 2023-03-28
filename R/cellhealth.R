#' Cell Painting dataset of CRISPR perturbations.
#'
#' Cell Painting dataset of CRISPR perturbations from
#' \doi{10.1091/mbc.E20-12-0784}
#'
#' @format A data frame with 198 rows and 8 variables:
#' \describe{
#'   \item{Metadata_Plate}{Plate id}
#'   \item{Metadata_Well}{Well id}
#'   \item{Metadata_cell_line}{Cell line id}
#'   \item{Metadata_gene_name}{Gene name}
#'   \item{Metadata_pert_name}{CRISPR guide name}
#'   \item{Cells_AreaShape_Compactness}{compactness measure}
#'   \item{Cells_AreaShape_Extent}{extent measure}
#'   \item{Cells_AreaShape_Zernike_0_0}{shape measure}
#' }
#' @source \url{https://github.com/broadinstitute/cell-health/}
"cellhealth"
