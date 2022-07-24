#' @name zoo_legends
#' @title Legends for sites, taxons, etc.
#'
#' @description Read a XLSX file where legends are recorded in different worksheets. 
#' Plot different legends for: sites' types, sites' zones (geographical areas), etc.
#'
#' @param dataDir directory of the legend workbook.
#' By default the package folder
#' @param dataFile name of the legend workbook
#' @param worksheets names of the different worksheets to be read 
#' By default: c("sites_types", "sites_zones", etc.)
#' @param outDir output folder
#' By default the package folder
#' @return Series of plot showing the legends (shapes and colors)
#'
#' @examples
#' zoo_legends()
#' 
#' zoo_legends(worksheets = c("sites_types", "sites_zones"))
#' 
#' sites_types <- zoo_legends(worksheets = c("sites_types"))
#' @export
zoo_legends <- function(dataDir = paste0(system.file(package = "zoowork"), "/extdata/"),
                        dataFile = "legends.xlsx",
                        worksheets = c("sites_types", "sites_zones", "taxon_types"),
                        outDir = paste0(system.file(package = "zoowork"), "/extdata/")){
  myblancktheme <- ggplot2::theme(axis.line = ggplot2::element_blank(),
                                  axis.text.x = ggplot2::element_blank(),
                                  axis.text.y = ggplot2::element_blank(),
                                  axis.ticks = ggplot2::element_blank(),
                                  axis.title.x = ggplot2::element_blank(),
                                  axis.title.y = ggplot2::element_blank(),
                                  legend.position = "none",
                                  panel.background = ggplot2::element_blank(),
                                  panel.border = ggplot2::element_blank(),
                                  panel.grid.major = ggplot2::element_blank(),
                                  panel.grid.minor = ggplot2::element_blank(),
                                  plot.background = ggplot2::element_blank())
  legend.wb <- paste0(dataDir, dataFile)
  legend.ws <- openxlsx::getSheetNames(legend.wb)
  # sites' types
  if("sites_types" %in% worksheets){
    sites_types <- openxlsx::read.xlsx(legend.wb,
                                       sheet = "sites_types",
                                       rowNames = F,
                                       skipEmptyRows = TRUE)
    sites_types$line <- seq(1, nrow(sites_types))
    gsites_types <- ggplot2::ggplot(sites_types) +
      ggplot2::geom_text(mapping = ggplot2::aes(x = .08,
                                                y = 1,
                                                label = "type of site",
                                                hjust = 0),
                         cex = 4) +
      # symbols
      ggplot2::geom_point(ggplot2::aes(0, 
                                       line + 2,
                                       colour = color,
                                       fill = color,
                                       stroke = .5,
                                       pch = shape),
                          size = 2) +
      # abrev
      ggplot2::geom_text(mapping = ggplot2::aes(x = .02,
                                                y= line + 2,
                                                label = tsite.abrev,
                                                hjust = 0)) +
      # types
      ggplot2::geom_text(mapping = ggplot2::aes(x = .12,
                                                y = line + 2,
                                                label = tsite,
                                                hjust = 0)) +
      ggplot2::scale_colour_identity() +
      ggplot2::scale_shape_identity() +
      ggplot2::scale_fill_identity() +
      ggplot2::xlim(0, 0.5) +
      ggplot2::scale_y_reverse() +
      myblancktheme
    outFile <- paste0(outDir, "legend_", "sites_types", ".png")
    ggplot2::ggsave(file = outFile,
                    gsites_types, 
                    width = 15,
                    height = nrow(sites_types) + 2,
                    units = "cm",
                    dpi = 300)
    return(sites_types)
  }
  # sites' zones
  if("sites_zones" %in% worksheets){
    sites_zones <- openxlsx::read.xlsx(legend.wb,
                                       sheet = "sites_zones",
                                       rowNames = F,
                                       skipEmptyRows = TRUE)
    sites_zones$line <- seq(1, nrow(sites_zones))
    gsites_zones <- ggplot2::ggplot(sites_zones) +
      ggplot2::geom_text(mapping = ggplot2::aes(x = .08,
                                                y = 1,
                                                label = "zones",
                                                hjust = 0),
                         cex = 4) +
      # symbols
      ggplot2::geom_point(ggplot2::aes(0, 
                                       line + 2,
                                       colour = color,
                                       fill = color,
                                       stroke = .5,
                                       pch = shape),
                          size = 2) +
      # abrev
      ggplot2::geom_text(mapping = ggplot2::aes(x = .02,
                                                y = line + 2,
                                                label = tzone.abrev,
                                                hjust = 0)) +
      # types
      ggplot2::geom_text(mapping = ggplot2::aes(x = .12,
                                                y = line + 2,
                                                label = tzone,
                                                hjust = 0)) +
      ggplot2::scale_colour_identity() +
      ggplot2::scale_shape_identity() +
      ggplot2::scale_fill_identity() +
      ggplot2::xlim(0, 0.5) +
      ggplot2::scale_y_reverse() +
      myblancktheme
    outFile <- paste0(outDir, "legend_", "sites_zones", ".png")
    ggplot2::ggsave(file = outFile,
                    gsites_zones, 
                    width = 15,
                    height = nrow(sites_zones) + 2,
                    units = "cm",
                    dpi = 300)
  }
  if("taxon_types" %in% worksheets){
    taxon_types <- openxlsx::read.xlsx(legend.wb,
                                       sheet = "taxon_types",
                                       rowNames = F,
                                       skipEmptyRows = TRUE)
    taxon_types$line <- seq(1, nrow(taxon_types))
    gtaxon_types <- ggplot2::ggplot(taxon_types) +
      geom_text(mapping = aes(x = .5,
                              y = 1,
                              label = "taxons",
                              hjust = 0),
                cex = 4) +
      # symbols
      ggplot2::geom_point(ggplot2::aes(0, 
                                       line + 3,
                                       colour = color,
                                       fill = color,
                                       stroke = .5,
                                       pch = shape),
                          size = 2) +
      # abrev
      ggplot2::geom_text(mapping = ggplot2::aes(x = .02,
                                                y = line + 3,
                                                label = ttaxon.abrev,
                                                hjust = 0)) +
      # types
      ggplot2::geom_text(mapping = ggplot2::aes(x = .12,
                                                y = line + 3,
                                                label = ttaxon,
                                                hjust = 0)) +
      ggplot2::geom_text(mapping = ggplot2::aes(x = .6,
                                                y = line + 3,
                                                label = ttaxon.cat,
                                                hjust = 0), cex = 3) +
      ggplot2::scale_colour_identity() +
      ggplot2::scale_shape_identity() +
      ggplot2::scale_fill_identity() +
      ggplot2::scale_y_reverse() +
      ggplot2::xlim(0, 1) +
      myblancktheme
    outFile <- paste0(outDir, "legend_", "taxon_types", ".png")
    ggplot2::ggsave(file = outFile,
                    gtaxon_types, 
                    width = 15,
                    height = nrow(taxon_types) + 2,
                    units = "cm",
                    dpi = 300)
  }
}


