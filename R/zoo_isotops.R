#' Isotopic variation on two y-columns.
#'
#' `zoo_isotops()` returns a ggplot graph.
#'
#' Compare two isotopic variation (eg O, Sr) on a two y-axis plots
#'
#' @param dataDir directory of the dataset
#' @param dataFile name of the dataset
#' @param img.title title of the output plot
#' @param iso.names names of the isotopes.By default columns c("O", "Sr")
#' @param iso.colors color of the isotopes. By default c("turquoise", "turquoise4")
#' @param img.layout layout for the output. By default c("grid", "align")
#' @param img.dim dimensions for the output, Width and length. By default list(grid = c(18, 16), align = c(13, 21))
#' @param img.units By default "cm"
#' @param col.ERJ x-axis. By default column 'ERJ'
#' @param col.ind group column. By default column 'ind' (individus)
#'   be removed?
#' @return ggplot graphs, in grid layout or aligned vertically.
#'
#' @examples
#' zoo_isotops()
#' 
#' zoo_isotops(img.title = "O and Sr isotopic variations",
#'             iso.names = c("O", "Sr"),
#'             iso.colors = c("blue", "red"),
#'             img.layout = "align",
#'             img.dim = c(13, 21)
#' )
#'
#' }

library(ggplot2)
library(tibble)
library(ggpubr)
library(grid)
library(gridExtra)



graphic_df <- function(df, 
                       img.title. = img.title,
                       iso.left. = iso.left,
                       iso.right. = iso.right,
                       y.left.color. = y.left.color,
                       y.right.color. = y.right.color,
                       min.left. = min.left,
                       min.right. = min.right,
                       max.left. = max.left,
                       max.right. = max.right,
                       xlim.ERJ. = xlim.ERJ,
                       a. = a,
                       b. = b){
  # df <- df.select
  # df <- read.table(paste0(dataDir, dataFile), header = TRUE, sep = "\t")
  # iso.left.name <- iso.left
  # iso.right.name <- iso.right
  # min.left.value <- min.left
  # min.right.value <- min.right
  # max.left.value <- max.left
  # max.right.value <- max.right
  df.tb <- tibble(
    ind = df$ind,
    ERJ = df$ERJ,
    left = df[ , iso.left.],
    right = df[ , iso.right.]
  )
  img.out <- ggplot(df.tb, aes(ERJ, left)) +
    ggtitle(img.title.) +
    geom_line(color = y.left.color., size = 0.2) +
    geom_point(col = y.left.color., size = 1) +
    geom_line(aes(y = a. + right*b.),
              color = y.right.color.,
              size = 0.2) +
    geom_point(aes(y = a. + right*b.),
               color = y.right.color.,
               size = 1) +
    scale_y_continuous(name = iso.left.,
                       limits = c(min.left., max.left.),
                       sec.axis = sec_axis(~ (. - a.)/b.,
                                           name = iso.right.,
                                           breaks = seq(min.right., max.right., by = .0002),
                                           labels = scales::number_format(accuracy = 0.0001,
                                                                          decimal.mark = '.'))) +
    scale_x_reverse(breaks = seq(ceiling(min(df.tb$ERJ))-1,
                                 ceiling(max(df.tb$ERJ)),
                                 by = 5)) +
    expand_limits(x = c(ceiling(xlim.ERJ.[1])-1, xlim.ERJ.[2])) +
    theme_bw() +
    theme(
      title = element_text(size = 8),
      axis.text.x = element_text(size = 6),
      axis.text.y = element_text(size = 6),
      # left y
      axis.title.y.left = element_text(color = y.left.color., size = 7),
      axis.text.y.left = element_text(color = y.left.color., size = 6),
      # right y
      axis.title.y.right = element_text(color = y.right.color., size = 7),
      axis.text.y.right = element_text(color = y.right.color., size = 6)
    )
  print("DONE")
  return(img.out)
}

zoo_isotops <- function(dataDir = paste0(getwd(), "/extdata/"),
                        dataFile = "C_O_Sr_etno3.txt",
                        img.title = "Sheep and goat",
                        iso.names = c("O", "Sr"),
                        iso.colors = c("turquoise", "turquoise4"),
                        img.format = ".pdf",
                        img.layout = c("grid", "align"),
                        img.dim = list(grid = c(18, 16), align = c(13, 21)),
                        img.units = "cm",
                        # TODO: include these mandatory columns
                        col.ERJ = "ERJ",
                        col.ind = "ind"){
  iso.left <- iso.names[1]
  iso.right <- iso.names[2]
  y.left.color <- iso.colors[1]
  y.right.color <- iso.colors[2]
  img.width.grid <- img.dim$grid[1]
  img.height.grid <- img.dim$grid[2]
  img.width.align <- img.dim$align[1]
  img.height.align <- img.dim$align[2]
  df.iso <- read.table(paste0(dataDir, dataFile), 
                       header = TRUE, 
                       sep = "\t")
  # min/max values for axes
  max.left <- max(df.iso[ , iso.left], na.rm = T) ; min.left <- min(df.iso[ , iso.left], na.rm = T)
  max.right <- max(df.iso[ , iso.right], na.rm = T) ; min.right <- min(df.iso[ , iso.right], na.rm = T)
  
  xlim.ERJ <- c(min(df.iso[ , col.ERJ]), max(df.iso[ , col.ERJ])) 
  ylim.left <- c(min(df.iso[ , iso.left]), max(df.iso[ , iso.left])) 
  ylim.right <- c(min(df.iso[ , iso.right]), max(df.iso[ , iso.right]))
  
  # prepare double axis
  # scale the right y data
  b <- diff(ylim.left) / diff(ylim.right)
  a <- ylim.left[1] - b*ylim.right[1]
  
  if(any(img.layout == "grid")){
    ll.g <- list()
    for(ind in unique(df.iso[ , col.ind])){
      # ind <- "OVAR1"
      df.select <- df.iso[df.iso[ , col.ind] == ind, ]
      img.out <- graphic_df(df.select, 
                            img.title,
                            iso.left, iso.right, 
                            y.left.color, y.right.color,
                            min.left, min.right,
                            max.left, max.right,
                            xlim.ERJ,
                            a,
                            b)
      ll.g[[length(ll.g) + 1]] <- img.out
    }
    print(length(ll.g))
    print("YES")
    g.grid <- do.call("grid.arrange", c(ll.g, ncol=3))
    print("YESYES")
    outFile <- gsub("\\..*", "", dataFile)
    g.out <- paste0(file.path(tempdir()), "\\", outFile, "_grid_", img.format)
    ggsave(g.out, g.grid,
           width = img.width.grid, 
           height = img.height.grid,
           units = img.units)
    cat(print(paste0("image '", g.out, "' has been saved !")))
  }
  if(any(img.layout == "align")){
    # TODO: ind -> df.iso[ , col.ind]
    img.out <- graphic_df(df.iso) + facet_grid(ind ~ .)
    g.out <- paste0(file.path(tempdir()), "\\", outFile, "_align_", img.format)
    ggsave(g.out, img.out,
           width = img.width.align, 
           height = img.height.align,
           units = img.units)
    cat(print(paste0("image '", g.out, "' has been saved !")))
  }
}

zoo_isotops()

zoo_isotops(img.title = "O and Sr isotopic variations",
            iso.names = c("O", "Sr"),
            iso.colors = c("blue", "red"),
            img.layout = "align",
            img.dim = c(13, 21)
)
