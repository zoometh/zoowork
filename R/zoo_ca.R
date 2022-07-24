#' Correspondance Analysis of the NISP data
#' @name zoo_ca
#' @description Correspondance Analysis of the NISP data by periods
#'
#' @param df a dataframe
#' @param num_column the column name of assemblage numbers
#' @param site_column the column name of assemblage sites
#' @param period_column the column name of assemblage periods 
#' @param percBOTA_column the column name of assemblage percents of BOTA
#' @param percOC_column the column name of assemblage percents of OC
#' @param percSUDO_column the column name of assemblage percents of SUDO
#' @param typSite_column the column name of assemblage site types
#' @param lorder_period a vector with the ordered periods
#' @param typsit_symb a dataframe with the symbology
#' @param CA1_interval the min and max intervals for the F1 axis
#' @param CA2_interval the min and max intervals for the F2 axis
#' @param colVar the color of the variables
#' @param shpVar the shape of the variables
#' @pt_siz the size of the symbols
#' 
#' @return a gglot with the different CA by period
#'
#' @examples
#' 
#' df <- zoo_read()
#' lorder_period <- zoo_order_period(df)
#' typsit_symb <- zoo_legends(worksheets = c("sites_types"))
#' zoo_ca(df = df, lorder_period = lorder_period, typsit_symb = typsit_symb)
#' 
#' @export
zoo_ca <- function(df = NA,
                   num_column = "num",
                   site_column = "site",
                   period_column = "period",
                   percBOTA_column = "percBOTA",
                   percOC_column = "percOC",
                   percSUDO_column = "percSUDO",
                   typSite_column = "type.site",
                   lorder_period = NA,
                   typsit_symb = NA,
                   CA1_interval = c(-1, 2),
                   CA2_interval = c(-1, 1),
                   colVar = "black",
                   shpVar = 17,
                   pt_siz = 1.5){
  perCA_tsit <- data.frame(perCA1 = 0,
                           perCA2 = 0,
                           per = "xx")
  ca_all_tsite <- data.frame(num = 'xx',
                             site = 'xx',
                             type.site = 'xx',
                             CA1 = 0,
                             CA2 = 0,
                             percBOTA = 0,
                             percSUDO = 0,
                             percOC = 0,
                             per = 'xx',
                             shape = 0,
                             color = 'xx')
  
  for (per in lorder_period){
    # per <- "MIA2"
    print(per)
    df_per <- df[df[ , period_column] %in% per,] # sélectionne sur périodes
    df_per <- df_per[complete.cases(df_per), ]
    row.names(df_per) <- df_per$num 
    df_lda.per <- df_per[ , c(percBOTA_column, percSUDO_column, percOC_column, typSite_column)]
    if(nrow(df_per) > 3){
      print("  - run CA")
      xdat <- df_lda.per[ , -which(names(df_lda.per) %in% c(typSite_column))]
      ca <- FactoMineR::CA(xdat,graph = FALSE)            # AFC
      inertCA1 <- round(as.numeric(ca$eig[, 2][1]), 1)
      inertCA2 <- round(as.numeric(ca$eig[, 2][2]), 1)
      # pour afficher les %
      perCA_tsit <- rbind(perCA_tsit, data.frame(perCA1 = inertCA1,
                                                 perCA2 = inertCA2,
                                                 per = per))
      coords_ind_ca <- as.data.frame(ca$row$coord)
      coords_var_ca <- as.data.frame(ca$col$coord)
      coords_ca <- rbind(coords_ind_ca,coords_var_ca)
      colnames(coords_ca)[1] <- 'CA1'
      colnames(coords_ca)[2] <- 'CA2'
      dataset.p <- merge(df_lda.per, coords_ca, by = "row.names", all.y = T)
      dataset.ps <- merge(dataset.p, typsit_symb, by.x = typSite_column, by.y = "tsite", all.x = T)
      dataset.ps$per <- per
      dataset.ps$color <- as.character(dataset.ps$color)
      for (i in seq(1, nrow(dataset.ps))){
        if(dataset.ps[i,"Row.names"] == percBOTA_column){
          dataset.ps[i,"Row.names"] <- "BOTA"
          dataset.ps[i,"color"] <- colVar  # blue
          dataset.ps[i,"shape"] <- shpVar 
          dataset.ps[i,"Type.site"] <- "var" # triangle plein
        }
        if(dataset.ps[i,"Row.names"] == percOC_column){
          dataset.ps[i,"Row.names"] <- "OC"
          dataset.ps[i,"color"] <-  colVar # green
          dataset.ps[i,"shape"] <-  shpVar # 
          dataset.ps[i,"Type.site"] <- "var" # triangle plein
        }
        if(dataset.ps[i,"Row.names"]== percSUDO_column){
          dataset.ps[i,"Row.names"] <- "SUDO"
          dataset.ps[i,"color"] <- colVar # "black" # red
          dataset.ps[i,"shape"] <- shpVar # 17 # triangle plein
          dataset.ps[i,"Type.site"] <- "var" # triangle plein
        }
      }
      dataset.ps$shape <- as.factor(dataset.ps$shape)
      names(dataset.ps)[names(dataset.ps) == 'Row.names'] <- num_column
      df_per_site <- df_per[ , c(site_column, num_column)]
      ff <- merge(dataset.ps, df_per_site, by = num_column, all.x = T)
      # ff <- ff[ , colnames(ca_all_tsite)]
      matches <- colnames(ca_all_tsite) # réordonne
      ff <- ff[ ,match(matches, colnames(ff))]
      ca_all_tsite <- rbind(ca_all_tsite,ff)
      ###
      perCA_tsit <- perCA_tsit[-1,] # supprime premier ligne = xxx
      ca_all_tsite<- ca_all_tsite[-1,]
      ca_all_tsite$shape <- as.factor(ca_all_tsite$shape)
      ca_all_tsite$color <- as.factor(ca_all_tsite$color)
    } else {
      print(paste0("There's only ", nrow(df_per), " individual in the dataframe, no CA can be computed"))
    }
  }
  # graphes CA
  gca_tsite <- ggplot2::ggplot(ca_all_tsite, aes(CA1, CA2)) +
    ggplot2::geom_text(ggplot2::aes(x = min(CA1_interval),
                                    y = max(CA2_interval),
                                    label = per), 
                       hjust = 0,
                       vjust = 1) +
    ggplot2::geom_point(ggplot2::aes(CA1, CA2,
                                     colour = color,
                                     fill = color,
                                     stroke = .5,
                                     pch = as.numeric(levels(ca_all_tsite$shape))[ca_all_tsite$shape]),
                        size = pt_siz) + 
    ggrepel::geom_text_repel(ggplot2::aes(CA1, CA2,label = num),
                             cex=2,
                             segment.size = 0.1,
                             segment.alpha = 0.5)+
    ggplot2::geom_hline(yintercept=0, linetype = "dashed", size = 0.2, alpha = 0.3)+
    ggplot2::geom_vline(xintercept=0, linetype = "dashed", size = 0.2, alpha = 0.3)+
    ggplot2::geom_text(data=perCA_tsit,
                       mapping = ggplot2::aes(x = 0, y = -Inf, 
                                              label = paste0(perCA1,"%")),
                       vjust = -1,
                       size = 2,
                       alpha = 0.5
    )+
    ggplot2::geom_text(data = perCA_tsit,
                       mapping = ggplot2::aes(x = -Inf, y = 0,
                                              label = paste0(perCA2, "%")),
                       vjust = 1,
                       angle = 90,
                       size = 2,
                       alpha = 0.5)+
    ggplot2::theme(axis.text = element_text(size = 5),
                   axis.title.x = element_text(size = 8),
                   axis.title.y = element_text(size = 8))+
    ggplot2::theme(axis.ticks = element_line(size = 0.2))+
    ggplot2::theme(legend.position = "none")+
    ggplot2::theme(strip.text.x = element_text(size = 8),
                   strip.text.y = element_blank())+
    ggplot2::theme(panel.border = element_rect(colour = 'black',
                                               fill = NA, 
                                               size = 0.2))+
    ggplot2::theme(panel.background = element_rect(fill = 'transparent'))+
    ggplot2::theme(panel.spacing.y = unit(0, "lines")) +
    ggplot2::scale_x_continuous(limits = CA1_interval, expand = c(0, 0))+
    ggplot2::scale_y_continuous(limits = CA2_interval, expand = c(0, 0))+
    ggplot2::scale_colour_identity() +
    ggplot2::scale_shape_identity() +
    ggplot2::scale_fill_identity() +
    ggplot2::facet_grid(per ~ .)
  return(gca_tsite)
}  


