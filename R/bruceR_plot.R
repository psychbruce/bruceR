#### Themes for ggplot2 ####


#' A nice \code{ggplot2} theme for scientific plotting
#'
#' @param base.size Basic font size. Default is 12.
#' @param line.size Line width. Default is 0.5.
#' @param border \code{F}, \code{""} (default), or a color. If set to \code{F} or \code{""}, it will not draw the border.
#' @import ggplot2
#' @export
theme_bruce=function(base.size=12, line.size=0.5,
                     border="", bg="white",
                     tag="bold", plot.title="bold", axis.title="plain",
                     title.pos=0, caption.pos=1,
                     grid.x="", grid.y="grey90",
                     line.x=TRUE, line.y=TRUE,
                     tick.x=TRUE, tick.y=TRUE) {
  # font face:
  #     "plain", "italic", "bold", "bold.italic"
  # margin:
  #     top, right, bottom, left
  bg=switch(bg,
            stata="#EAF2F3", stata.grey="#E8E8E8",
            solar="#FDF6E3", wsj="#F8F2E4", light="#F6F1EB", dust="#FAF7F2",
            bg) # see ggthemr::ggthemr()$palette$background
  # ggthemr::colour_plot(c(solar="#FDF6E3", wsj="#F8F2E4", light="#F6F1EB", dust="#FAF7F2"))
  margin=ggplot2::margin
  theme = theme_bw() +
    theme(
      panel.grid=element_blank(),
      panel.grid.major.x=if(grid.x=="" | grid.x==F) element_blank() else
        element_line(size=line.size, color=grid.x),
      panel.grid.major.y=if(grid.y=="" | grid.y==F) element_blank() else
        element_line(size=line.size, color=grid.y),
      panel.border=if(border=="" | border==F) element_blank() else
        element_rect(size=line.size+0.4, color=border, fill=NA),
      axis.line=element_line(size=line.size, color="black"), # lineend="square"
      axis.title=element_text(face=axis.title, color="black", size=base.size+2),
      axis.title.x=element_text(margin=margin(0.5, 0, 0.2, 0, "lines")),
      axis.title.y=element_text(margin=margin(0, 0.5, 0, 0.2, "lines")),
      axis.ticks=element_line(size=line.size, color="black", lineend="square"),
      axis.ticks.length=unit(0.3, "lines"),
      axis.text=element_text(color="black", size=base.size),
      axis.text.x=element_text(margin=margin(ifelse(tick.x, 0.5, 0.3), 0, 0, 0, "lines")),
      axis.text.y=element_text(margin=margin(0, ifelse(tick.y, 0.5, 0.3), 0, 0, "lines")),
      plot.title=element_text(face=plot.title, size=base.size+2, hjust=title.pos,
                              margin=margin(0.2, 0, 0.6, 0, "lines")),
      plot.subtitle=element_text(face=plot.title, size=base.size, hjust=title.pos,
                                 margin=margin(0, 0, 0.6, 0, "lines")),
      plot.caption=element_text(size=base.size-2, hjust=caption.pos,
                                margin=margin(-1, 0, 0.1, 0, "lines")),
      plot.tag=element_text(face=tag, size=base.size+2),
      plot.tag.position=c(0.005, 0.985),
      plot.background=element_rect(color=bg, fill=bg),
      plot.margin=margin(0.02, 0.02, 0.02, 0.02, "npc")
    )
  if(border!="" | line.x==F) theme = theme + theme(axis.line.x=element_blank())
  if(border!="" | line.y==F) theme = theme + theme(axis.line.y=element_blank())
  if(!tick.x) theme = theme + theme(axis.ticks.x=element_blank())
  if(!tick.y) theme = theme + theme(axis.ticks.y=element_blank())
  return(theme)
}

# theme_stata=ggthemes::theme_stata(scheme="sj")
# theme_stata=ggthemes::theme_stata(scheme="s1color")
# theme_stata$axis.text.y$angle=0
# theme_stata$axis.text$size=12
# theme_stata$axis.title$size=12


#### Standard China Map ####

## China Map Template
if(FALSE) {
  bou=rgdal::readOGR("data-raw/bou2_4p.shp")
  bou@data$id=rownames(bou@data)
  maptemp=dplyr::full_join(ggplot2::fortify(bou), bou@data, by="id")
  usethis::use_data(maptemp, overwrite=TRUE)

  provdata_demo=rio::import("data-raw/provdata_demo.xlsx")
  usethis::use_data(provdata_demo, overwrite=TRUE)
}

#' Draw standard China maps
#' @import ggplot2
#' @importFrom cowplot ggdraw draw_plot save_plot
## @import grid
#' @param provdata Province-level data. You can use \code{\link[dplyr]{left_join}} to merge your prov data with \code{provdata_demo} (a demo dataset in \code{bruceR}) by the variable \code{"prov"}.
#' If not specified, it will draw a demo map for you (see Examples).
#'
#' For details about \code{provdata_demo}, type this in your console:
#' \code{View(provdata_demo)}
#' @param citydata City-level data with two variables (must be "geoE" and "geoN") specifying the longitude and latitude of cities, respectively.
#' @param var A character specifying the variable you want to map to the plot.
#' @param multiply A number useful when you want to expand the raw values by, e.g., 100 times.
#' @param log \code{TRUE} or \code{FALSE} (default). Whether to log-transform the raw values.
#' @param nsmall Number of decimal places of output. Default is 0.
#' @param colors Color palettes. The following palettes are available (see \code{\link[ggplot2]{scale_color_brewer}}):
#'
#' \strong{Sequential:}
#' \code{Blues, Greens, Greys, Oranges, Purples, Reds,
#' BuGn, BuPu, GnBu, OrRd, PuBu, PuRd, RdPu, YlGn,
#' PuBuGn, YlGnBu, YlOrBr, YlOrRd}
#'
#' \strong{Diverging:}
#' \code{BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral}
#'
#' \strong{Qualitative (not suggested):}
#' \code{Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3}
#' @param direc \code{1} (default) or \code{-1}, specifying the direction of color palette.
#' @param cityshape The shape of city dots. I recommend using 16 (round) or 18 (rhombus). The default is 18. For details, see \href{http://sape.inf.usi.ch/quick-reference/ggplot2/shape}{shape parameter}.
#' @param cityalpha The transparency of city dots. The default is 0.9.
#' @param addlabel \code{TRUE} (default) or \code{FALSE}. Whether to add value labels. For clarity, value labels are only added to provinces but not to cities.
#' @param labelprefix A character specifying a variable in your data for adding label prefix, usually \code{"prov"} if you want to add the names of provinces prior to values.
#' (Note: You can draw the label prefix only, by setting \code{addlable=FALSE} and \code{labelprefix="yourvariable"}.)
#' @param labelseg A character specifying the joint character between label prefix and values (e.g., setting to \code{": "} will make a label look like \code{"Beijing: 1.23"}).
#' @param tag Tag of the map (left-top corner).
#' @param title Title of the map.
#' @param guidetitle Title of the colorbar guide.
#' @param addguidevalue \code{TRUE} (default) or \code{FALSE}. Whether to add values under the colorbar guide.
#' @param limits A number vector specifying the range of values to plot (relevant both to the main plot and to the colorbar guide). Default is the actual range of your variable.
#' @param breaks A number vector specifying the breaking points of colorbar, e.g., \code{seq(0, 100, 25)}.
#' @param bordersize Line size of map border. Default is \code{0.2}.
#' @param bordercolor Line color of map border. Default is \code{"grey70"}.
#' @param na.color A color for those provinces with missing values. Default is \code{"grey90"}.
#' @param filename File name to create on disk. The file type can be any of ".pdf", ".png", ".jpg", ".bmp", ".tiff", ".eps", ... (see \code{\link[ggplot2]{ggsave}}).
#' @param dpi Dots per inch (DPI). A higher DPI produces clearer and more detailed output. Academic papars usually require 300 dpi at least. Here I use 500 as a default value.
#' (Note: PDF documents are not influenced by DPI.)
#' @return Invisibly return a list of two maps (a main map and a sub-map for Nanhai islands).
#' @examples
#' drawChinaMap() # draw a demo map
#' drawChinaMap(provdata_demo, var="geoE", nsmall=1, filename="ChinaMap1.png")
#' drawChinaMap(provdata_demo, var="geoN", nsmall=1, colors="Reds", direc=-1, addlabel=FALSE, filename="ChinaMap2.png")
#' @export
drawChinaMap=function(provdata=NULL, citydata=NULL,
                      var=NA, multiply=1, log=FALSE, nsmall=0,
                      colors="Blues", direc=1,
                      cityshape=18, cityalpha=0.9,
                      addlabel=TRUE, labelprefix="", labelseg=":",
                      tag="", title=var, guidetitle="", addguidevalue=TRUE,
                      limits=NULL, breaks=NULL,
                      bordersize=0.2, bordercolor="grey70", na.color="grey90",
                      filename="ChinaMap.png", dpi=500) {
  # Merge data
  if(is.null(citydata)) {
    level="prov"
    if(is.null(provdata)) {
      provdata=provdata_demo
      title="China Map (demo)"
      labelprefix="prov"
    }
    data=provdata
  } else {
    level="city"
    data=citydata
  }
  suppressWarnings({
    mapdata=dplyr::full_join(maptemp, provdata, by="NAME")
  })

  # Basic settings
  map.long=c(73.4, 135.1)
  map.lat=c(17.4, 53.6)
  jdx.long=c(107, 122)
  jdx.lat=c(4, 24)
  jdx=data.frame(ID=rep(1:10, each=2),
                 long=c(109.10, 109.80,
                        110.20, 109.90,
                        108.56, 108.55,
                        112.35, 113.90,
                        116.85, 117.75,
                        119.50, 119.90,
                        119.80, 119.75,
                        119.60, 119.90,
                        120.80, 121.70,
                        122.60, 123.00),
                 lat=c(16.40, 15.20,
                       13.30, 12.00,
                        9.10,  7.70,
                        5.45,  5.80,
                        8.50,  9.50,
                       11.80, 13.00,
                       14.70, 15.90,
                       17.50, 18.70,
                       20.40, 21.30,
                       23.20, 24.40))
  maptheme=theme_void() +
    theme(legend.position=c(0.24, ifelse(guidetitle!="" & addguidevalue==FALSE, 0.05, 0.07)),
          legend.title=element_text(size=14, color="black"),
          plot.tag=element_text(size=16, color="black", face="bold",
                                margin=margin(-1, 0, ifelse(is.null(title), 0, 1), 0.5, "lines")),
          plot.title=element_text(size=16, color="black", face="bold", hjust=0.5,
                                  margin=margin(-1, 0, 0.5, 0, "lines")))
  mapguide=guide_colorbar(title=guidetitle, title.position="top", title.hjust=0.5,
                          direction="horizontal", label=addguidevalue, ticks=FALSE,
                          barwidth=unit(5,"cm"), barheight=unit(5,"mm"))
  if(is.na(var)==FALSE) {
    guide.range=range(data[[var]], na.rm=TRUE)
    if(is.null(limits)) limits=c(floor(guide.range[1]), ceiling(guide.range[2]))
    if(is.null(breaks)) breaks=limits
  }

  # Draw maps
  map=ggplot() + maptheme
  if(is.na(var) | level=="city") {
    bordercolor="grey30"
    map=map + geom_polygon(data=mapdata, aes(x=long, y=lat, group=group), fill="grey95", color=bordercolor, size=bordersize)
  } else {
    if(log) {
      map=map + geom_polygon(data=mapdata, aes(x=long, y=lat, group=group, fill=log(get(var))), color=bordercolor, size=bordersize)
    } else {
      map=map + geom_polygon(data=mapdata, aes(x=long, y=lat, group=group, fill=get(var)), color=bordercolor, size=bordersize)
    }
    map=map +
      scale_fill_distiller(palette=colors, direction=direc, na.value=na.color,
                           limits=limits, breaks=breaks,
                           guide=mapguide)
  }
  if(level=="city") {
    if(log) {
      map=map + geom_point(data=citydata, aes(x=geoE, y=geoN, color=log(get(var))), shape=cityshape, size=3, alpha=cityalpha)
    } else {
      map=map + geom_point(data=citydata, aes(x=geoE, y=geoN, color=get(var)), shape=cityshape, size=3, alpha=cityalpha)
    }
    map=map +
      scale_color_distiller(palette=colors, direction=direc, na.value=na.color,
                            limits=limits, breaks=breaks,
                            guide=mapguide)
  }
  map=map + geom_line(data=jdx, aes(x=long, y=lat, group=ID), color="black", size=0.5)
  map1=map + coord_map(c("lambert", "albers")[1], parameters=c(25, 47), xlim=map.long, ylim=map.lat)
  map2=map + coord_map(xlim=jdx.long, ylim=jdx.lat) + geom_rect(aes(xmin=jdx.long[1], xmax=jdx.long[2], ymin=jdx.lat[1], ymax=jdx.lat[2]), fill=NA, color="black", size=0.5) + theme(legend.position="none")

  # Add labels
  if(level=="prov") {
    if((is.na(var)==TRUE | addlabel==FALSE) & labelprefix!="") {
      map1=map1 + geom_text(data=provdata, aes(x=geoE, y=geoN, label=get(labelprefix)), fontface="bold", size=3)
    }
    if(is.na(var)==FALSE & addlabel==TRUE & labelprefix=="") {
      map1=map1 + geom_text(data=provdata, aes(x=geoE, y=geoN, label=sprintf(paste0("%.", nsmall, "f"), get(var)*multiply)), size=3)
    }
    if(is.na(var)==FALSE & addlabel==TRUE & labelprefix!="") {
      map1=map1 + geom_text(data=provdata, aes(x=geoE, y=geoN, label=paste0(get(labelprefix), labelseg, sprintf(paste0("%.", nsmall, "f"), get(var)*multiply))), size=3)
    }
  }
  map1=map1 + labs(tag=tag, title=title)

  # Output (old; have bugs influencing subsequent plotting)
  # if(grepl(".pdf$", filename)) {
  #   pdf(filename, width=8, height=6)
  # } else {
  #   png(filename, width=8, height=6, units="in", res=dpi)
  # }
  # print(map1)
  # print(map2, vp=grid::viewport(width=0.2, height=0.2, x=0.86, y=0.16))
  # dev.off()

  # Output (with 'cowplot' package)
  # ggdraw=cowplot::ggdraw
  # draw_plot=cowplot::draw_plot
  # save_plot=cowplot::save_plot
  save_plot(filename, base_width=8, base_height=6, dpi=dpi,
            plot=ggdraw() + draw_plot(map1) + draw_plot(map2, x=0.76, y=0.06, width=0.2, height=0.2))

  # Feedback
  path=ifelse(grepl(":", filename), filename, paste0(getwd(), '/', filename))
  Print("<<green \u2714>> Saved to <<blue '{path}'>>")

  invisible(list(map.main=map1, map.jdx=map2))
}
