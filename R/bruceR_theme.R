#### Themes for ggplot2 ####


#' A nice \code{ggplot2} theme for scientific publication.
#'
#' @param base.size Basic font size. Default is 12.
#' @param line.size Line width. Default is 0.5.
#' @param border \code{TRUE}, \code{FALSE}, or \code{"black"} (default).
#' @param bg Background color of whole plot. Default is \code{"white"}.
#' You can use any colors or choose from some pre-set color palettes:
#' \code{"stata", "stata.grey", "solar", "wsj", "light", "dust"}.
#'
#' To see these colors, you can type:
#'
#' \code{ggthemr::colour_plot(c(stata="#EAF2F3", stata.grey="#E8E8E8",
#' solar="#FDF6E3", wsj="#F8F2E4", light="#F6F1EB", dust="#FAF7F2"))}
#' @param panel.bg Background color of panel. Default is \code{"white"}.
#' @param tag Font face of tag. Choose from \code{"plain", "italic", "bold", "bold.italic"}.
#' @param plot.title Font face of title. Choose from \code{"plain", "italic", "bold", "bold.italic"}.
#' @param axis.title Font face of axis text. Choose from \code{"plain", "italic", "bold", "bold.italic"}.
#' @param title.pos Title position (0~1).
#' @param subtitle.pos Subtitle position (0~1).
#' @param caption.pos Caption position (0~1).
#' @param font Text font. Only applicable to Windows system.
#' @param grid.x \code{FALSE}, \code{""} (default), or a color (e.g., \code{"grey90"}) to set the color of panel grid (x).
#' @param grid.y \code{FALSE}, \code{""} (default), or a color (e.g., \code{"grey90"}) to set the color of panel grid (y).
#' @param line.x \code{TRUE} (default) or \code{FALSE}. Whether to draw the x-axis line.
#' @param line.y \code{TRUE} (default) or \code{FALSE}. Whether to draw the y-axis line.
#' @param tick.x \code{TRUE} (default) or \code{FALSE}. Whether to draw the x-axis ticks.
#' @param tick.y \code{TRUE} (default) or \code{FALSE}. Whether to draw the y-axis ticks.
#'
#' @examples
#' ## Example 1 (bivariate correlation)
#' d=as.data.table(bfi)
#' d[,":="(E=MEAN(d,"E",1:5,rev=c(1,2),likert=1:6),
#'         O=MEAN(d,"O",1:5,rev=c(2,5),likert=1:6))]
#' ggplot(data=d, aes(x=E, y=O)) +
#'   geom_point(alpha=0.1) +
#'   geom_smooth(method="loess") +
#'   labs(x="Extraversion", y="Openness") +
#'   theme_bruce()
#'
#' ## Example 2 (2x2 ANOVA)
#' d=data.frame(X1=factor(rep(1:3, each=2)),
#'              X2=factor(rep(1:2, 3)),
#'              Y.mean=c(5, 3, 2, 7, 3, 6),
#'              Y.se=rep(c(0.1, 0.2, 0.1), each=2))
#' ggplot(data=d, aes(x=X1, y=Y.mean, fill=X2)) +
#'   geom_bar(position="dodge", stat="identity", width=0.6, show.legend=FALSE) +
#'   geom_errorbar(aes(x=X1, ymin=Y.mean-Y.se, ymax=Y.mean+Y.se),
#'                 width=0.1, color="black", position=position_dodge(0.6)) +
#'   scale_y_continuous(expand=expansion(add=0),
#'                      limits=c(0,8), breaks=0:8) +
#'   scale_fill_brewer(palette="Set1") +
#'   theme_bruce(bg="wsj")
#'
#' @import ggplot2
#' @importFrom grDevices windowsFont windowsFonts
#' @export
theme_bruce=function(base.size=12, line.size=0.5,
                     border="black",
                     bg="white", panel.bg="white",
                     tag="bold", plot.title="bold", axis.title="plain",
                     title.pos=0.5, subtitle.pos=0.5, caption.pos=1,
                     font=NULL,
                     grid.x="", grid.y="",
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
  if(!is.null(font)) windowsFonts(FONT=windowsFont(font))
  theme = theme_bw() +
    theme(
      panel.grid.minor=element_blank(),
      panel.grid.major.x=if(grid.x=="" | grid.x==F) element_blank() else
        element_line(size=line.size, color=grid.x),
      panel.grid.major.y=if(grid.y=="" | grid.y==F) element_blank() else
        element_line(size=line.size, color=grid.y),
      panel.border=if(border=="" | border==F) element_blank() else
        element_rect(size=line.size+0.4, color=border, fill=NA),
      panel.background=element_rect(fill=panel.bg),
      axis.line=element_line(size=line.size, color="black"),  # lineend="square"
      axis.title=element_text(face=axis.title, color="black", size=base.size+2),
      axis.title.x=element_text(margin=margin(0.5, 0, 0.4, 0, "lines")),
      axis.title.y=element_text(margin=margin(0, 0.5, 0, 0.4, "lines")),
      axis.ticks=element_line(size=line.size, color="black", lineend="square"),
      axis.ticks.length=unit(0.3, "lines"),
      axis.text=element_text(color="black", size=base.size),
      axis.text.x=element_text(margin=margin(ifelse(tick.x, 0.5, 0.3), 0, 0, 0, "lines")),
      axis.text.y=element_text(margin=margin(0, ifelse(tick.y, 0.5, 0.3), 0, 0, "lines")),
      plot.title=element_text(face=plot.title, size=base.size+2, hjust=title.pos,
                              margin=margin(0.2, 0, 0.6, 0, "lines")),
      plot.subtitle=element_text(face=plot.title, size=base.size, hjust=subtitle.pos,
                                 margin=margin(0, 0, 0.6, 0, "lines")),
      plot.caption=element_text(size=base.size-2, hjust=caption.pos,
                                margin=margin(0, 0, 0.1, 0, "lines")),
      plot.tag=element_text(face=tag, size=base.size+2),
      plot.tag.position=c(0.005, 0.985),
      plot.background=element_rect(color=bg, fill=bg),
      plot.margin=margin(0.02, 0.02, 0.02, 0.02, "npc")
    )
  if(!is.null(font)) theme = theme + theme(text=element_text(family="FONT"))
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
