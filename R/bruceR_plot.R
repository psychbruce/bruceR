#### Themes for ggplot2 ####


#' A nice \code{ggplot2} theme for scientific plotting
#'
#' @import ggplot2
#' @param base.size Basic font size. Default is 12.
#' @param line.size Line width. Default is 0.5.
#' @param border \code{FALSE}, \code{""} (default), or a color. If set to \code{FALSE} or \code{""}, it will not draw the border.
#' @param bg Background color. Default is \code{"white"}. You can use any colors or choose from some pre-set color palettes:
#' \code{"stata", "stata.grey", "solar", "wsj", "light", "dust"}.
#'
#' To see these colors, you can type:
#'
#' \code{ggthemr::colour_plot(c(stata="#EAF2F3", stata.grey="#E8E8E8",
#' solar="#FDF6E3", wsj="#F8F2E4", light="#F6F1EB", dust="#FAF7F2"))}
#' @param tag Font face of tag. Choose from \code{"plain", "italic", "bold", "bold.italic"}.
#' @param plot.title Font face of title. Choose from \code{"plain", "italic", "bold", "bold.italic"}.
#' @param axis.title Font face of axis text. Choose from \code{"plain", "italic", "bold", "bold.italic"}.
#' @param title.pos Title position (0~1).
#' @param caption.pos Caption position (0~1).
#' @param grid.x \code{FALSE}, \code{""}, or a color to set the color of panel grid (x). Default is \code{""}.
#' @param grid.y \code{FALSE}, \code{""}, or a color to set the color of panel grid (y). Default is \code{"grey90"}.
#' @param line.x \code{TRUE} (default) or \code{FALSE}. Whether to draw the x-axis line.
#' @param line.y \code{TRUE} (default) or \code{FALSE}. Whether to draw the y-axis line.
#' @param tick.x \code{TRUE} (default) or \code{FALSE}. Whether to draw the x-axis ticks.
#' @param tick.y \code{TRUE} (default) or \code{FALSE}. Whether to draw the y-axis ticks.
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
