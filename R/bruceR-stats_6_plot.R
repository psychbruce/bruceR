#### Plot Toolbox ####


#' A nice `ggplot2` theme that enables Markdown/HTML rich text.
#'
#' @description
#' A nice `ggplot2` theme for scientific publication. It also uses [ggtext::element_markdown()] to render Markdown/HTML formatted rich text. You can use a combination of Markdown and/or HTML syntax (e.g., `"*y* = *x*<sup>2</sup>"`) in plot text or title, and this function draws text elements with rich text format.
#'
#' For more usage, see:
#' - [ggtext::geom_richtext()]
#' - [ggtext::geom_textbox()]
#' - [ggtext::element_markdown()]
#' - [ggtext::element_textbox()]
#'
#' @param markdown Use `element_markdown()` instead of `element_text()`. Defaults to `FALSE`.
#' If set to `TRUE`, then you should also use `element_markdown()` in `theme()` (if any).
#' @param base.size Basic font size. Defaults to `12`.
#' @param line.width Line width. Defaults to `0.5`.
#' @param border `TRUE`, `FALSE`, or `"black"` (default).
#' @param bg Background color of whole plot. Defaults to `"white"`.
#' You can use any colors or choose from some pre-set color palettes:
#' `"stata"`, `"stata.grey"`, `"solar"`, `"wsj"`, `"light"`, `"dust"`.
#' @param panel.bg Background color of panel. Defaults to `"white"`.
#' @param tag Font face of tag. Choose from `"plain"`, `"italic"`, `"bold"`, `"bold.italic"`.
#' @param plot.title Font face of title. Choose from `"plain"`, `"italic"`, `"bold"`, `"bold.italic"`.
#' @param axis.title Font face of axis text. Choose from `"plain"`, `"italic"`, `"bold"`, `"bold.italic"`.
#' @param title.pos Title position (0~1).
#' @param subtitle.pos Subtitle position (0~1).
#' @param caption.pos Caption position (0~1).
#' @param font Text font. Only applicable to Windows system.
#' @param grid.x `FALSE`, `""` (default), or a color (e.g., `"grey90"`) to set the color of panel grid (x).
#' @param grid.y `FALSE`, `""` (default), or a color (e.g., `"grey90"`) to set the color of panel grid (y).
#' @param line.x Draw the x-axis line. Defaults to `TRUE`.
#' @param line.y Draw the y-axis line. Defaults to `TRUE`.
#' @param tick.x Draw the x-axis ticks. Defaults to `TRUE`.
#' @param tick.y Draw the y-axis ticks. Defaults to `TRUE`.
#'
#' @return
#' A `theme` object that should be used for `ggplot2`.
#'
#' @examples
#' \donttest{## Example 1 (bivariate correlation)
#' d = as.data.table(psych::bfi)
#' added(d, {
#'   E = .mean("E", 1:5, rev=c(1,2), range=1:6)
#'   O = .mean("O", 1:5, rev=c(2,5), range=1:6)
#' })
#' ggplot(data=d, aes(x=E, y=O)) +
#'   geom_point(alpha=0.1) +
#'   geom_smooth(method="loess") +
#'   labs(x="Extraversion<sub>Big 5</sub>",
#'        y="Openness<sub>Big 5</sub>") +
#'   theme_bruce(markdown=TRUE)
#'
#' ## Example 2 (2x2 ANOVA)
#' d = data.frame(X1 = factor(rep(1:3, each=2)),
#'                X2 = factor(rep(1:2, 3)),
#'                Y.mean = c(5, 3, 2, 7, 3, 6),
#'                Y.se = rep(c(0.1, 0.2, 0.1), each=2))
#' ggplot(data=d, aes(x=X1, y=Y.mean, fill=X2)) +
#'   geom_bar(position="dodge", stat="identity", width=0.6, show.legend=FALSE) +
#'   geom_errorbar(aes(x=X1, ymin=Y.mean-Y.se, ymax=Y.mean+Y.se),
#'                 width=0.1, color="black", position=position_dodge(0.6)) +
#'   scale_y_continuous(expand=expansion(add=0),
#'                      limits=c(0,8), breaks=0:8) +
#'   scale_fill_brewer(palette="Set1") +
#'   labs(x="Independent Variable (*X*)",  # italic X
#'        y="Dependent Variable (*Y*)",  # italic Y
#'        title="Demo Plot<sup>bruceR</sup>") +
#'   theme_bruce(markdown=TRUE, border="")
#' }
#' @export
theme_bruce = function(
    markdown=FALSE,
    base.size=12, line.width=0.5,
    border="black",
    bg="white", panel.bg="white",
    tag="bold", plot.title="bold", axis.title="plain",
    title.pos=0.5, subtitle.pos=0.5, caption.pos=1,
    font=NULL,
    grid.x="", grid.y="",
    line.x=TRUE, line.y=TRUE,
    tick.x=TRUE, tick.y=TRUE
) {
  if(markdown) {
    installed("ggtext")
    element_text = ggtext::element_markdown
  }
  # font face:
  #     "plain", "italic", "bold", "bold.italic"
  # margin:
  #     top, right, bottom, left
  bg = switch(bg,
              stata="#EAF2F3", stata.grey="#E8E8E8",
              solar="#FDF6E3", wsj="#F8F2E4", light="#F6F1EB", dust="#FAF7F2",
              bg) # see ggthemr::ggthemr()$palette$background
  # ggthemr::colour_plot(c(solar="#FDF6E3", wsj="#F8F2E4", light="#F6F1EB", dust="#FAF7F2"))
  margin = ggplot2::margin
  if(!is.null(font)) grDevices::windowsFonts(FONT=grDevices::windowsFont(font))
  theme = theme_bw() +
    theme(
      # text = element_markdown(),
      # title = element_markdown(),
      # legend.text = element_markdown(size=base.size-2),
      # legend.title = element_markdown(size=base.size),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = if(grid.x=="" | grid.x==FALSE) element_blank() else
        element_line(linewidth=line.width, color=grid.x),
      panel.grid.major.y = if(grid.y=="" | grid.y==FALSE) element_blank() else
        element_line(linewidth=line.width, color=grid.y),
      panel.border = if(border=="" | border==FALSE) element_blank() else
        element_rect(linewidth=line.width+0.4, color=border, fill=NA),
      panel.background = element_rect(fill=panel.bg),
      axis.line = element_line(linewidth=line.width, color="black"),  # lineend="square"
      axis.title = element_text(size=base.size+2, color="black", face=axis.title),
      axis.title.x = element_text(margin=margin(0.5, 0, 0.4, 0, "lines")),
      axis.title.y = element_text(margin=margin(0, 0.5, 0, 0.4, "lines")),
      axis.ticks = element_line(linewidth=line.width, color="black", lineend="square"),
      axis.ticks.length = unit(0.3, "lines"),
      axis.text = element_text(size=base.size, color="black"),
      axis.text.x = element_text(margin=margin(ifelse(tick.x, 0.5, 0.3), 0, 0, 0, "lines")),
      axis.text.y = element_text(margin=margin(0, ifelse(tick.y, 0.5, 0.3), 0, 0, "lines")),
      strip.background = element_rect(color=NA),
      strip.text = element_text(size=base.size-2),
      legend.title = element_text(size=base.size),
      legend.text = element_text(size=base.size-2),
      plot.title = element_text(size=base.size+2, face=plot.title, hjust=title.pos,
                                margin=margin(0.2, 0, 0.6, 0, "lines")),
      plot.subtitle = element_text(size=base.size, face=plot.title, hjust=subtitle.pos,
                                   margin=margin(0, 0, 0.6, 0, "lines")),
      plot.caption = element_text(size=base.size-2, hjust=caption.pos,
                                  margin=margin(0, 0, 0.1, 0, "lines")),
      plot.tag = element_text(size=base.size+2, face=tag),
      plot.tag.position = c(0.005, 0.985),
      plot.background = element_rect(color=bg, fill=bg),
      plot.margin = margin(0.02, 0.02, 0.02, 0.02, "npc")
    )
  if(!is.null(font))
    theme = theme + theme(text = element_text(family="FONT"))
  if((border!="" & border!=FALSE) | line.x==FALSE)
    theme = theme + theme(axis.line.x = element_blank())
  if((border!="" & border!=FALSE) | line.y==FALSE)
    theme = theme + theme(axis.line.y = element_blank())
  if(!tick.x)
    theme = theme + theme(axis.ticks.x = element_blank())
  if(!tick.y)
    theme = theme + theme(axis.ticks.y = element_blank())
  return(theme)
}

# theme_stata = ggthemes::theme_stata(scheme="sj")
# theme_stata = ggthemes::theme_stata(scheme="s1color")


#' Show colors.
#'
#' @param colors Color names.
#'
#' Examples:
#' - `"red"` (R base color names)
#' - `"#FF0000"` (hex color names)
#' - `see::social_colors()`
#' - `viridis::viridis_pal()(10)`
#' - `RColorBrewer::brewer.pal(name="Set1", n=9)`
#' - `RColorBrewer::brewer.pal(name="Set2", n=8)`
#' - `RColorBrewer::brewer.pal(name="Spectral", n=11)`
#'
#' @return
#' A `ggplot` object.
#'
#' @examples
#' \donttest{show_colors("blue")
#' show_colors("#0000FF")  # blue (hex name)
#' show_colors(RGB(0, 0, 255))  # blue (RGB)
#' show_colors(see::social_colors())
#' show_colors(see::pizza_colors())
#' }
#' @export
show_colors = function(colors) {
  colors.names = names(colors)
  if(is.null(colors.names)) colors.names = colors
  dc = data.frame(names=as_factor(colors.names),
                  colors=as_factor(colors))
  ggplot(dc, aes(x=fct_rev(names), y=1,
                 fill=fct_rev(colors))) +
    geom_bar(stat="identity", width=1, show.legend=FALSE) +
    scale_fill_manual(values=rev(as.character(dc$colors))) +
    scale_x_discrete(position="top") +  # flipped y position = "right"
    coord_flip(expand=FALSE) +
    labs(x=NULL, y=NULL) +
    theme_void() +
    theme(axis.text.y=element_text(size=12, hjust=0,
                                   margin=margin(0, 0, 0, 1, "lines")),
          plot.margin=margin(0.05, 0.05, 0.05, 0.05, "npc"))
}

