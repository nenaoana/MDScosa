pd_mdsplot <- 
  function (
    coordmat,
    labels = NULL,
    point_col = "black",
    point_size = 1.5,
    text_col = "black",
    text_size = 3,
    line_col = "red",
    title = "MDS",
    WF,
    EL,
    CL,
    DF
  )
  { if (!is.null(labels)){  
    ggplot(coordmat, aes(D1, D2)) + 
      geom_point(colour=point_col, size=point_size) +
      labs(x="", y="", title=title) + 
      #theme_void() + theme(legend.position="none")
      theme(panel.border = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      geom_path(data=coordmat[c(WF,EL),], color=line_col) +
      geom_path(data=coordmat[c(CL,DF),], color=line_col, linetype = 2) + 
      geom_text(colour=text_col, check_overlap = TRUE, size=text_size, 
                hjust = "left", vjust = "bottom", nudge_x = 0.03, nudge_y = 0, angle = 10,
                label = labels)}
    else {
      ggplot(coordmat, aes(D1, D2)) + 
        geom_point(colour=point_col, size=point_size) +
        labs(x="", y="", title=title) + 
        #theme_void() + theme(legend.position="none")
        theme(panel.border = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black")) +
        geom_path(data=coordmat[c(WF,EL),], color=line_col) +
        geom_path(data=coordmat[c(CL,DF),], color=line_col, linetype = 2)
    }
  }
