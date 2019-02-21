#' @import dplyr
#' @import shiny
#' @import bnutil
#' @import ggplot2
#' @import pgscales
#' @import reshape2

#
#' @export
operatorProperties = function() {

}

#' @export
shinyServerRun = function(input, output, session, context) {
  output$body = renderUI({
    mainPanel(
      h4("Correlation Matrix"),
      p("Preparing data, please wait ..."),
      verbatimTextOutput("banner")
    )
  })

  getDataReactive = context$getData()
  getFolderReactive = context$getRunFolder()
  getPropertiesReactive = context$getPropertiesAsMap()

  observe({
    getData=getDataReactive$value
    getFolder = getFolderReactive$value
    #getProperties = getPropertiesReactive$value

    if (is.null(getData)){
      return()
    }

    if(is.null(getFolder)){
      return()
    }

    bndata = getData()
    folder = getFolder()



    output$banner = renderText({

      if(bndata$hasColors){
        Color = droplevels(interaction(bndata$getColors()))
      } else {
        Color = "Main"
      }
      df = data.frame(bndata$getData(outlier=FALSE), group = Color)
      arrayNames = apply(df[bndata$arrayColumnNames], 1, paste, collapse = "-")
      df = data.frame(df, arrayNames = as.factor(arrayNames) )

      meta = data.frame(labelDescription = c("rowSeq", "colSeq", "Dummy"),
                        groupingType = c("rowSeq", "colSeq", "QuantitationType"))

      resdf = data.frame(rowSeq = 1, colSeq = 1, Dummy = NaN)
      result = AnnotatedData$new(data = resdf, metadata = meta)
      context$setResult(result)
      save(file = file.path(folder, "runData.RData"), df)
      return("Done!!")
    })

  })
}

#' @export
shinyServerShowResults = function(input, output, session, context) {

  output$body = renderUI({
    fluidPage(
      titlePanel("Correlation Matrix"),
      sidebarLayout(
        sidebarPanel(
          wellPanel(
            selectInput("cortype", "Correlation Type", choices = list("Pearson", "Lin-CCC"))
          ),
          wellPanel(
            sliderInput("dispmax", "Max of display range", min = 0, max = 1,value = 1),
            sliderInput("dispmin", "Min of display range", min = 0, max = 1,value = 0.7),
            checkboxInput("showcor", "Show correlation value in plot",value = TRUE),
            sliderInput("xlabelsize","X label size", min = 0, max = 20, value = 8),
            sliderInput("ylabelsize","Y label size", min = 0, max = 20, value = 1)
          ),
          actionLink("resfolder",label = "Save plot to disk and open results folder")
        ),
        mainPanel(
          plotOutput("cmat", height = "800px")
        )
      )
    )
  })

  getFolderReactive = context$getRunFolder()

  observe({

    getFolder = getFolderReactive$value
    if (is.null(getFolder)) return()
    folder = getFolder()

    load(file.path(folder, "runData.RData"))

    cormat = reactive({
      if(input$cortype == "Pearson"){
        cordf = pcmatrix(df)
      } else if (input$cortype == "Lin-CCC"){
        cordf = lccmatrix(df)
      }
    })

    ggcormat = reactive({
      cordf = cormat()
      clim = c(input$dispmin, input$dispmax)

      prt = ggplot(cordf, aes(x = X, y = reorder(Y, desc(Y)), fill = value, label = round(value,3)) ) + geom_tile()
      prt = prt + scale_fill_gradientn(colours = pgscales::cjet(), limits = clim)
      prt = prt + facet_wrap( ~ group)
      prt = prt + theme(axis.text.y = element_text(size = input$ylabelsize),
                        axis.text.x = element_text(size = input$xlabelsize, angle = 90))
      prt = prt + xlab("") + ylab("") + ggtitle(paste(input$cortype, "correlation coeficient"))
      if (input$showcor){
        prt = prt + geom_text(colour = "white")
      }

      return(prt)
    })

    output$cmat = renderPlot({
      ggcormat()
    })

    observeEvent(input$resfolder, {
      prt = ggcormat()
      ggsave(filename = file.path(folder, "Correlation Matrix.png"), plot = prt, device = "png")
      shell.exec(folder)
    })

  })
}
