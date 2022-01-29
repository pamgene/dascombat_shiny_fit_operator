library(shiny)
library(tercen) # tercen API
library(tim) # tercen/tim for get_serialized_result()
library(dplyr) # data manipulation
library(tidyr) # data manipulation
library(reshape2) # data manipulation
library(ggplot2) # plotting

source("R/pgcombat.R")
options(shiny.error = browser)

############################################
#### This part should not be modified
getCtx <- function(session) {
  # retreive url query parameters provided by tercen
  query <- parseQueryString(session$clientData$url_search)
  token <- query[["token"]]
  taskId <- query[["taskId"]]
  
  # create a Tercen context object using the token
  ctx <- tercenCtx(taskId = taskId, authToken = token)
  return(ctx)
}
####
############################################

server <- shinyServer(function(input, output, session) {
  dataInput <- reactive({
    getValues(session)
  })
  
  
  observe({
    # Check if we can obtain data
    
    getData = dataInput
    if (is.null(getData))
      return()
    
    # Obtain data
    bndata = getData()
    df = bndata$data
    
    print('Exploring data (debug purposes)')
    print(df)
    
    #TO-DO: check if checks are really neccessary
    #if (!bndata$hasColors) {
    #  stop("Need exactly 1 data color for the batch variable or model link")
    #}
    
    #if (length(bndata$colorColumnNames) > 1) {
    #  stop("Need exactly 1 data color for the batch variable or model link")
    #}
    
    # Modify data including colors
    df = bndata$data
    df$bv = as.factor(getCtx(session)$select(getCtx(session)$colors))
    
    lab = paste("Select reference batch from the values in",
                getCtx(session)$colors)
    updateSelectInput(session,
                      "refbatch",
                      label = lab,
                      choices = levels(df$bv))
    updateSelectInput(session, "modlink", choices = bndata$arrayColumnNames)
    
    lmodel = reactive({
      return(input$modeltype == "L")
    })
    
    comfit = reactive({
      X0 = acast(df, .ri ~ .ci, value.var = ".y")
      bv = acast(df,  .ri ~ .ci, value.var = ".color")[1, ]
      bv = droplevels(factor(bv))
      rowSeq = acast(df,  .ri ~ .ci, value.var = ".ri")[, 1]
      colSeq = acast(df,  .ri ~ .ci, value.var = ".ci")[1, ]
      dimnames(X0) = list(rowSeq = rowSeq, colSeq = colSeq)
      cmod = pgCombat$new()
      if (input$useref) {
        cmod = cmod$fit(X0,
                        bv,
                        ref.batch = input$refbatch,
                        mean.only = lmodel())
      } else {
        cmod = cmod$fit(X0, bv, mean.only = lmodel())
      }
      
      return(cmod)
    })
    
    modfile = reactive({
      mfile = levels(factor(df[[input$modlink]]))
      bFile = file.exists(mfile)
      if (!any(bFile))
        stop("Model link not found")
      mfile = mfile[bFile]
      if (length(mfile) > 1)
        stop("Incorrect model link")
      return(mfile)
      
    })
    
    comapply = reactive({
      modlink = modfile()
      X0 = acast(df, rowSeq ~ colSeq, value.var = "value")
      bv = acast(df,  rowSeq ~ colSeq, value.var = "bv")[1, ]
      bv = droplevels(factor(bv))
      rowSeq = acast(df,  rowSeq ~ colSeq, value.var = "rowSeq")[, 1]
      colSeq = acast(df,  rowSeq ~ colSeq, value.var = "colSeq")[1, ]
      dimnames(X0) = list(rowSeq = rowSeq, colSeq = colSeq)
      load(modlink)
      Xc = aCom$apply(X0, bv)
      dimnames(Xc) = dimnames(X0)
      result = list(X0 = X0,
                    Xc = Xc,
                    batches = bv)
    })
    
    settingsTable = reactive({
      if (!input$applymode) {
        settings = data.frame(
          setting = c("applymode", "useref", "refbatch", "modeltype"),
          value  = c(
            input$applymode,
            input$useref,
            input$refbatch,
            input$modeltype
          )
        )
      } else{
        settings = data.frame(setting = "applymode", value = input$applymode)
      }
      return(settings)
    })
    
    output$pca = renderPlot({
      if (!input$applymode) {
        aCom = comfit()
      } else {
        aCom = comapply()
      }
      
      iPca = prcomp(t(aCom$X0))
      fPca = prcomp(t(aCom$Xc))
      pcaresi = data.frame(
        PC1 = iPca$x[, 1],
        PC2 = iPca$x[, 2],
        bv = aCom$batches,
        stage = "before"
      )
      pcaresf = data.frame(
        PC1 = fPca$x[, 1],
        PC2 = fPca$x[, 2],
        bv = aCom$batches,
        stage = "after"
      )
      pcares = rbind(pcaresi, pcaresf)
      prt = ggplot(pcares, aes(
        x = PC1 ,
        y = PC2,
        colour = bv
      )) + geom_point()
      prt = prt + facet_wrap( ~ stage, scales = "free")
      return(prt)
    })
    
    output$status = renderText({
      isolate({
        bLink = input$returnlink
      })
      if (input$done > 0) {
        if (!input$applymode) {
          aCom = comfit()
        } else {
          aCom = comapply()
        }
        Xc = aCom$Xc
        dfXc = melt(Xc, value.name = "CmbCor")
        dfXc$rowSeq = as.double(dfXc$rowSeq)
        dfXc$colSeq = as.double(dfXc$colSeq)
        if (!input$returnlink | input$applymode) {
          mdf = data.frame(
            labelDescription = c("rowSeq", "colSeq", "CmbCor"),
            groupingType = c("rowSeq", "colSeq", "QuantitationType")
          )
          result = dfXc
        } else {
          print('Saving data and model...')
          
          # serialize data and return back
          res <- get_serialized_result(
            df = df,
            object = df,
            object_name = "dascombat_model",
            ctx = getCtx(session)
          )
          
          getCtx(session)$save(res)
          
          print('Saved data and model...')
          
        }
        #settings = settingsTable()
        #save(file = file.path(getRunFolder(), "runSettings.RData"), settings)
        #context$setResult(result)
        return("Done")
      } else {
        return(".")
      }
    })
  })
})


getValues <- function(session) {
  ctx <- getCtx(session)
  values <- list()
  
  values$data <- ctx %>% select(.y, .ri, .ci) %>%
    mutate(.color = ctx$select(ctx$colors[[1]]) %>% pull())
  values$ctx <- ctx
  
  return(values)
}
