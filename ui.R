library(shiny)
library(tercen) # tercen API
library(tim) # tercen/tim for get_serialized_result()
library(dplyr) # data manipulation
library(tidyr) # data manipulation
library(reshape2) # data manipulation
library(ggplot2) # plotting

source("R/pgcombat.R")
options(shiny.error = browser)

ui <- shinyUI(fluidPage(sidebarLayout(
  sidebarPanel(
    checkboxInput("applymode", "Apply saved model", FALSE),
    conditionalPanel(
      condition = "!input.applymode",
      checkboxInput("useref", "Use a reference batch", value = FALSE),
      conditionalPanel(
        condition = 'input.useref',
        selectInput("refbatch", "Select reference variable", choices = list())
      ),
      selectInput("modeltype", "Type of model", choices =  c("L/S", "L")),
      checkboxInput("returnlink", "Return link to Combat model", value = FALSE)
    ),
    conditionalPanel(
      condition = "input.applymode"
    ),
    actionButton("done", "Done"),
    verbatimTextOutput("status")
  ),
  mainPanel(plotOutput("pca"))
)))