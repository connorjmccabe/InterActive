options(digits=9)
options(shiny.maxRequestSize=20*1024^2) #max file size = 20 MB
# server.R
require(ggplot2)
require(QuantPsyc)
# require(plyr)
require(gridExtra)
require(stats)
require(MASS)
require(RColorBrewer)
require(foreign)
require(pscl)
require(grDevices)
require(Hmisc)
require(quantmod)
# require(shinyjs)
require(pander)


# HELPER FUNCTIONS --------------------------------------------------------

cfMake <- function(formula=NULL,data,nscen=1,names=NULL,hull=FALSE,f="mean",...) {
  if (!is.null(formula)) {
    #resploc <- attr(terms(formula),"response")
    #data <- data[,all.vars(formula)[-resploc]]
    data <- data[,all.vars(formula)]
  }
  data <- na.omit(data)
  xmean <- apply(data,2,f,...)
  xscen <- list(x=NULL,xpre=NULL)
  xscen$x <- as.data.frame(matrix(data=xmean,nrow=nscen,ncol=ncol(data),byrow=TRUE))
  xscen$xpre <- xscen$x
  colnames(xscen$x) <- names(data)
  colnames(xscen$xpre) <- names(data)
  if (!is.null(names)) {
    row.names(xscen$x) <- names
    row.names(xscen$xpre) <- names
  }
  if (!is.null(formula)) {

    # Get terms attribute
    tl <- attributes(terms(formula))$term.labels

    # Loop over terms
    for (i in 1:length(tl)) {
      tlCur <- tl[i]

      # Check for logitBound transformations
      if (substr(tlCur,1,11)=="logitBound(") {
        # if found, check number of terms needed.
        varname <- substr(tlCur,start=12,stop=nchar(tlCur)-1)
        subform <- as.formula(paste("~",varname,"-1"))
        toLT <- as.vector(model.matrix(subform,data=data))
        testLT <- as.matrix(logitBound(toLT))

        # revise formula so logitBound() call includes "forceAny" and/or "forceAll" as needed
        if (any(colnames(testLT)=="any")) {
          tlCur <- paste(substr(tlCur,start=1,stop=nchar(tlCur)-1), ", forceAny=TRUE)",sep="")
        }
        if (any(colnames(testLT)=="all")) {
          print(testLT)
          tlCur <- paste(substr(tlCur,start=1,stop=nchar(tlCur)-1), ", forceAll=TRUE)",sep="")
        }
        tl[i] <- tlCur
        rhs <- paste(tl, collapse = " + ")
        newform <- as.formula(paste("lhs ~", rhs), env=.GlobalEnv)
        newform[[2L]] <- formula[[2L]]
        formula <- newform
      }

      # Check for logBound transformations
      if (substr(tlCur,1,9)=="logBound(") {
        # if found, check number of terms needed.
        varname <- substr(tlCur,start=10,stop=nchar(tlCur)-1)
        subform <- as.formula(paste("~",varname,"-1"))
        toLT <- as.vector(model.matrix(subform,data=data))
        testLT <- as.matrix(logitBound(toLT))
        # revise formula so logBound() call includes "forceAny" as needed
        if (any(colnames(testLT)=="any")) {
          tlCur <- paste(substr(tlCur,start=1,stop=nchar(tlCur)-1), ", forceAny=TRUE)",sep="")
        }
        tl[i] <- tlCur
        rhs <- paste(tl, collapse = " + ")
        newform <- as.formula(paste("lhs ~", rhs), env=.GlobalEnv)
        newform[[2L]] <- formula[[2L]]
        formula <- newform
      }
    }

    xscen$model <- formula
  }
  class(xscen) <- c("list","counterfactual")

  # Check for extrapolation
  if (hull&&(!is.null(formula))&&(!is.null(data))) {
    require(WhatIf)
    wi <- whatif(formula=formula, data=data, cfact=xscen$x)
    xscen$extrapolatex <- !wi$in.hull
    wi <- whatif(formula=formula, data=data, cfact=xscen$xpre)
    xscen$extrapolatexpre <- !wi$in.hull
    xscen$extrapolatefd <- xscen$extrapolatex|xscen$extrapolatexpre
    xscen$data <- data
    if (any(c(xscen$extrapolatex,xscen$extrapolatexpre,xscen$extrapolatefd)==FALSE)) {
      warning("Some counterfactuals involve extrapolation outside the convex hull")
      if (any(xscen$extrapolatex==FALSE)) {
        print(c("x scenarios:  ",row.names(x)[xscen$extrapolatex]))
      }
      if (any(xscen$extrapolatexpre==FALSE)) {
        print(c("xpre scenarios:  ",row.names(xpre)[xscen$extrapolatexpre]))
      }
      if (any(xscen$extrapolatefd==FALSE)) {
        print(c("first diff scenarios:  ",row.names(x)[xscen$extrapolatefd]))
      }
    }
  }

  xscen
}

cfChange <- function(xscen,covname,x=NULL,xpre=NULL,scen=1) {
  if (!is.null(x))
    xscen$x[scen,covname] <- x
  if (!is.null(xpre))
    xscen$xpre[scen,covname] <- xpre
  if (!is.null(xscen$extrapolatex)) {
    require(WhatIf)
    wi <- whatif(formula=xscen$model, data=xscen$data, cfact=xscen$x)
    xscen$extrapolatex <- !wi$in.hull
    wi <- whatif(formula=xscen$model, data=xscen$data, cfact=xscen$xpre)
    xscen$extrapolatexpre <- !wi$in.hull
    xscen$extrapolatefd <- xscen$extrapolatex|xscen$extrapolatexpre
    if (any(c(xscen$extrapolatex,xscen$extrapolatexpre,xscen$extrapolatefd)==FALSE)) {
      warning("Some counterfactuals involve extrapolation outside the convex hull")
      if (any(xscen$extrapolatex==FALSE)) {
        print(c("x scenarios:  ",row.names(x)[xscen$extrapolatex]))
      }
      if (any(xscen$extrapolatexpre==FALSE)) {
        print(c("xpre scenarios:  ",row.names(xpre)[xscen$extrapolatexpre]))
      }
      if (any(xscen$extrapolatefd==FALSE)) {
        print(c("first diff scenarios:  ",row.names(x)[xscen$extrapolatefd]))
      }
    }

  }

  xscen
}

linearsimev <- function(x,b,ci=0.95,constant=1,sigma2=NULL,sims=10,save=FALSE,nscen=1) {
  if (is.null(x)) {
    if (is.na(constant))
      stop("either x must be given, or a constant specified")
    else
      x <- matrix(1,nrow=nscen,ncol=1)
  } else {
    if (any(class(x)=="counterfactual")&&!is.null(x$model)) {
      x <- model.matrix(x$model,x$x)
    } else {
      if (any(class(x)=="list")) x <- x$x
      if (is.data.frame(x)) x <- as.matrix(x)
      if (!is.matrix(x)) {
        if (is.matrix(b)) {
          x <- t(x)
          if (!is.na(constant)) {
            x <- append(x,1,constant-1)
          }
        } else {
          x <- as.matrix(x)
          if (!is.na(constant)) {
            x <- appendmatrix(x,rep(1,nrow(x)),constant)
          }
        }
      } else {
        if (!is.na(constant)) {
          x <- appendmatrix(x,rep(1,nrow(x)),constant)
        }
      }
    }
  }
  esims <- nrow(as.matrix(b))

  if (!is.null(sigma2)) {
    predict <- TRUE
    sigma <- sqrt(sigma2)
  } else
    predict <- FALSE

  nscen <- nrow(x)
  nci <- length(ci)
  res <- list(pe = rep(NA, nscen),
              lower = matrix(NA,nrow=nscen,ncol=nci),
              upper = matrix(NA,nrow=nscen,ncol=nci))
  if (predict) {
    res$plower <- matrix(NA,nrow=nscen,ncol=nci)
    res$pupper <- matrix(NA,nrow=nscen,ncol=nci)
  }
  if (save) {
    res$ev <- matrix(NA,nrow=nscen,ncol=esims)
    if (predict) {
      res$pv <- matrix(NA,nrow=nscen,ncol=esims*sims)
    }
  }
  for (i in 1:nscen) {
    simmu <- b%*%x[i,]
    if (save) res$ev[i,] <- simmu
    res$pe[i] <- mean(simmu)
    for (k in 1:nci) {
      cint <- quantile(simmu,probs=c( (1-ci[k])/2, (1-(1-ci[k])/2) ) )
      res$lower[i,k] <- cint[1]
      res$upper[i,k] <- cint[2]
    }

    # Simulate predicted values if requested
    if (predict) {
      pv <- rnorm(sims*esims,mean=simmu,sd=sigma)
      if (save)
        res$pv[i,] <- pv
      for (k in 1:nci) {
        cint <- quantile(pv,probs=c( (1-ci[k])/2, (1-(1-ci[k])/2) ) )
        res$plower[i,k] <- cint[1]
        res$pupper[i,k] <- cint[2]
      }
    }
  }
  res$lower <- drop(res$lower)
  res$upper <- drop(res$upper)
  if (predict) {
    res$plower <- drop(res$plower)
    res$pupper <- drop(res$pupper)
  }
  res
}

probe.low.1sd<-function(MOD)
{
  low<-(MOD-mean(MOD,na.rm=TRUE))+sd(MOD, na.rm=TRUE)
  return(low)
}

#At two standard deviations below:
probe.low.2sd<-function(MOD)
{
  low<-(MOD-mean(MOD,na.rm=TRUE))+2*sd(MOD, na.rm=TRUE)
  return(low)
}

#Below is a function for probing interactions at high levels of a moderator
#by centering a the variable at one standard deviation below the mean.
probe.high.1sd<-function(MOD)
{
  high<-(MOD-mean(MOD, na.rm=TRUE))-sd(MOD, na.rm=TRUE)
  return(high)
}

#At two standard deviations above:
probe.high.2sd<-function(MOD)
{
  high<-(MOD-mean(MOD, na.rm=TRUE))-2*sd(MOD, na.rm=TRUE)
  return(high)
}

lighten <- function(col,
                    pct=0.75,
                    alpha=1){
  if (abs(pct)>1) {
    warning("invalid pct in lighten(); must be between 0 and 1.")
    pcol <- col2rgb(col)/255
  } else {
    col <- col2rgb(col)/255
    if (pct>0) {
      pcol <- col + pct*(1-col)
    } else {
      pcol <- col*pct
    }
  }
  rgb(pcol[1], pcol[2], pcol[3], alpha)
}

extract <- function(MOD, regressor) {
  tmp = numeric()
  tmp[1] <- MOD$coef[regressor] #run it, and put the coefficient in cell [i,2] of the table I made
  tmp[2] <- summary(MOD)$coef[regressor,2] #run it, and put the standard error in cell [i,3] of the table I made
  tmp[3] <- confint(MOD)[regressor,1] #run it, and put the lower limit of my 95% CI in cell [i,4] of the table I made
  tmp[4] <- confint(MOD)[regressor,2] #same for upper limit
  return(tmp)
}

pvals<-round((seq(0,1,.001))**3,9)
colfunc <- colorRampPalette(c("#0016A8", "#CFD2EE"))
whites<-rep("#CFD2EE",500)
pcols<-c(colfunc(501),whites)
seq<-(seq(1,length(pvals),1))

pcolslight <- sapply(pcols, function(i) {
  pcols<- lighten(i,pct=0.50)
})

dfcol<-as.data.frame(cbind(seq,pvals))
dfcol$pcols<-pcols
dfcol$pcolslight<-pcolslight

bluespal<-c("#08306B","#2171B5","#4292C6","#9ECAE1","#C6DBEF")
# bluespal<-c("#08306B","#9ECAE1","#4292C6","#2171B5","#08306B")
greyspal<-c("#000000","#252525","#525252","#737373","#969696")
# bluespal<-rep("black",5)
names(bluespal)<-c("less001","less01","less05","less10","greater10")
names(greyspal)<-c("less001","less01","less05","less10","greater10")

# lighterpal<-sapply(pal, function(i) {
#   pal<- lighten(i,pct=0.50)
# })
require(RColorBrewer)
lightergreys<-c(lighten(brewer.pal(5,"Greys")[1],pct=0.50), #lighten each by 50%
                lighten(brewer.pal(5,"Greys")[2],pct=0.50),
                lighten(brewer.pal(5,"Greys")[3],pct=0.50),
                lighten(brewer.pal(5,"Greys")[4],pct=0.50),
                lighten(brewer.pal(5,"Greys")[5],pct=0.50))

lighterblues<-c(lighten(brewer.pal(5,"Blues")[1],pct=0.50), #lighten each by 50%
                lighten(brewer.pal(5,"Blues")[2],pct=0.50),
                lighten(brewer.pal(5,"Blues")[3],pct=0.50),
                lighten(brewer.pal(5,"Blues")[4],pct=0.50),
                lighten(brewer.pal(5,"Blues")[5],pct=0.50))

textInputRow<-function (inputId, label, value = "")
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId),
      tags$input(id = inputId, type = "text", value = value,class="input-small"))
}

# END HELPER FUNCTIONS ----------------------------------------------------


  function(input, output) {
    values<-reactiveValues() #defines empty list that will take model-produced values
    output$msg<-renderText({
    if (is.null(input$file1)){
      "Please upload your dataset to begin."
    } else(values$msg<-NULL)
    })

# DATAFRAME UPLOAD CODE ---------------------------------------------------

    df<-reactive({
      if (!is.null(input$file1)){
        if(input$file1$type == "text/csv"){
          read.csv(input$file1$datapath, header = TRUE)
        }
        else{
          read.spss(input$file1$datapath, to.data.frame=TRUE)
        }
      }
    })

#1. Define Dynamic UI Outputs -----------------------------------------------

    output$ui.categorical <- renderUI ({
      if (is.null(input$file1))
        return()
      checkboxInput(inputId="cat", label = h5("Categorical Moderator"),
                    value=FALSE)
    })

# Continuous Variable Scaling Options -----------------------------------------------

output$ui.scale <- renderUI ({
  if (is.null(input$file1))
    return()
  radioButtons(inputId="scale", label = h4("Continuous Variable Scaling:"),
               c("Raw" = "raw", "Centered" = "cen", "Standardized" = "std"),
               selected = "cen")
})

# GLM Family Options ------------------------------------------------------

# Building the model options ------------------------------------------

    output$ui.manual <- renderUI ({
      if (is.null(input$file1))
        return()
      checkboxInput(inputId="manual", label = h5("Manual Formula"),
                    value=FALSE)
    })

#Specify covariates for the model

output$ui.covars <- renderUI ({
  if (is.null(input$file1))
    return()

  if (input$manual==TRUE)
    return()

  selectInput(inputId = "covars",
              label = "Choose your covariate(s):",
              choices = colnames(df()),
              selected = "",
              multiple = TRUE,
              selectize = TRUE)
})

#Specify focal variable (x-axis)

output$ui.foc <- renderUI ({
  if (is.null(input$file1))
    return()

  if (input$manual==TRUE) {textInput("foc", label = "Specify your focal (x-axis) variable:", value = "")}
  else{

  selectInput("foc",
              label = "Choose your focal (x-axis) variable:",
              choices = colnames(df()),
              selected = "")
  }
})

# Specify moderator variable (appearing on legend dimension)

output$ui.mod <- renderUI ({
  if (is.null(input$file1))
    return()

  if (input$manual==TRUE) {textInput("mod", label = "Specify your moderator (legend) variable:", value = "")}
  else{

  selectInput("mod",
              label = "Choose your moderator (legend) variable:",
              choices = colnames(df()),
              selected = "")
  }
})

#Specify outcome variable (appearing on y-axis)
output$ui.dv <- renderUI ({
  if (is.null(input$file1))
    return()

  if (input$manual==TRUE)
    return()

  selectInput("dv",
              label = "Choose your dependent (y-axis) variable:",
              choices = colnames(df()),
              selected = "")
})

# specifying polynomial terms (right now, just quadratic)
output$ui.poly <- renderUI ({
  if (is.null(input$file1))
    return()

  if (input$manual==TRUE)
    return()

radioButtons(inputId="poly", label = h4("Form of Focal Predictor:"),
             c("Linear" = "lin", "Quadratic" = "quad"),
             selected = "lin")
})

output$ui.formula <- renderUI ({
  if (is.null(input$file1))
    return()

  if (input$manual==FALSE)
    return()

  textAreaInput("manform", label = "Formula", value = "")
})

# Define "go" button to run the model

# DEFINE CUSTOMIZATION OPTIONS
#X axis label
output$ui.xlab<-renderUI ({
  if (is.null(input$file1))
    return()

textInput(inputId="xaxislab",
          label="X-Axis label:",
          value=input$foc)
})

#Y axis label
output$ui.ylab<-renderUI ({
  if (is.null(input$file1))
    return()

  textInput(inputId="yaxislab",
            label="Y-Axis label:",
            value=input$dv)
})

output$ui.title<-renderUI ({
  if (is.null(input$file1))
    return()

  textInput(inputId="title",
            label="Title of Graph:",
            value=paste("Level of Moderator (",input$mod,")"))
})

output$ui.greyscale <- renderUI ({
  if (is.null(input$file1))
    return()
  checkboxInput(inputId="greyscale", label = h5("Greyscale"),
                value=FALSE)
})

output$ui.go <- renderUI ({
  if (is.null(input$file1))
    return()
actionButton("go","Run Analysis", width = "100%", icon = icon("fa fa-thumbs-up"))
})

output$ui.rosgo <- renderUI ({
  if (is.null(input$file1))
    return()
  actionButton("go","Make RoS plot", width = "100%", icon = icon("fa fa-thumbs-up"))
})

observeEvent(input$go, { #Once the "go" button is hit, InterActive looks at all the ui input and runs the model.
  output$ui.downloadplot <- renderUI ({
    if (is.null(input$file1))
      return()
    downloadButton('downloadPlot', 'Download Plot')
  })

  output$ui.modlevel<- renderUI ({
    if (is.null(input$file1))
      return()
    if (input$static==TRUE)
      return()

    data<-df() #Pull data from uploaded data set
    mod <- input$mod #Define moderator from input
    if(input$cat==FALSE){
    if (input$scale == "cen"){mod.temp <- scale(data[,mod], scale = FALSE, center = TRUE)}
    if (input$scale == "std"){mod.temp <- scale(data[,mod], scale = TRUE)}
    }

    #Defines anchors for the slider, ranging from -3 to +3 SDs
    modlow <- -3*sd(mod.temp, na.rm = TRUE)
    modmean <- mean(mod.temp, na.rm = TRUE)
    modhigh <- 3*sd(mod.temp, na.rm = TRUE)

    values$modhyplow<-round(modlow,2)
    values$modhypmean<-round(modmean,2)
    values$modhyphigh<-round(modhigh,2)

    # DEFINE MODERATOR SLIDER
    wellPanel(sliderInput("modlevel", "Level of Moderator:",
                round(modlow,2),
                round(modhigh,2),
               round(modmean,2),
                step = .1, animate=
                  animationOptions(interval=1000, loop=TRUE)))

  })


  # Static display button
  output$ui.static <- renderUI ({
    if (is.null(input$file1))
      return()
    checkboxInput(inputId="static", label = h5("Static Display"),
                  value=TRUE)
  })

#2. Analyze and Report Results in Table -------------------------------------

#output$modplot creates a plot graphic, either dynamic or static, based upon the selected model and the given level of a moderator.

# PLOT CREATOR ------------------------------------------------------------
  output$modplot <- renderPlot({
      if (is.null(input$file1))
        return(NULL)

  # Reproduce Model Results -------------------------------------------------

      data<-df() #pull in defined data
      dftrue<-df()
      mod <- input$mod #define moderator
      foc <- input$foc #define focal predictor
      if( input$manual == TRUE){dv <- sub('\\ ~.*', '', format(input$manform))}
      else{dv <- input$dv}

      if (input$scale == "cen"){
        if(input$cat==FALSE){data[,mod] <- scale(data[,mod],  scale = FALSE, center = TRUE)}
        data[,foc] <- scale(data[,foc],  scale = FALSE, center = TRUE)
        if(input$cat==FALSE){dftrue[,mod] <- scale(dftrue[,mod],  scale = FALSE, center = TRUE)}
        dftrue[,foc] <- scale(dftrue[,foc],  scale = FALSE, center = TRUE)

      }
      if (input$scale == "std"){
        if(input$cat==FALSE){data[,mod] <- scale(data[,mod], scale = TRUE)}
        data[,foc] <- scale(data[,foc], scale = TRUE)
        if(input$cat==FALSE){dftrue[,mod] <- scale(dftrue[,mod],  scale = TRUE)}
        dftrue[,foc] <- scale(dftrue[,foc],  scale = TRUE)
      }


      values$modlevel<-input$modlevel
      if(input$static == TRUE) (values$modlevel<- mean(data[,mod], na.rm = TRUE))
      modlevel <- values$modlevel

      if(input$cat==FALSE){data[,mod]<-data[,mod]-values$modlevel} #NOTE: This MUST come after the input$scale parts above!!!!!

      #Here, I build a formula based on user inputs.
      if(input$manual == FALSE){
      cov<-paste(input$covars, collapse=" + ")#define covariate portion based on what the user specified

      if (input$poly == "quad"){ #if user selects a quadratic focal term, the below code will be run.
        focmod<-paste("+",
                      foc,"+",
                      "I(",foc,"^2)","+", #Key difference: specifies an additonal quadratic term here.
                      mod,"+",
                      foc,"*",
                      mod, "+",
                      "I(",foc,"^2)","*", #Also specifies a quadratic interaction.
                      mod)
      }
      else{
        #here, I paste together a formula based on user inputs for a standard linear model.
        focmod<-paste("+",foc,"+",mod,"+",foc,"*",mod)} #Define all predictor terms (including interaction).
      }

        if(input$manual == FALSE){dep<-paste(input$dv, "~")} # Define outcome variable
        # else{dep<-sub('~*', '', input$formula)}

        if (input$manual==TRUE){values$form<-as.formula(input$manform)}
        else{values$form<-as.formula(paste(dep,cov,focmod))}
        formula<-values$manform

          values$m<-lm(values$form,data, na.action = na.omit)
          values$mtrue<-lm(values$form,dftrue, na.action = na.omit)

        if (input$manual==FALSE){
          values$model<-as.formula(paste(dep,cov,focmod))
          } #need just one part of the model for two-part models to do simulations later

      m <- values$m
      mtrue <- values$mtrue

# Crossover point computation using reparameterization method

      values$co<- -(m$coefficients[mod]/(m$coefficients[paste0(foc,":",mod)]))

      repar.df<-as.data.frame(cbind(data[,foc],data[,mod], data[,dv]))
      colnames(repar.df)<-c("focal","moder","outcome")

      m.repar<-nls(outcome ~ B0 + B1*(focal - C) + B3*((focal - C)*moder),
          data = repar.df,
          start = list(B0 = m$coefficients["(Intercept)"],
                       B1 = m$coefficients[foc],
                       C = values$co,
                       B3 = m$coefficients[paste0(foc,":",mod)]))
      values$m.repar<-m.repar

  # DEFINE FOR STATIC PLOTS -------------------------------------------------
      if(input$manual == TRUE){
        values$form2<-as.formula(gsub(mod, "s.mod", format(input$manform)))

values$model2<-values$form2
        }
      else{
      cov<-paste(input$covars, collapse="+") #define covariate portion based on what the user specified
      if (input$poly == "quad"){ #if user selects a quadratic focal term, the below code will be run.
        focmod2<-paste("+",
                       foc,"+",
                       "I(",foc,"^2)","+", #Key difference: specifies an additonal quadratic term here.
                       "s.mod","+",
                       foc,"*",
                       "s.mod", "+",
                       "I(",foc,"^2)","*", #Also specifies a quadratic interaction.
                       "s.mod")
      }
      else{
        #here, I paste together a formula based on user inputs for a standard linear model.
        focmod2<-paste("+",foc,"+","s.mod","+",foc,"*","s.mod")
        } #Define all predictor terms
      dep2<-paste(input$dv, "~") # Define outcome variable
      }

#DEFINE STATIC PLOT FOR CATEGORICAL MODERATOR
      if(input$cat==TRUE){

          if(input$manual == FALSE){
            values$form2<-as.formula(paste(dep2,cov,focmod2))
            values$model2<-as.formula(paste(dep2,cov,focmod2))
          }

          data$s.mod <- data[,mod]-min(unique(data[,mod]),na.rm=TRUE)
          m.cat0<-lm(values$form2,data, na.action = na.omit)
          data$s.mod <- data[,mod]-max(unique(data[,mod]),na.rm=TRUE)
          m.cat1<-lm(values$form2,data, na.action = na.omit)

      }
      else{

        if(input$manual == FALSE){
      values$form2<-as.formula(paste(dep2,cov,focmod2))
      values$model2<-as.formula(paste(dep2,cov,focmod2))
        }

      data$s.mod <- probe.low.2sd(data[,mod])
      m.2low<-lm(values$form2,data, na.action = na.omit)
      data$s.mod <- probe.low.1sd(data[,mod])
      m.1low<-lm(values$form2,data, na.action = na.omit)
      data$s.mod <- data[,mod]
      m.mean<-lm(values$form2,data, na.action = na.omit)
      data$s.mod <- probe.high.1sd(data[,mod])
      m.1high<-lm(values$form2,data, na.action = na.omit)
      data$s.mod <- probe.high.2sd(data[,mod])
      m.2high<-lm(values$form2,data, na.action = na.omit)

   }
      data$s.mod <- data[,mod]

# MAKING RoS PLOT ---------------------------------------------------------

df.ros<-dftrue
df.ros[,foc] <- scale(df.ros[,foc])
df.ros[,mod] <- scale(df.ros[,mod])
# df.ros[,dv]<- scale(df.ros[,mod])

hyp.Z <- seq(-3,3,.1)

      ros <- sapply(hyp.Z, function(i) {
        df.ros[,mod] <- round(df.ros[,mod],2) - i
        mod <- lm(values$form,df.ros, na.action = na.omit)
        tmp <- c(i, extract(mod, foc))})

      ros <- data.frame(t(ros))
      colnames(ros) <- c("hyp.Z","pe.X","SE","LL","UL")
      ros$significance[(ros$LL*ros$UL) > 0] <- "sig"
      ros$significance[(ros$LL*ros$UL) <= 0] <- "not sig"
      ros.sig<-ros[ros$significance=="sig",]
      if(min(ros.sig$hyp.Z) < 0 && max(ros.sig$hyp.Z) > 0){
      ros.siglow<-ros[ros$significance=="sig" & ros$hyp.Z < 0,"hyp.Z"]
      ros.sighigh<-ros[ros$significance=="sig" & ros$hyp.Z > 0,"hyp.Z"]
      }
      else(ros.siglow<-ros.sighigh<-ros[ros$significance=="sig","hyp.Z"])

      values$ros<-ros
      values$ros.sighigh<-ros.sighigh

#Creating the RoS plot
      rosplot <- ggplot() +
        geom_ribbon(data = ros, aes(hyp.Z, pe.X), ymin = ros$LL, ymax = ros$UL,
                    fill=brewer.pal(3,"Greys")[3], alpha = .25) +
        geom_line(data = ros, aes(hyp.Z, pe.X), color="Black", size = 1.25) +
        scale_x_continuous(breaks = c(-3,-2,-1,0,1,2,3),labels = c(-3,-2,-1,0,1,2,3),limits = c(-3,3)) +
        ylim(min(ros$LL),max(ros$UL)) +
        geom_hline(yintercept = 0) +
        # geom_vline(xintercept = max(ros.siglow), linetype="dashed") +
        # geom_vline(xintercept = max(ros.sighigh), linetype="dashed") +
        xlab("Moderator") +
        ylab("Simple Slope of\nFocal Predictor") +
        geom_rug(data = df.ros, aes(x=df.ros[,mod])) +
        # geom_density(data = df.ros, aes(x=df.ros[,mod]), linetype = "dotted", fill=brewer.pal(3,"Greys")[2], alpha = .25, position = position_dodge(width = .2)) +
        theme_bw()

values$rosplot<-rosplot

# Define Dynamic Plots ----------------------------------------------------

      # data[,mod]<-data[,mod]-modlevel

      values$resfoc.form1<-as.formula((gsub(paste0(foc," \\+ "),"",format(values$form))))
      values$resdv.form<-as.formula((gsub(paste0("\\+ ",mod," \\* ",foc),"",format(values$resfoc.form1))))
      values$resfoc.form<-as.formula(paste(foc,"~",sub(".*~ ","",format(values$resdv.form))))

      dfpoints<-as.data.frame(cbind(data[,foc],glm(values$form,data, na.action = na.omit,family="gaussian")$y,m$fitted.values))
      colnames(dfpoints)<-c("pred","y","fitted")

      min2sdlab<-paste0("-2 SD")
      min1sdlab<-paste0("-1 SD")
      meanlab<-paste0("Mean\n")
      plus1sdlab<-paste0("+1 SD")
      plus2sdlab<-paste0("+2 SD")

      if(input$static == FALSE) {
      #Begin new code for simulating estimates using simcf

      m.pe<-mtrue$coefficients #specify coefficients
      m.vcov<-vcov(mtrue) #specify variance-covariance matrix

      #Define a sequence of counterfactuals across the range of observed values for my focal predictor. NOTE: this is potentially problematic because extreme scores can influence whether we are simulating across a range where there are minimal/no data points represented. I will work out a solution in a future iteration of this project.
      focal.seq <- c(0,seq(min(data[,foc], na.rm = TRUE),max(data[,foc], na.rm = TRUE),.1))

      focal.hyp <-cfMake(values$form, dftrue, nscen = length(focal.seq)) #make counterfactuals
      #
      for (j in 1:length(focal.seq)){
         for (l in 2:(length(all.vars(values$form)))){#This loop ensures we loop through all predictors in our model for however many the user specified (minus the intercept and interaction term).
          focal.hyp <- cfChange(focal.hyp, all.vars(values$form)[l], x=mean(m$model[,l], na.rm = TRUE), scen=j) #Hold covariates at their mean
          focal.hyp <- cfChange(focal.hyp, mod, x=modlevel, scen=j)#Based on user input, specify the value where my moderator will be held constant.
          focal.hyp <- cfChange(focal.hyp, foc, x=focal.seq[j], scen=j) #create scenarios for the jth counterfactual of my focal predictor.
         }
      }
      sims <- 10000
      focal.hyp$model <- values$model

      simparam <- mvrnorm(sims,m.pe,m.vcov) # draw parameters


  # Define Dynamic Plot

#Linear

      simbetas <- simparam[,1:length(m.pe)]
      focal.hyp.Ysims <- linearsimev(focal.hyp, simbetas, ci=.95)
      # paste(focal.hyp$model)
    #Now, we put the simulated values into a dataframe to be read in by ggplot2

    df.plot<-data.frame(
      focal.seq, #focal hypothetical values
      focal.hyp.Ysims$pe, #point estimates
      focal.hyp.Ysims$lower, #simulated lower limits
      focal.hyp.Ysims$upper, #simulated upper limits

      rep(summary(m)$coefficients[foc,"Pr(>|t|)"],length(focal.seq)))

    #Add column names
    colnames(df.plot)<-c("focal.seq","pe","lower","upper","p.val")

    #This looks up the color corresponding to the p-value associated with the PE of my focal predictor, and stores it in the dataframe for use in ggplot.
    df.plot$col<-dfcol[which(round(dfcol$pvals,20)==round(summary(m)$coefficients[foc,"Pr(>|t|)"]),20),"pcols"]

    #Finally, I build the graphic using ggplot2, commented below:
    plot.lin <- ggplot() +

      geom_point(data=dfpoints, aes(pred,fitted),size = .75, alpha = .5) +

      #Specifies confidence intervals referencing the lower and upper CI values
      geom_ribbon(data=df.plot, aes(x=focal.seq, y=pe, ymin = df.plot$lower, ymax = df.plot$upper, fill=df.plot$col), alpha = .25) +

      #Manual specification of the color. This looks up the color corresponding to the p-value associated with the PE of my focal predictor, and stores it in the dataframe for use in ggplot. I use the color (pcols) as the line color and a lightened version (pcolslight) for the confidence region shading.
      scale_colour_manual(values=brewer.pal(5,"Blues")[5]) +
      scale_fill_manual(values=lighten(brewer.pal(5,"Blues")[5])) +

      #Include fit lines
      geom_line(data = df.plot, aes(focal.seq, pe, fill=col, color=col), size=2) +

      #Specify my y limits as the minimum and maximum observed in the data. Currently, this is just two standard deviations below and above my minimum and maximum observed Y values to allow for my confidence regions to be displayed. I intend on improving this in the future.

  ylim(min(data[,dv]-2*sd(data[,dv], na.rm = TRUE)), max(data[,dv]+2*sd(data[,dv], na.rm = TRUE))) +
xlim(min(data[,foc], na.rm = TRUE),max(data[,foc], na.rm = TRUE)) +

      #thematic specifications of my graphic
      theme(text=element_text(family="Trebuchet MS",size=15, color="black"),
            legend.position="none",
            panel.background = element_blank(),
            legend.background = element_rect(fill = "white"),
            legend.title=element_blank(),
            legend.key = element_rect(fill = "white"),
            panel.grid.minor = element_blank(),
            axis.text.x=element_text(colour="black"),
            axis.text.y=element_text(colour="black"),
            axis.title.y=element_text(size=15),
            panel.grid.major = element_line(colour="#C0C0C0"),
            plot.background=element_rect(fill='white')) +
      labs(fill = mod, color = mod, linetype=mod) +

      #These are labels based on the current data, but I will build this out to be more dynamic in the future, allowing users to specify their own axis and plot labels.
      labs(x = foc, y = dv) +
      coord_cartesian() +

      #Across the title, I provide the point estimate and p-value for the focal predictor at the given level of the moderator. Note that there are some minor rounding issues I need to fix that result in p-values equalling zero (not possible).
      ggtitle(paste("Level of Moderator (",mod,") =", modlevel,"\n",foc," PE = ",round(summary(m)$coefficients[foc,"Estimate"],3), ", p = ",round(summary(m)$coefficients[foc,"Pr(>|t|)"],3))) +

      #I provide horizonal lines at the observed minimum and maximum of my outcome  variable. This helps the reader understand whether they are extropolating their prediction lines outside the range of observed data. Again, I will be building this out to come up with an effective solution for doing this with focal and moderator variables as well.

      geom_hline(yintercept=c(max(data[,dv], na.rm = TRUE),min(data[,dv], na.rm = TRUE)),linetype=2) +
      theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=15, hjust=0, color="black"))

    #       {
      # annotate("errorbarh", x = values$co, y = df.plot[which(round(df.plot$focal.seq,1)==round(values$co,1)),"pe"],
      #         xmin= confint(m.repar)["C","2.5%"],
      #          xmax=confint(m.repar)["C","97.5%"],
      #          colour = "black", size = 2, alpha = .5, linetype = "solid", height = 5) +
      #   # }
      #
      #

    if(values$co <= max(df.plot$focal.seq) & values$co >= min(df.plot$focal.seq))
       {
      plot.lin <- plot.lin +
        annotate("point", x = values$co, y =
                 # as.numeric(mode(df.plot$pe)),
               df.plot[which(round(df.plot$focal.seq,1)==round(values$co,1)),"pe"],
               color= "black", fill = "orange", size = 3, shape = 23)
    }

    # if(summary(m)$coefficients[paste0(foc,":",mod),"Pr(>|t|)"] > .05){
    #   plot.lin <- plot.lin + annotate("text", x = mean(data[,input$foc], na.rm = TRUE), y = median(data[,input$dv], na.rm = TRUE), label = "NOT SIGNIFICANT", size = 7)
    # }
    values$df.plot<-df.plot
plot.lin
      }

#Below is similar code, applied instead to creating a static graphic using the principles of small multiples.
#This is similar to the pick-a-point approach of probing and graphing interactions, though offers graphics across a greater range than 3 points (now we do 5).

else{

# Define Static Plots ------------------------------------------------------
  if(input$cat==TRUE){

      m.cat0.pe<-m.cat0$coefficients #specify coefficients
      m.cat0.vcov<-vcov(m.cat0) #specify variance-covariance matrix

      m.cat1.pe<-m.cat1$coefficients #specify coefficients
      m.cat1.vcov<-vcov(m.cat1) #specify variance-covariance matrix


    focal.seq <- seq(min(data[,foc], na.rm = TRUE),max(data[,foc], na.rm = TRUE),.1)
    focal.hyp <-cfMake(values$form2, data, nscen = length(focal.seq)) #make counterfactuals
    sims <- 10000
    form2<-values$form2
    focal.hyp$model <- values$model2
    simparam.cat0 <- mvrnorm(sims,m.cat0.pe,m.cat0.vcov) # draw parameters
    simparam.cat1 <- mvrnorm(sims,m.cat1.pe,m.cat1.vcov) # draw parameters

    simbetas.cat0 <- simparam.cat0[,1:length(m.cat0.pe)]
    simbetas.cat1 <- simparam.cat1[,1:length(m.cat1.pe)]

    # Category 0 plot

    for (j in 1:length(focal.seq)){
      for (l in 2:(length(all.vars(values$form2)))){
        focal.hyp <- cfChange(focal.hyp, all.vars(form2)[l], x=mean(m.cat0$model[,l]), scen=j) #Hold covariates at their mean
        focal.hyp <- cfChange(focal.hyp, "s.mod", x=0, scen=j)#Based on user input, specify the value where my moderator will be held constant.
        focal.hyp <- cfChange(focal.hyp, foc, x=focal.seq[j], scen=j) #create scenarios for the jth counterfactual of my focal predictor.
      }
    }

      focal.hyp.Ysims.cat0 <- linearsimev(focal.hyp, simbetas.cat0, ci=.95)
    # Category 1 plot

    for (j in 1:length(focal.seq)){
      for (l in 2:(length(all.vars(values$form2)))){
        focal.hyp <- cfChange(focal.hyp, all.vars(values$form2)[l], x=mean(m.cat1$model[,l]), scen=j) #Hold covariates at their mean
        focal.hyp <- cfChange(focal.hyp, "s.mod", x=0, scen=j)#Based on user input, specify the value where my moderator will be held constant.
        focal.hyp <- cfChange(focal.hyp, foc, x=focal.seq[j], scen=j) #create scenarios for the jth counterfactual of my focal predictor.
      }
    }

      focal.hyp.Ysims.cat1 <- linearsimev(focal.hyp, simbetas.cat1, ci=.95)


    # Create Static Plots

    #Create static plots for non-two-part models

      df.plot.cat0<-data.frame(
        focal.seq, #focal hypothetical values
        focal.hyp.Ysims.cat0$pe, #point estimates
        focal.hyp.Ysims.cat0$lower, #simulated lower limits
        focal.hyp.Ysims.cat0$upper, #simulated upper limits
        rep(paste0("0\nBeta = ",round(lm.beta(m.cat0)[foc],3),"\np = ",round(summary(m.cat0)$coefficients[foc,"Pr(>|t|)"],3))),
        rep(summary(m.cat0)$coefficients[foc,"Pr(>|t|)"],length(focal.seq)))
      #Add column names
      colnames(df.plot.cat0)<-c("focal.seq","pe","lower","upper","level","p.val")
      df.plot.cat0$col<-dfcol[which(round(dfcol$pvals,20)==round(summary(m.cat0)$coefficients[foc,"Pr(>|t|)"]),20),"pcols"]

      df.plot.cat1<-data.frame(
        focal.seq, #focal hypothetical values
        focal.hyp.Ysims.cat1$pe, #point estimates
        focal.hyp.Ysims.cat1$lower, #simulated lower limits
        focal.hyp.Ysims.cat1$upper, #simulated upper limits
        rep(paste0("1\nBeta = ",round(lm.beta(m.cat1)[foc],3),"\np = ",round(summary(m.cat1)$coefficients[foc,"Pr(>|t|)"],3))),
        rep(summary(m.cat1)$coefficients[foc,"Pr(>|t|)"],length(focal.seq)))
      #Add column names
      colnames(df.plot.cat1)<-c("focal.seq","pe","lower","upper","level","p.val")
      df.plot.cat1$col<-dfcol[which(round(dfcol$pvals,20)==round(summary(m.cat1)$coefficients[foc,"Pr(>|t|)"]),20),"pcols"]


      df.staticplot<-as.data.frame(rbind(df.plot.cat0,df.plot.cat1))

      df.staticplot$col[df.staticplot$p.val>.10]<-bluespal["greater10"]
      df.staticplot$col[df.staticplot$p.val<=.10 & df.staticplot$p.val>.05]<-bluespal["less10"]
      df.staticplot$col[df.staticplot$p.val<=.05 & df.staticplot$p.val>.01]<-bluespal["less05"]
      df.staticplot$col[df.staticplot$p.val<=.01 & df.staticplot$p.val>.001]<-bluespal["less01"]
      df.staticplot$col[df.staticplot$p.val<=.001]<-bluespal["less001"]

      df.staticplot$greycol[df.staticplot$p.val>.10]<-greyspal["greater10"]
      df.staticplot$greycol[df.staticplot$p.val<=.10 & df.staticplot$p.val>.05]<-greyspal["less10"]
      df.staticplot$greycol[df.staticplot$p.val<=.05 & df.staticplot$p.val>.01]<-greyspal["less05"]
      df.staticplot$greycol[df.staticplot$p.val<=.01 & df.staticplot$p.val>.001]<-greyspal["less01"]
      df.staticplot$greycol[df.staticplot$p.val<=.001]<-greyspal["less001"]

      values$dfplot<-df.staticplot

      dfpoints$p.val[data[,mod]== min(unique(data[,mod]),na.rm=TRUE)]<-round(summary(m.cat0)$coefficients[foc,"Pr(>|t|)"],3)
      dfpoints$p.val[data[,mod]== max(unique(data[,mod]),na.rm=TRUE)] <-round(summary(m.cat1)$coefficients[foc,"Pr(>|t|)"],3)

      dfpoints$level[data[,mod]== min(unique(data[,mod]),na.rm=TRUE)]<-paste0("0\nBeta = ",round(lm.beta(m.cat0)[foc],3),"\np = ",round(summary(m.cat0)$coefficients[foc,"Pr(>|t|)"],3))
      dfpoints$level[data[,mod]== max(unique(data[,mod]),na.rm=TRUE)]<-paste0("1\nBeta = ",round(lm.beta(m.cat1)[foc],3),"\np = ",round(summary(m.cat1)$coefficients[foc,"Pr(>|t|)"],3))

      dfpoints<-na.omit(dfpoints)

      dfpoints$level <- factor(dfpoints$level, levels = c(paste0("0\nBeta = ",round(lm.beta(m.cat0)[foc],3),"\np = ",round(summary(m.cat0)$coefficients[foc,"Pr(>|t|)"],3)),
                                                          paste0("1\nBeta = ",round(lm.beta(m.cat1)[foc],3),"\np = ",round(summary(m.cat1)$coefficients[foc,"Pr(>|t|)"],3))))

      #This messes with my graphs to do, even though it works.
      # dfpoints$level<-gsub("p = 0", "p < .001",dfpoints$level)
      # df.staticplot$level<-gsub("p = 0", "p < .001",df.staticplot$level)
      #
      # dfpoints$resfoc<-lm(values$resfoc.form,m$model, na.action = na.omit)$fitted.values
      # dfpoints$yres<-lm(values$resdv.form,m$model, na.action = na.omit)$fitted.values

      values$dfpoints<-dfpoints

      df.colstemp<-df.staticplot[c("level","col","greycol")]
      df.cols<-df.colstemp[!duplicated(df.colstemp$level), ]
      df.cols$colslight<-sapply(df.cols$col, function(i) {df.cols$col<- lighten(i,pct=0.50)})
      df.cols$greycolslight<-sapply(df.cols$greycol, function(i) {df.cols$greycol<- lighten(i,pct=0.50)})
      df.cols<-as.data.frame(df.cols)
      values$df.cols<-df.cols
      #
      df.staticplot<-df.staticplot[57:225,]
      values$df.staticplot<-df.staticplot

      staticplot<-ggplot() +

        geom_point(data=dfpoints, aes(x=pred,y=y),size = .75, alpha = .5) +
        geom_ribbon(data=df.staticplot, aes(x=focal.seq, y=pe, ymin = lower, ymax = upper, fill = level), alpha = .25) +
        geom_line(data = df.staticplot, aes(focal.seq, pe, fill=level, color=level), size=2) +

        ylim(min(data[,dv]-2*sd(data[,dv],na.rm = TRUE)),max(data[,dv]+2*sd(data[,dv],na.rm = TRUE))) +

        #Similarly, define my x axis based on the minimum and maximum values observed in my data
        xlim(min(data[,foc], na.rm = TRUE),max(data[,foc], na.rm = TRUE)) +

        #Manual specification of the RColorBrewer blue palette

        #thematic specifications of my graphic
        theme(text=element_text(family="Trebuchet MS",size=15, color="black"),
              legend.position="none",
              panel.background = element_blank(),
              legend.background = element_rect(fill = "white"),
              legend.title=element_blank(),
              legend.key = element_rect(fill = "white"),
              panel.grid.minor = element_blank(),
              axis.text.x=element_text(colour="black"),
              axis.text.y=element_text(colour="black"),
              axis.title.y=element_text(size=15),
              panel.grid.major = element_line(colour="#C0C0C0"),
              plot.background=element_rect(fill='white')) +
        labs(fill = mod, color = mod, linetype=mod) +

        #These are labels based on the current data, but I will build this out to be more dynamic in the future, allowing users to specify their own axis and plot labels.
        labs(x = foc, y = dv) +


        coord_cartesian() +

        #Across the title, I provide the point estimate and p-value for the focal predictor at the given level of the moderator. Note that there are some minor rounding issues I need to fix that result in p-values equalling zero (not possible).

        ggtitle(paste0("Level of Moderator (",mod,")")) +

        #I provide horizonal lines at the observed minimum and maximum of my outcome  variable. This helps the reader understand whether they are extropolating their prediction lines outside the range of observed data. Again, I will be building this out to come up with an effective solution for doing this with focal and moderator variables as well.

        geom_hline(yintercept=c(max(data[,dv], na.rm = TRUE),min(data[,dv], na.rm = TRUE)),linetype=2) +

        facet_grid(~level) +
        scale_color_manual(values=df.cols$col) +
        #Define my fill reigion as lighter versions of the above
        scale_fill_manual(values=df.cols$colslight) +

        # annotate("segment", x = confint(values$m.repar)["C","2.5%"],
        #          xend = confint(values$m.repar)["C","97.5%"],
        #          y = 50,
        #          yend = 50,
        #          colour = "black", linetype = "solid", size = 3) +

        theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=15, hjust=0, color="black"))

      if(values$co <= max(df.staticplot$focal.seq) & values$co >= min(df.staticplot$focal.seq))
      {
        staticplot <- staticplot +
          annotate("point", x = values$co, y =
                     # as.numeric(mode(df.plot$pe)),
                     df.staticplot[which(round(df.staticplot$focal.seq,1)==round(values$co,1)),"pe"],
                   color= "black", fill = brewer.pal(9,"Greys")[2], size = 2, shape = 23)
      }

      # # if(summary(m.mean)$coefficients[paste0(foc,":",mod),"Pr(>|t|)"] > .05){
      #   staticplot <- staticplot + annotate("text", x = median(data[,input$foc], na.rm = TRUE), y = median(data[,input$dv]), label = "NOT SIGNIFICANT", size = 7)
      # # }


      values$plot<-staticplot; staticplot
  }
else{

    m.2low.pe<-m.2low$coefficients #specify coefficients
    m.2low.vcov<-vcov(m.2low) #specify variance-covariance matrix

    m.1low.pe<-m.1low$coefficients #specify coefficients
    m.1low.vcov<-vcov(m.1low) #specify variance-covariance matrix

    m.mean.pe<-m.mean$coefficients #specify coefficients
    m.mean.vcov<-vcov(m.mean) #specify variance-covariance matrix

    m.1high.pe<-m.1high$coefficients #specify coefficients
    m.1high.vcov<-vcov(m.1high) #specify variance-covariance matrix

    m.2high.pe<-m.2high$coefficients #specify coefficients
    m.2high.vcov<-vcov(m.2high) #specify variance-covariance matrix

  focal.seq <- seq(min(data[,foc], na.rm = TRUE),max(data[,foc], na.rm = TRUE),.1)
  focal.hyp <-cfMake(values$form2, data, nscen = length(focal.seq)) #make counterfactuals
  sims <- 10000
  form2<-values$form2
  focal.hyp$model <- values$model2
  simparam.2low <- mvrnorm(sims,m.2low.pe,m.2low.vcov) # draw parameters
  simparam.1low <- mvrnorm(sims,m.1low.pe,m.1low.vcov) # draw parameters
  simparam.mean <- mvrnorm(sims,m.mean.pe,m.mean.vcov) # draw parameters
  simparam.1high <- mvrnorm(sims,m.1high.pe,m.1high.vcov) # draw parameters
  simparam.2high <- mvrnorm(sims,m.2high.pe,m.2high.vcov) # draw parameters

  simbetas.2low <- simparam.2low[,1:length(m.2low.pe)]
  simbetas.1low <- simparam.1low[,1:length(m.1low.pe)]
  simbetas.mean <- simparam.mean[,1:length(m.mean.pe)]
  simbetas.1high <- simparam.1high[,1:length(m.1high.pe)]
  simbetas.2high <- simparam.2high[,1:length(m.2high.pe)]


  # 2 SD below plot

    for (j in 1:length(focal.seq)){
      for (l in 2:(length(all.vars(values$form2)))){
        focal.hyp <- cfChange(focal.hyp, all.vars(form2)[l], x=mean(m.2low$model[,l]), scen=j) #Hold covariates at their mean
        focal.hyp <- cfChange(focal.hyp, "s.mod", x=mean(data$s.mod, na.rm = TRUE), scen=j)#Based on user input, specify the value where my moderator will be held constant.
        focal.hyp <- cfChange(focal.hyp, foc, x=focal.seq[j], scen=j) #create scenarios for the jth counterfactual of my focal predictor.
      }
    }

    focal.hyp.Ysims.2low <- linearsimev(focal.hyp, simbetas.2low, ci=.95)

    # 1 SD below plot

    for (j in 1:length(focal.seq)){
      for (l in 2:(length(all.vars(values$form2)))){
        focal.hyp <- cfChange(focal.hyp, all.vars(values$form2)[l], x=mean(m.1low$model[,l]), scen=j) #Hold covariates at their mean
        focal.hyp <- cfChange(focal.hyp, "s.mod", x=mean(data$s.mod, na.rm = TRUE), scen=j)#Based on user input, specify the value where my moderator will be held constant.
        focal.hyp <- cfChange(focal.hyp, foc, x=focal.seq[j], scen=j) #create scenarios for the jth counterfactual of my focal predictor.
      }
    }

    focal.hyp.Ysims.1low <- linearsimev(focal.hyp, simbetas.1low, ci=.95)

    # MEAN plot

    for (j in 1:length(focal.seq)){
      for (l in 2:(length(all.vars(values$form2)))){
        focal.hyp <- cfChange(focal.hyp, all.vars(values$form2)[l], x=mean(m.mean$model[,l]), scen=j) #Hold covariates at their mean
        focal.hyp <- cfChange(focal.hyp, "s.mod", x=mean(data$s.mod, na.rm = TRUE), scen=j)#Based on user input, specify the value where my moderator will be held constant.
        focal.hyp <- cfChange(focal.hyp, foc, x=focal.seq[j], scen=j) #create scenarios for the jth counterfactual of my focal predictor.
      }
    }


    focal.hyp.Ysims.mean <- linearsimev(focal.hyp, simbetas.mean, ci=.95)

    # # 1 SD Above plot

    for (j in 1:length(focal.seq)){
      for (l in 2:(length(all.vars(values$form2)))){
        focal.hyp <- cfChange(focal.hyp, all.vars(values$form2)[l], x=mean(m.1high$model[,l]), scen=j) #Hold covariates at their mean
        focal.hyp <- cfChange(focal.hyp, "s.mod", x=mean(data$s.mod, na.rm = TRUE), scen=j)#Based on user input, specify the value where my moderator will be held constant.
        focal.hyp <- cfChange(focal.hyp, foc, x=focal.seq[j], scen=j) #create scenarios for the jth counterfactual of my focal predictor.
      }
    }


    focal.hyp.Ysims.1high <- linearsimev(focal.hyp, simbetas.1high, ci=.95)

    # # 2 SD Above plot

    for (j in 1:length(focal.seq)){
      for (l in 2:(length(all.vars(values$form2)))){
        focal.hyp <- cfChange(focal.hyp, all.vars(values$form2)[l], x=mean(m.2high$model[,l]), scen=j) #Hold covariates at their mean
        focal.hyp <- cfChange(focal.hyp, "s.mod", x=mean(data$s.mod, na.rm = TRUE), scen=j)#Based on user input, specify the value where my moderator will be held constant.
        focal.hyp <- cfChange(focal.hyp, foc, x=focal.seq[j], scen=j) #create scenarios for the jth counterfactual of my focal predictor.
      }
    }

    focal.hyp.Ysims.2high <- linearsimev(focal.hyp, simbetas.2high, ci=.95)


# Create Static Plots

#SET UP DATA FRAMES

  #Create static plots for non-two-part models

    df.plot.2low<-data.frame(
      focal.seq, #focal hypothetical values
      focal.hyp.Ysims.2low$pe, #point estimates
      focal.hyp.Ysims.2low$lower, #simulated lower limits
      focal.hyp.Ysims.2low$upper, #simulated upper limits
      rep(paste0("-2 SD\nBeta = ",round(lm.beta(m.2low)[foc],3),"\np = ",round(summary(m.2low)$coefficients[foc,"Pr(>|t|)"],3))),
      rep(summary(m.2low)$coefficients[foc,"Pr(>|t|)"],length(focal.seq)))
    #Add column names
    colnames(df.plot.2low)<-c("focal.seq","pe","lower","upper","level","p.val")
    df.plot.2low$col<-dfcol[which(round(dfcol$pvals,20)==round(summary(m.2low)$coefficients[foc,"Pr(>|t|)"]),20),"pcols"]

    df.plot.1low<-data.frame(
      focal.seq, #focal hypothetical values
      focal.hyp.Ysims.1low$pe, #point estimates
      focal.hyp.Ysims.1low$lower, #simulated lower limits
      focal.hyp.Ysims.1low$upper, #simulated upper limits
      rep(paste0("-1 SD\nBeta = ",round(lm.beta(m.1low)[foc],3),"\np = ",round(summary(m.1low)$coefficients[foc,"Pr(>|t|)"],3))),
      rep(summary(m.1low)$coefficients[foc,"Pr(>|t|)"],length(focal.seq)))
    #Add column names
    colnames(df.plot.1low)<-c("focal.seq","pe","lower","upper","level","p.val")
    df.plot.1low$col<-dfcol[which(round(dfcol$pvals,20)==round(summary(m.1low)$coefficients[foc,"Pr(>|t|)"]),20),"pcols"]

    df.plot.mean<-data.frame(
      focal.seq, #focal hypothetical values
      focal.hyp.Ysims.mean$pe, #point estimates
      focal.hyp.Ysims.mean$lower, #simulated lower limits
      focal.hyp.Ysims.mean$upper, #simulated upper limits
      rep(paste0("Mean\nBeta = ",round(lm.beta(m.mean)[foc],3),"\np = ",round(summary(m.mean)$coefficients[foc,"Pr(>|t|)"],3))),
      rep(summary(m.mean)$coefficients[foc,"Pr(>|t|)"],length(focal.seq)))
    #Add column names
    colnames(df.plot.mean)<-c("focal.seq","pe","lower","upper","level","p.val")
    df.plot.mean$col<-dfcol[which(round(dfcol$pvals,20)==round(summary(m.mean)$coefficients[foc,"Pr(>|t|)"]),20),"pcols"]

    df.plot.1high<-data.frame(
      focal.seq, #focal hypothetical values
      focal.hyp.Ysims.1high$pe, #point estimates
      focal.hyp.Ysims.1high$lower, #simulated lower limits
      focal.hyp.Ysims.1high$upper, #simulated upper limits
      rep(paste0("+1 SD\nBeta = ",round(lm.beta(m.1high)[foc],3),"\np = ",round(summary(m.1high)$coefficients[foc,"Pr(>|t|)"],3))),
      rep(summary(m.1high)$coefficients[foc,"Pr(>|t|)"],length(focal.seq)))
    #Add column names
    colnames(df.plot.1high)<-c("focal.seq","pe","lower","upper","level","p.val")
    df.plot.1high$col<-dfcol[which(round(dfcol$pvals,20)==round(summary(m.1high)$coefficients[foc,"Pr(>|t|)"]),20),"pcols"]

    df.plot.2high<-data.frame(
      focal.seq, #focal hypothetical values
      focal.hyp.Ysims.2high$pe, #point estimates
      focal.hyp.Ysims.2high$lower, #simulated lower limits
      focal.hyp.Ysims.2high$upper, #simulated upper limits
      rep(paste0("+2 SD\nBeta = ",round(lm.beta(m.2high)[foc],3),"\np = ",round(summary(m.2high)$coefficients[foc,"Pr(>|t|)"],3))),
      rep(summary(m.2high)$coefficients[foc,"Pr(>|t|)"],length(focal.seq)))
    #Add column names
    colnames(df.plot.2high)<-c("focal.seq","pe","lower","upper","level","p.val")
    df.plot.2high$col<-dfcol[which(round(dfcol$pvals,20)==round(summary(m.2high)$coefficients[foc,"Pr(>|t|)"]),20),"pcols"]

    df.staticplot<-as.data.frame(rbind(df.plot.2low,df.plot.1low,df.plot.mean,df.plot.1high,df.plot.2high))

    df.staticplot$col[df.staticplot$p.val>.10]<-bluespal["greater10"]
    df.staticplot$col[df.staticplot$p.val<=.10 & df.staticplot$p.val>.05]<-bluespal["less10"]
    df.staticplot$col[df.staticplot$p.val<=.05 & df.staticplot$p.val>.01]<-bluespal["less05"]
    df.staticplot$col[df.staticplot$p.val<=.01 & df.staticplot$p.val>.001]<-bluespal["less01"]
    df.staticplot$col[df.staticplot$p.val<=.001]<-bluespal["less001"]

    df.staticplot$greycol[df.staticplot$p.val>.10]<-greyspal["greater10"]
    df.staticplot$greycol[df.staticplot$p.val<=.10 & df.staticplot$p.val>.05]<-greyspal["less10"]
    df.staticplot$greycol[df.staticplot$p.val<=.05 & df.staticplot$p.val>.01]<-greyspal["less05"]
    df.staticplot$greycol[df.staticplot$p.val<=.01 & df.staticplot$p.val>.001]<-greyspal["less01"]
    df.staticplot$greycol[df.staticplot$p.val<=.001]<-greyspal["less001"]

    values$dfplot<-df.staticplot

    dfpoints$p.val[scale(data[,mod])<=-1.5]<-round(summary(m.2low)$coefficients[foc,"Pr(>|t|)"],3)
    dfpoints$p.val[scale(data[,mod])< -0.5 & scale(data[,mod])>=-1.5] <-round(summary(m.1low)$coefficients[foc,"Pr(>|t|)"],3)
    dfpoints$p.val[scale(data[,mod])<0.5 & scale(data[,mod])>=-0.5]<-round(summary(m.mean)$coefficients[foc,"Pr(>|t|)"],3)
    dfpoints$p.val[scale(data[,mod])<1.5 & scale(data[,mod])>=0.5]<-round(summary(m.1high)$coefficients[foc,"Pr(>|t|)"],3)
    dfpoints$p.val[scale(data[,mod])>=1.5]<-round(summary(m.2high)$coefficients[foc,"Pr(>|t|)"],3)

    dfpoints$level[scale(data[,mod])<=-1.5]<-paste0("-2 SD\nBeta = ",round(lm.beta(m.2low)[foc],3),"\np = ",round(summary(m.2low)$coefficients[foc,"Pr(>|t|)"],3))
    dfpoints$level[scale(data[,mod])< -0.5 & scale(data[,mod])>=-1.5]<-paste0("-1 SD\nBeta = ",round(lm.beta(m.1low)[foc],3),"\np = ",round(summary(m.1low)$coefficients[foc,"Pr(>|t|)"],3))
    dfpoints$level[scale(data[,mod])<0.5 & scale(data[,mod])>=-0.5]<-paste0("Mean\nBeta = ",round(lm.beta(m.mean)[foc],3),"\np = ",round(summary(m.mean)$coefficients[foc,"Pr(>|t|)"],3))
    dfpoints$level[scale(data[,mod])<1.5 & scale(data[,mod])>=0.5]<-paste0("+1 SD\nBeta = ",round(lm.beta(m.1high)[foc],3),"\np = ",round(summary(m.1high)$coefficients[foc,"Pr(>|t|)"],3))
    dfpoints$level[scale(data[,mod])>=1.5]<-paste0("+2 SD\nBeta = ",round(lm.beta(m.2high)[foc],3),"\np = ",round(summary(m.2high)$coefficients[foc,"Pr(>|t|)"],3))

    dfpoints<-na.omit(dfpoints)

    dfpoints$level <- factor(dfpoints$level, levels = c(paste0("-2 SD\nBeta = ",round(lm.beta(m.2low)[foc],3),"\np = ",round(summary(m.2low)$coefficients[foc,"Pr(>|t|)"],3)),
                                                        paste0("-1 SD\nBeta = ",round(lm.beta(m.1low)[foc],3),"\np = ",round(summary(m.1low)$coefficients[foc,"Pr(>|t|)"],3)),
                                                        paste0("Mean\nBeta = ",round(lm.beta(m.mean)[foc],3),"\np = ",round(summary(m.mean)$coefficients[foc,"Pr(>|t|)"],3)),
                                                        paste0("+1 SD\nBeta = ",round(lm.beta(m.1high)[foc],3),"\np = ",round(summary(m.1high)$coefficients[foc,"Pr(>|t|)"],3)),
                                                        paste0("+2 SD\nBeta = ",round(lm.beta(m.2high)[foc],3),"\np = ",round(summary(m.2high)$coefficients[foc,"Pr(>|t|)"],3))))

#This messes with my graphs to do, even though it works.
    # dfpoints$level<-gsub("p = 0", "p < .001",dfpoints$level)
    # df.staticplot$level<-gsub("p = 0", "p < .001",df.staticplot$level)
    #
    # dfpoints$resfoc<-lm(values$resfoc.form,m$model, na.action = na.omit)$fitted.values
    # dfpoints$yres<-lm(values$resdv.form,m$model, na.action = na.omit)$fitted.values

    values$dfpoints<-dfpoints

    df.colstemp<-df.staticplot[c("level","col","greycol")]
    df.cols<-df.colstemp[!duplicated(df.colstemp$level), ]
    df.cols$colslight<-sapply(df.cols$col, function(i) {df.cols$col<- lighten(i,pct=0.50)})
    df.cols$greycolslight<-sapply(df.cols$greycol, function(i) {df.cols$greycol<- lighten(i,pct=0.50)})
    df.cols<-as.data.frame(df.cols)
    values$df.cols<-df.cols
    #
    ####THIS IS THE CONTINOUS INTERACTION STATIC PLOT
    #TEMPORARY: get rid of +-2 sd
    # df.staticplot<-df.staticplot[which(df.staticplot$level!=paste0("+2 SD\nBeta = ",round(lm.beta(m.2high)[foc],3),"\np = ",round(summary(m.2high)$coefficients[foc,"Pr(>|t|)"],3))),]
    # df.staticplot<-df.staticplot[which(df.staticplot$level!=paste0("-2 SD\nBeta = ",round(lm.beta(m.2low)[foc],3),"\np = ",round(summary(m.2low)$coefficients[foc,"Pr(>|t|)"],3))),]
    # dfpoints<-dfpoints[which(dfpoints$level!=paste0("+2 SD\nBeta = ",round(lm.beta(m.2high)[foc],3),"\np = ",round(summary(m.2high)$coefficients[foc,"Pr(>|t|)"],3))),]
    # dfpoints<-dfpoints[which(dfpoints$level!=paste0("-2 SD\nBeta = ",round(lm.beta(m.2low)[foc],3),"\np = ",round(summary(m.2low)$coefficients[foc,"Pr(>|t|)"],3))),]
    #
    # df.cols<-df.cols[which(df.cols$level!=paste0("+2 SD\nBeta = ",round(lm.beta(m.2high)[foc],3),"\np = ",round(summary(m.2high)$coefficients[foc,"Pr(>|t|)"],3))),]
    # df.cols<-df.cols[which(df.cols$level!=paste0("-2 SD\nBeta = ",round(lm.beta(m.2low)[foc],3),"\np = ",round(summary(m.2low)$coefficients[foc,"Pr(>|t|)"],3))),]
    # values$df.cols<-df.cols

    staticplot<-ggplot() +

      geom_point(data=dfpoints, aes(x=pred,y=y),size = .75, alpha = .5) +
      geom_ribbon(data=df.staticplot, aes(x=focal.seq, y=pe, ymin = lower, ymax = upper, fill = level), alpha = .25) +
      geom_line(data = df.staticplot, aes(focal.seq, pe, fill=level, color=level), size=2) +

      ylim(min(data[,dv]-2*sd(data[,dv],na.rm = TRUE)),max(data[,dv]+2*sd(data[,dv],na.rm = TRUE))) +

      #Similarly, define my x axis based on the minimum and maximum values observed in my data
      xlim(min(data[,foc], na.rm = TRUE),max(data[,foc], na.rm = TRUE)) +

      #Manual specification of the RColorBrewer blue palette

      #thematic specifications of my graphic
      theme(text=element_text(family="Trebuchet MS",size=15, color="black"),
            legend.position="none",
            panel.background = element_blank(),
            legend.background = element_rect(fill = "white"),
            legend.title=element_blank(),
            legend.key = element_rect(fill = "white"),
            panel.grid.minor = element_blank(),
            axis.text.x=element_text(colour="black"),
            axis.text.y=element_text(colour="black"),
            axis.title.y=element_text(size=15),
            panel.grid.major = element_line(colour="#C0C0C0"),
            plot.background=element_rect(fill='white')) +
      labs(fill = mod, color = mod, linetype=mod) +

      #These are labels based on the current data, but I will build this out to be more dynamic in the future, allowing users to specify their own axis and plot labels.
      labs(x = foc, y = dv) +


      coord_cartesian() +

      #Across the title, I provide the point estimate and p-value for the focal predictor at the given level of the moderator. Note that there are some minor rounding issues I need to fix that result in p-values equalling zero (not possible).

      ggtitle(paste0("Level of Moderator (",mod,")")) +

      #I provide horizonal lines at the observed minimum and maximum of my outcome  variable. This helps the reader understand whether they are extropolating their prediction lines outside the range of observed data. Again, I will be building this out to come up with an effective solution for doing this with focal and moderator variables as well.

      geom_hline(yintercept=c(max(data[,dv], na.rm = TRUE),min(data[,dv], na.rm = TRUE)),linetype=2) +

      facet_grid(~level) +
      scale_color_manual(values=df.cols$col) +
      #Define my fill reigion as lighter versions of the above
      scale_fill_manual(values=df.cols$colslight) +

      # annotate("segment", x = confint(values$m.repar)["C","2.5%"],
      #                     xend = confint(values$m.repar)["C","97.5%"],
      #                     y = 50,
      #                     yend = 50,
      #                     colour = "black", linetype = "solid", size = 3) +

      theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=15, hjust=0, color="black"))

    if(values$co <= max(df.staticplot$focal.seq) & values$co >= min(df.staticplot$focal.seq))
    {
      staticplot <- staticplot +
        annotate("point", x = values$co, y =
                   # as.numeric(mode(df.plot$pe)),
                   df.staticplot[which(round(df.staticplot$focal.seq,1)==round(values$co,1)),"pe"],
                 color= "black", fill = brewer.pal(9,"Greys")[2], size = 2, shape = 23)
    }

    # # if(summary(m.mean)$coefficients[paste0(foc,":",mod),"Pr(>|t|)"] > .05){
    #   staticplot <- staticplot + annotate("text", x = median(data[,input$foc], na.rm = TRUE), y = median(data[,input$dv]), label = "NOT SIGNIFICANT", size = 7)
    # # }


    values$plot<-staticplot; staticplot
  #end of else statement for non-two-part static plots
}#end of else statement for defining static plots
}#End of else statement defining continuous moderator static plot
   })#signifies end of output$modplot!
  output$results <- renderTable({
    if (is.null(input$file1))
      return(NULL)

    m <- values$m

cbind(names(m$coefficients),round(summary(m)$coefficients,3))
  })

  output$resultstitle<-reactive({
    if (is.null(input$file1))
      return("")

    else (paste("Model Coefficients (Gaussian family)"))

  })

  output$resultscount <- renderTable({
    if (is.null(input$file1))
      return(NULL)

    m <- values$m

return(NULL)

  })

  output$resultstitlecount<-reactive({
    if (is.null(input$file1))
      return("")

    return("")

  })


    output$downloadPlot <- downloadHandler(
      filename = 'ModerationPlot.png',
      content = function(file) {
        ggsave(file,values$plot)
      })

    output$downloadPlot.final <- downloadHandler(
      filename = 'ModerationPlot.final.png',
      content = function(file) {
        ggsave(file,values$plotfinal)
      })

    output$downloadEst <- downloadHandler(
      filename = 'plot_estimates.csv',
      content = function(file) {
        write.csv(values$dfplot,file)
      })

    output$rosplot.final <- downloadHandler(
      filename = 'rosplot.png',
      content = function(file) {
        ggsave(file,values$rosplot)
      })



}) #end of eventReactive
output$datashow <- renderTable({
  if (is.null(input$file1))
    return(NULL)

  df()
})
output$plotdata <- renderTable({
  if (is.null(input$file1))
    return(NULL)

  # values$df.plot
  values$dfpoints
})

output$test.tab <- renderText({
  if (is.null(input$file1))
    return(NULL)
  paste(confint(values$m.repar)["C","2.5%"],confint(values$m.repar)["C","97.5%"])
  # values$df.plot

})

output$rostest <- renderTable({
  if (is.null(input$file1))
    return(NULL)

  # values$df.plot
  values$ros
})

output$rosplot <- renderPlot({
  if (is.null(input$file1))
    return(NULL)

  # values$df.plot
  values$rosplot
})

output$plot.final <- renderPlot({
  if (is.null(input$file1))
    return(NULL)

  # values$df.plot
  plotfinal<-values$plot +
    xlab(input$xaxislab) +
    ylab(input$yaxislab) +
    ggtitle(input$title)

  if(input$greyscale == TRUE){
    plotfinal<- plotfinal +
                scale_color_manual(values=values$df.cols$greycol) +
                scale_fill_manual(values=values$df.cols$greycolslight)
  }
  values$plotfinal<-plotfinal
  values$plotfinal
})
# output$co<- renderText({confint(values$m.repar)["C","2.5%"]})
  } #END SERVER CODE

