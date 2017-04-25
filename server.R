options(digits=9)
options(shiny.maxRequestSize=20*1024^2) #max file size = 20 MB
require(ggplot2)
# server.R

# HELPER FUNCTIONS --------------------------------------------------------

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

  if (input$manual==TRUE) {textInput("foc", label = "Specify your focal (x-axis) variable:", value = "", placeholder = "X")}
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

  if (input$manual==TRUE) {textInput("mod", label = "Specify your moderator (legend) variable:", value = "", placeholder = "Z")}
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

  textAreaInput("manform", label = "Formula", value = "", placeholder = "Y ~ X + Z + X*Z")
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
  actionButton("rosgo","Make RoS plot", width = "100%", icon = icon("fa fa-thumbs-up"))
})

output$ui.xlab.ros<-renderUI ({
  if (is.null(input$file1))
    return()
  
  textInput(inputId="xaxislab.ros",
            label="X-Axis label:",
            value=input$mod)
})

#Y axis label
output$ui.ylab.ros<-renderUI ({
  if (is.null(input$file1))
    return()
  
  textInput(inputId="yaxislab.ros",
            label="Y-Axis label:",
            value=paste("Simple Slope of ",input$foc))
})

observeEvent(input$go, { #Once the "go" button is hit, InterActive looks at all the ui input and runs the model.
  
  # Define small multiple moderator values
  
  output$ui.sm1<-renderUI ({
    if (is.null(input$file1))
      return()
    
    textInput(inputId="sm1",
              label="Multiple 1",
              value= -2,
              width = "80%")  
  })
  output$ui.sm2<-renderUI ({
    if (is.null(input$file1))
      return()
    
    textInput(inputId="sm2",
              label="Multiple 2",
              value= -1,
              width="80%")  
  })
  
  output$ui.sm3<-renderUI ({
    if (is.null(input$file1))
      return()
    
    textInput(inputId="sm3",
              label="Mulitple 3",
              value= 0,
              width="80%")  
  })
  output$ui.sm4<-renderUI ({
    if (is.null(input$file1))
      return()
    
    textInput(inputId="sm4",
              label="Multiple 4",
              value= 1,
              width="80%")  
  })
  
  output$ui.sm5<-renderUI ({
    if (is.null(input$file1))
      return()
    
    textInput(inputId="sm5",
              label="Multiple 5",
              value= 2,
              width="80%")  
  })
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

#DEFINE PLOT TEMPLATE FUNCTION
    
    interactiv.plot<-function(data, dfpoints, plotdf){
      geom_point(data=dfpoints, aes(x=pred,y=y),size = .75, alpha = .5) +
        geom_ribbon(data=plotdf, aes(x=focal.seq, y=pe, ymin = lower, ymax = upper, fill = level), alpha = .25) +
        geom_line(data = plotdf, aes(focal.seq, pe, fill=level, color=level), size=2) +
        
        ylim(min(data[,dv]-2*sd(data[,dv],na.rm = TRUE)),max(data[,dv]+2*sd(data[,dv],na.rm = TRUE))) +
        
        #Similarly, define my x axis based on the minimum and maximum values observed in my data
        xlim(min(data[,foc], na.rm = TRUE),max(data[,foc], na.rm = TRUE)) +
        
        #Manual specification of the RColorBrewer blue palette
        
        #thematic specifications of my graphic
        theme(text=element_text(family="Helvetica",size=12, color="black"),
              legend.position="none",
              panel.background = element_blank(),
              legend.background = element_rect(fill = "white"),
              legend.title=element_blank(),
              legend.key = element_rect(fill = "white"),
              panel.grid.minor = element_blank(),
              axis.text.x=element_text(colour="black"),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(colour="black"),
              axis.title.y=element_text(size=14),
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
        
        theme(plot.title = element_text(family="Helvetica", face="bold", size=14, hjust=0, color="black"))
    }
    
    interactiv.plot2<-function(data, dfpoints, plotdf){
      ggplot() +
        
        geom_point(data=dfpoints, aes(x=pred,y=y),size = .75, alpha = .5) +
        geom_ribbon(data=plotdf, aes(x=focal.seq, y=pe, ymin = lower, ymax = upper, fill = level), alpha = .25) +
        geom_line(data = plotdf, aes(focal.seq, pe, fill=level, color=level), size=2) +
        
        ylim(min(data[,dv]-2*sd(data[,dv],na.rm = TRUE)),max(data[,dv]+2*sd(data[,dv],na.rm = TRUE))) +
        
        #Similarly, define my x axis based on the minimum and maximum values observed in my data
        xlim(min(data[,foc], na.rm = TRUE),max(data[,foc], na.rm = TRUE)) +
        
        #Manual specification of the RColorBrewer blue palette
        
        #thematic specifications of my graphic
        theme(text=element_text(family="Helvetica",size=12, color="black"),
              legend.position="none",
              panel.background = element_blank(),
              legend.background = element_rect(fill = "white"),
              legend.title=element_blank(),
              legend.key = element_rect(fill = "white"),
              panel.grid.minor = element_blank(),
              axis.text.x=element_text(colour="black"),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(colour="black"),
              axis.title.y=element_text(size=14),
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
        
        theme(plot.title = element_text(family="Helvetica", face="bold", size=14, hjust=0, color="black"))
    }
    
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
        formtemp<-as.formula(gsub(mod, "s.mod", format(input$manform)))
        values$form2<-as.formula(gsub(foc, "s.foc", formtemp))

values$model2<-values$form2
        }
      else{
      cov<-paste(input$covars, collapse="+") #define covariate portion based on what the user specified
      if (input$poly == "quad"){ #if user selects a quadratic focal term, the below code will be run.
        focmod2<-paste("+",
                       "s.foc","+",
                       "I(","s.foc","^2)","+", #Key difference: specifies an additonal quadratic term here.
                       "s.mod","+",
                       "s.foc","*",
                       "s.mod", "+",
                       "I(","s.foc","^2)","*", #Also specifies a quadratic interaction.
                       "s.mod")
      }
      else{
        #here, I paste together a formula based on user inputs for a standard linear model.
        focmod2<-paste("+","s.foc","+","s.mod","+","s.foc","*","s.mod")
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

   }

# MAKING RoS PLOT ---------------------------------------------------------
observeEvent(input$rosgo, {
df.ros<-dftrue
df.ros[,foc] <- scale(df.ros[,foc])
df.ros[,mod] <- scale(df.ros[,mod])
# df.ros[,dv]<- scale(df.ros[,mod])

hyp.Z <- seq(-3,3,.05)

      ros <- sapply(hyp.Z, function(i) {
        df.ros[,mod] <- round(df.ros[,mod],2) - i
        mod <- lm(values$form,df.ros, na.action = na.omit)
        tmp <- c(i, extract(mod, foc))})

      ros <- data.frame(t(ros))
      colnames(ros) <- c("hyp.Z","pe.X","SE","LL","UL")
      ros$significance[(ros$LL*ros$UL) > 0] <- "sig"
      ros$significance[(ros$LL*ros$UL) <= 0] <- "not sig"
      ros$sign[ros$pe.X<0]<-"neg"
      ros$sign[ros$pe.X>=0]<-"pos"
      
      values$ros<-ros[c("hyp.Z","pe.X","LL","UL")]

#Creating the RoS plot
      rosplot <- ggplot() +
        geom_ribbon(data = ros, aes(hyp.Z, pe.X), ymin = ros$LL, ymax = ros$UL,
                    fill=brewer.pal(3,"Greys")[3], alpha = .25) +
        geom_line(data = ros, aes(hyp.Z, pe.X), color="Black", size = 1.25) +
        scale_x_continuous(breaks = c(-3,-2,-1,0,1,2,3),labels = c(-3,-2,-1,0,1,2,3),limits = c(-3,3)) +
        ylim(min(ros$LL),max(ros$UL)) +
        geom_hline(yintercept = 0) +
        xlab(input$xaxislab.ros) +
        ylab(input$yaxislab.ros) +
        geom_rug(data = df.ros, aes(x=df.ros[,mod])) +
        # geom_density(data = df.ros, aes(x=df.ros[,mod]), linetype = "dotted", fill=brewer.pal(3,"Greys")[2], alpha = .25, position = position_dodge(width = .2)) +
        theme_bw() +
        theme(text=element_text(family="Helvetica",size=12, color="black"))

if(m$coefficients[paste0(foc,":",mod)]<0){
if("not sig"%in%ros[which(ros$sign=="neg"),"significance"]==TRUE &
   "sig"%in%ros[which(ros$sign=="neg"),"significance"]==TRUE ){  
      rosplot <- rosplot + 
        geom_vline(xintercept = min(ros[which(ros$significance=="sig" & ros$sign=="neg"),"hyp.Z"]), linetype="dashed")
}
      
if("not sig"%in%ros[which(ros$sign=="pos"),"significance"]==TRUE &
         "sig"%in%ros[which(ros$sign=="pos"),"significance"]==TRUE){  
        rosplot <- rosplot + 
          geom_vline(xintercept = max(ros[which(ros$significance=="sig" & ros$sign=="pos"),"hyp.Z"]), linetype="dashed")
}
}
else{
  if("not sig"%in%ros[which(ros$sign=="neg"),"significance"]==TRUE &
     "sig"%in%ros[which(ros$sign=="neg"),"significance"]==TRUE ){  
    rosplot <- rosplot + 
      geom_vline(xintercept = max(ros[which(ros$significance=="sig" & ros$sign=="neg"),"hyp.Z"]), linetype="dashed")
  }
  
  if("not sig"%in%ros[which(ros$sign=="pos"),"significance"]==TRUE &
     "sig"%in%ros[which(ros$sign=="pos"),"significance"]==TRUE){  
    rosplot <- rosplot + 
      geom_vline(xintercept = min(ros[which(ros$significance=="sig" & ros$sign=="pos"),"hyp.Z"]), linetype="dashed")
  }
}
      

values$rosplot<-rosplot

})

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
        focal.seq <- c(0,seq(min(data[,foc], na.rm = TRUE),max(data[,foc], na.rm = TRUE),.1))
        df.plot<-as.data.frame(matrix(nrow = length(focal.seq),ncol=3))
        for (j in 1:length(focal.seq)){
          data$s.foc<-data[,foc]-sd(data[,foc], na.rm=TRUE)*focal.seq[j]
          data$s.mod<-data[,mod]-input$modlevel
          lm<-lm(values$form2,data, na.action = na.omit)
          df.plot$pe[j]<-lm$coefficients["(Intercept)"]
          df.plot$lower[j]<-confint(lm)["(Intercept)","2.5 %"]
          df.plot$upper[j]<-confint(lm)["(Intercept)","97.5 %"]
          df.plot$pe[j]<-lm$coefficients["(Intercept)"]
          df.plot$p.val<-rep(summary(lm)$coefficients["s.foc","Pr(>|t|)"],length(focal.seq))
          df.plot$focal.seq<-focal.seq
        }

    #This looks up the color corresponding to the p-value associated with the PE of my focal predictor, and stores it in the dataframe for use in ggplot.
    df.plot$col<-dfcol[which(round(dfcol$pvals,20)==round(summary(m)$coefficients[foc,"Pr(>|t|)"]),20),"pcols"]

    #Finally, I build the graphic using ggplot2, commented below:
    plot.lin <- interactiv.plot(data=data,dfpoints=dfpoints,plotdf=df.plot)

    if(values$co <= max(df.plot$focal.seq) & values$co >= min(df.plot$focal.seq))
       {
      plot.lin <- plot.lin +
        annotate("point", x = values$co, y =
                 # as.numeric(mode(df.plot$pe)),
                   (m$coefficients["(Intercept)"] + values$co*m$coefficients[foc]),
               color= "black", fill = "orange", size = 3, shape = 23)
    }

    values$df.plot<-df.plot
plot.lin
      }

#Below is similar code, applied instead to creating a static graphic using the principles of small multiples.
#This is similar to the pick-a-point approach of probing and graphing interactions, though offers graphics across a greater range than 3 points (now we do 5).

else{

# Define Static Plots ------------------------------------------------------
  if(input$cat==TRUE){

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

      staticplot<-interactiv.plot2(data=data,dfpoints=dfpoints,plotdf = df.staticplot)

      if(values$co <= max(df.staticplot$focal.seq) & values$co >= min(df.staticplot$focal.seq))
      {
        staticplot <- staticplot +
          annotate("point", x = values$co, y =
                     # as.numeric(mode(df.plot$pe)),
                     (m$coefficients["(Intercept)"] + values$co*m$coefficients[foc]),
                   color= "black", fill = brewer.pal(9,"Greys")[2], size = 2, shape = 23)
      }

      values$plot<-staticplot; staticplot
  }
else{

# Create Static Plots

#SET UP DATA FRAMES

  #Create static plots for non-two-part models

  focal.seq <- c(0,seq(min(data[,foc], na.rm = TRUE),max(data[,foc], na.rm = TRUE),.1))
  mod.seq <- c(as.numeric(input$sm1),
               as.numeric(input$sm2),
               as.numeric(input$sm3),
               as.numeric(input$sm4),
               as.numeric(input$sm5))
  modlevel<- list()
  ppoints<-as.data.frame(matrix(nrow=length(focal.seq),ncol=3))
  colnames(ppoints)<-c("pe","lower","upper")
  for (k in 1:length(mod.seq)){

    data$s.mod<-data[,mod]-sd(data[,mod], na.rm=TRUE)*mod.seq[k]
    for (j in 1:length(focal.seq)){
      data$s.foc<-data[,foc]-sd(data[,foc], na.rm=TRUE)*focal.seq[j]
      
      lm<-lm(values$form2,data, na.action = na.omit)
      ppoints$pe[j]<-lm$coefficients["(Intercept)"]
      ppoints$lower[j]<-confint(lm)["(Intercept)","2.5 %"]
      ppoints$upper[j]<-confint(lm)["(Intercept)","97.5 %"]
      ppoints$pe[j]<-lm$coefficients["(Intercept)"]
      ppoints$p.val<-rep(summary(lm)$coefficients["s.foc","Pr(>|t|)"],length(focal.seq))
      ppoints$focal.seq<-focal.seq
      leveltemp<-rep(paste0("b = ",
                                round(lm$coefficients["s.foc"],2),"\n95% CI =\n[",
                                round(confint(lm)["s.foc","2.5 %"],2),", ",
                                round(confint(lm)["s.foc","97.5 %"],2),"]"))
    }
    ppoints$level<-paste0(mod.seq[k]," SD\n",leveltemp)
    modlevel[[k]]<-ppoints
  }
  data$s.mod<-data[,mod]
  data$s.foc<-data[,foc]
  
    df.staticplot<-as.data.frame(rbind(modlevel[[1]],
                                       modlevel[[2]],
                                       modlevel[[3]],
                                       modlevel[[4]],
                                       modlevel[[5]]))

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

    divides<-c(((as.numeric(input$sm2) - as.numeric(input$sm1))/2),
               ((as.numeric(input$sm3) - as.numeric(input$sm2))/2),
               ((as.numeric(input$sm4) - as.numeric(input$sm3))/2),
               ((as.numeric(input$sm5) - as.numeric(input$sm4))/2))
    divides<-abs(divides)
               
    values$divides<-divides
    dfpoints$p.val[(rank(data[,mod])/length(data[,mod]))<ecdf(scale(data[,mod]))(as.numeric(input$sm1)+divides[1])]<-round(modlevel[[1]]$p.val[1],3)
    dfpoints$p.val[(rank(data[,mod])/length(data[,mod]))<ecdf(scale(data[,mod]))(as.numeric(input$sm2)+divides[2]) & (rank(data[,mod])/length(data[,mod]))>=ecdf(scale(data[,mod]))(as.numeric(input$sm1)+divides[1])] <-round(modlevel[[2]]$p.val[1],3)
    dfpoints$p.val[(rank(data[,mod])/length(data[,mod]))<ecdf(scale(data[,mod]))(as.numeric(input$sm3)+divides[3]) & (rank(data[,mod])/length(data[,mod]))>=ecdf(scale(data[,mod]))(as.numeric(input$sm2)+divides[2])]<-round(modlevel[[3]]$p.val[1],3)
    dfpoints$p.val[(rank(data[,mod])/length(data[,mod]))<ecdf(scale(data[,mod]))(as.numeric(input$sm4)+divides[4]) & (rank(data[,mod])/length(data[,mod]))>=ecdf(scale(data[,mod]))(as.numeric(input$sm3)+divides[3])]<-round(modlevel[[4]]$p.val[1],3)
    dfpoints$p.val[(rank(data[,mod])/length(data[,mod]))>ecdf(scale(data[,mod]))(as.numeric(input$sm4)+divides[4])]<-round(modlevel[[5]]$p.val[1],3)

    #SM1
    dfpoints$level[(rank(data[,mod])/length(data[,mod]))<ecdf(scale(data[,mod]))(as.numeric(input$sm1)+divides[1])]<-modlevel[[1]]$level[1]
    #get percentile rank
    #SM2
    dfpoints$level[(rank(data[,mod])/length(data[,mod]))<ecdf(scale(data[,mod]))(as.numeric(input$sm2)+divides[2]) & (rank(data[,mod])/length(data[,mod]))>=ecdf(scale(data[,mod]))(as.numeric(input$sm1)+divides[1])]<-modlevel[[2]]$level[1]
    dfpoints$level[(rank(data[,mod])/length(data[,mod]))<ecdf(scale(data[,mod]))(as.numeric(input$sm3)+divides[3]) & (rank(data[,mod])/length(data[,mod]))>=ecdf(scale(data[,mod]))(as.numeric(input$sm2)+divides[2])]<-modlevel[[3]]$level[1]
    dfpoints$level[(rank(data[,mod])/length(data[,mod]))<ecdf(scale(data[,mod]))(as.numeric(input$sm4)+divides[4]) & (rank(data[,mod])/length(data[,mod]))>=ecdf(scale(data[,mod]))(as.numeric(input$sm3)+divides[3])]<-modlevel[[4]]$level[1]
    dfpoints$level[(rank(data[,mod])/length(data[,mod]))>ecdf(scale(data[,mod]))(as.numeric(input$sm4)+divides[4])]<-modlevel[[5]]$level[1]
    
    dfpoints<-na.omit(dfpoints)

    dfpoints$level <- factor(dfpoints$level, levels = c(modlevel[[1]]$level[1],
                                                        modlevel[[2]]$level[1],
                                                        modlevel[[3]]$level[1],
                                                        modlevel[[4]]$level[1],
                                                        modlevel[[5]]$level[1]))
    
    df.staticplot$level <- factor(df.staticplot$level, levels = c(modlevel[[1]]$level[1],
                                                        modlevel[[2]]$level[1],
                                                        modlevel[[3]]$level[1],
                                                        modlevel[[4]]$level[1],
                                                        modlevel[[5]]$level[1]))
    

    values$dfpoints<-dfpoints

    df.colstemp<-df.staticplot[c("level","col","greycol")]
    df.cols<-df.colstemp[!duplicated(df.colstemp$level), ]
    df.cols$colslight<-sapply(df.cols$col, function(i) {df.cols$col<- lighten(i,pct=0.50)})
    df.cols$greycolslight<-sapply(df.cols$greycol, function(i) {df.cols$greycol<- lighten(i,pct=0.50)})
    df.cols<-as.data.frame(df.cols)
    values$df.cols<-df.cols
    #

    staticplot<-interactiv.plot2(data=data,dfpoints=dfpoints,plotdf = df.staticplot)

    if(values$co <= max(df.staticplot$focal.seq) & values$co >= min(df.staticplot$focal.seq))
    {
      staticplot <- staticplot +
        annotate("point", x = values$co, y =
                   # as.numeric(mode(df.plot$pe)),
                   (m$coefficients["(Intercept)"] + values$co*m$coefficients[foc]),
                 color= "black", fill = brewer.pal(9,"Greys")[2], size = 2, shape = 23)
    }

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
        ggsave(file,values$plot, width = 7, height = 4, units = "in")
      })

    output$downloadPlot.final <- downloadHandler(
      filename = 'ModerationPlot.final.png',
      content = function(file) {
        ggsave(file,values$plotfinal, width = 7, height = 4, units = "in")
      })

    output$downloadEst <- downloadHandler(
      filename = 'plot_estimates.csv',
      content = function(file) {
        write.csv(values$dfplot,file)
      })

    output$rosplot.final <- downloadHandler(
      filename = 'rosplot.png',
      content = function(file) {
        ggsave(file,values$plotfinal.ros,width = 7, height = 4, units = "in")
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

  values$dfplot
})

output$test.tab <- renderText({
  if (is.null(input$file1))
    return(NULL)
  # paste(confint(values$m.repar)["C","2.5%"],confint(values$m.repar)["C","97.5%"])
  values$divides
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
output$plot.final.ros <- renderPlot({
  if (is.null(values$rosplot))
    return(NULL)
  plotfinal.ros<-values$rosplot +
    xlab(input$xaxislab.ros) +
    ylab(input$yaxislab.ros)
  values$plotfinal.ros<-plotfinal.ros
  plotfinal.ros
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

