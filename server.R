#Server

options(digits=9)
options(shiny.maxRequestSize=20*1024^2) #max file size = 20 MB
require(ggplot2)
require(RColorBrewer)
# server.R

# HELPER FUNCTIONS --------------------------------------------------------

#lighten() coverts a color to 75% transparency. This is used for creating shaded confidence regions.
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

#extract() is used later in extracting coefficients for regions-of-signifiance analysis.
extract <- function(MOD, regressor) {
  tmp = numeric()
  tmp[1] <- MOD$coef[regressor] #run it, and put the coefficient in cell [i,2] of a table
  tmp[2] <- summary(MOD)$coef[regressor,2] #run it, and put the standard error in cell [i,3] of a table
  tmp[3] <- confint(MOD)[regressor,1] #run it, and put the lower limit of my 95% CI in cell [i,4] of a table
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

bluespal<-c("#08306B","#2171B5","#4292C6","#9ECAE1","#C6DBEF")
greyspal<-c("#000000","#252525","#525252","#737373","#969696")
names(bluespal)<-c("less001","less01","less05","less10","greater10")
names(greyspal)<-c("less001","less01","less05","less10","greater10")

# END HELPER FUNCTIONS ----------------------------------------------------

  function(input, output, session) {
    values<-reactiveValues() #defines empty list that will take model-produced values
    output$msg<-renderText({
    if (is.null(input$file1)){
      "Please upload your dataset to begin."
    } else(values$msg<-NULL)
    })

# DATAFRAME UPLOAD CODE ---------------------------------------------------

    df<-reactive({
      if (!is.null(input$file1)){
        if(tools::file_ext(input$file1) == "csv"){
          read.csv(input$file1$datapath, header = TRUE)
        }
        else if (tools::file_ext(input$file1) == "xlsx"){
          file.rename(input$file1$datapath,
                      paste(input$file1$datapath, ".xlsx", sep=""))
          as.data.frame(readxl::read_excel(paste(input$file1$datapath, ".xlsx", sep=""), 1,col_names = TRUE))
        }
        else if (tools::file_ext(input$file1) == "xls"){
          file.rename(input$file1$datapath,
                      paste(input$file1$datapath, ".xls", sep=""))
          as.data.frame(readxl::read_xls(paste(input$file1$datapath, ".xls", sep=""), 1,col_names = TRUE))
        }
        else if (tools::file_ext(input$file1) == "sav"){
          foreign::read.spss(input$file1$datapath, to.data.frame=TRUE)
        }
      }
    })

# User Interface (UI) code ------------------------------------------------

#Categorical moderator option
output$ui.categorical <- renderUI ({
      if (is.null(input$file1))
        return()
      checkboxInput(inputId="cat", label = h5("Categorical Moderator"),
                    value=FALSE)
    })

# Continuous Variable Scaling Options -----------------------------------------------

#Scaling for the X predictor
output$ui.scalex <- renderUI ({
  if (is.null(input$file1))
    return()
  radioButtons(inputId="scalex", label = NULL, inline = TRUE,
               c("Raw X" = "raw", "Centered X" = "cen", "Standardized X" = "std"),
               selected = "cen")
})
    
#Scaling for the Z predictor
    output$ui.scalez <- renderUI ({
      if (is.null(input$file1))
        return()
      radioButtons(inputId="scalez", label = NULL, inline = TRUE,
                   c("Raw Z" = "raw", "Centered Z" = "cen", "Standardized Z" = "std"),
                   selected = "cen")
    })

#Scaling for the dependent variable
    output$ui.scaledv <- renderUI ({
      if (is.null(input$file1))
        return()
      radioButtons(inputId="scaledv", label = NULL, inline = TRUE,
                   c("Raw Y" = "raw", "Centered Y" = "cen", "Standardized Y" = "std"),
                   selected = "raw")
    })
    
#Model Selection Options ------------------------------------------

#Specify covariates for the model

output$ui.covars <- renderUI ({
  if (is.null(input$file1))
    return()

  selectInput(inputId = "covars",
              label = "Choose your covariate(s)\n(control variables):",
              choices = colnames(df()),
              selected = "",
              multiple = TRUE,
              selectize = TRUE)
})

#Specify focal variable (x-axis)

output$ui.foc <- renderUI ({
  if (is.null(input$file1))
    return()

  selectInput("foc",
              label = "Choose your focal (x-axis) variable:",
              choices = colnames(df()),
              selected = "")
})

# Specify moderator variable

output$ui.mod <- renderUI ({
  if (is.null(input$file1))
    return()

  selectInput("mod",
              label = "Choose your moderator (legend) variable:",
              choices = colnames(df()),
              selected = "")
})

#Specify outcome variable (appearing on y-axis)
output$ui.dv <- renderUI ({
  if (is.null(input$file1))
    return()

  selectInput("dv",
              label = "Choose your dependent (y-axis) variable:",
              choices = colnames(df()),
              selected = "")
})

# specifying polynomial terms (currently accomodates quadratic effects only)
output$ui.poly <- renderUI ({
  if (is.null(input$file1))
    return()

radioButtons(inputId="poly", label = h4("Form of Focal Predictor:"),
             c("Linear" = "lin", "Quadratic" = "quad"),
             selected = "lin")
})


# Defining customization UI options for small multiples plot ---------------------

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

#Define "run analysis" button
output$ui.go <- renderUI ({
  if (is.null(input$file1))
    return()
actionButton("go","Run Analysis", width = "100%", icon = icon("fa fa-thumbs-up"))
})


# Defining customization UI options for regions of significance plot --------------

#X axis label
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

#Plot title
output$ui.title.ros<-renderUI ({
  if (is.null(input$file1))
    return()
  
  textInput(inputId="rostitle",
            label="Title of Graph:",
            value="")
})

#Produce a table of data descriptives
output$varnames <- renderTable({
  if (is.null(input$file1))
    return()
  if (is.null(input$go))
    return()
  desc<- as.data.frame(psych::describe(df()))[,c("mean","sd","min","max","skew")]
  varlist<-cbind(colnames(df()),desc)
  colnames(varlist)<-c("Variable",colnames(desc))
  varlist
})

observeEvent(input$go, { #Once the "go" button is hit, InterActive looks at all the ui input and runs the model.
  updateTabsetPanel(session, "inTabset",
                    selected = "Plot")
  
  # Define small multiple moderator values (from left to right)
  
#Small multiple 1
  output$ui.sm1<-renderUI ({
    if (is.null(input$file1))
      return()
    if (input$cat==TRUE) return()
    
    textInput(inputId="sm1",
              label="Multiple 1",
              value= -2,
              width = "80%")  
  })

#Small multiple 2
  output$ui.sm2<-renderUI ({
    if (is.null(input$file1))
      return()
    if (input$cat==TRUE) return()
    textInput(inputId="sm2",
              label="Multiple 2",
              value= -1,
              width="80%")  
  })
  
#Small multiple 3
  output$ui.sm3<-renderUI ({
    if (is.null(input$file1))
      return()
    if (input$cat==TRUE) return()
    
    textInput(inputId="sm3",
              label="Mulitple 3",
              value= 0,
              width="80%")  
  })
  
#Small multiple 4
  output$ui.sm4<-renderUI ({
    if (is.null(input$file1))
      return()
    if (input$cat==TRUE) return()
    textInput(inputId="sm4",
              label="Multiple 4",
              value= 1,
              width="80%")  
  })

#Small multiple 5  
  output$ui.sm5<-renderUI ({
    if (is.null(input$file1))
      return()
    if (input$cat==TRUE) return()
    
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

#Define small multiples for binary categorical moderator
  output$ui.catsm1<-renderUI ({
    if (is.null(input$file1))
      return()
    if (input$cat==FALSE)
      return()
    mod <- input$mod
    mod.seq<-na.omit(unique(df()[,mod]))
    textInput(inputId="catsm1",
              label="Category 0",
              value= paste("Category ",mod.seq[1]),
              width = "50%")  
  })
  
  output$ui.catsm2<-renderUI ({
    if (is.null(input$file1))
      return()
    if (input$cat==FALSE)
      return()
    mod <- input$mod
    mod.seq<-na.omit(unique(df()[,mod]))
    textInput(inputId="catsm2",
              label="Category 1",
              value= paste("Category ",mod.seq[2]),
              width = "50%")  
  })
  
# Simple slopes analysis and plot creation code -----------------------------------
  
  output$modplot <- renderPlot({
      if (is.null(input$file1))
        return(NULL)

#Defining plot template functions
 
    #for continuous moderator variables   
    interactiv.plot<-function(data, dfpoints, plotdf){
      geom_point(data=dfpoints, aes(x=pred,y=y),size = .75, alpha = .5) +
        geom_ribbon(data=plotdf, aes(x=focal.seq, y=pe, ymin = lower, ymax = upper, fill = level), alpha = .25) +
        geom_line(data = plotdf, aes(focal.seq, pe, fill=level, color=level), size=1) +
        
        ylim(min(data[,dv]-2*sd(data[,dv],na.rm = TRUE)),max(data[,dv]+2*sd(data[,dv],na.rm = TRUE))) +
        
        xlim(min(data[,foc], na.rm = TRUE),max(data[,foc], na.rm = TRUE)) +
        
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

        labs(x = foc, y = dv) +
        
        coord_cartesian() +
        
        ggtitle(paste0("Level of Moderator (",mod,")")) +
        
        geom_hline(yintercept=c(max(data[,dv], na.rm = TRUE),min(data[,dv], na.rm = TRUE)),linetype=2) +
        
        facet_grid(~level) +
        scale_color_manual(values=df.cols$col) +
        scale_fill_manual(values=df.cols$colslight) +
        
        theme(plot.title = element_text(family="Helvetica", face="bold", size=14, hjust=0, color="black"))
    }
    
#For categorical moderator variables
    interactiv.plot2<-function(data, dfpoints, plotdf){
      ggplot() +
        
        geom_point(data=dfpoints, aes(x=pred,y=y),size = .75, alpha = .5) +
        geom_ribbon(data=plotdf, aes(x=focal.seq, y=pe, ymin = lower, ymax = upper, fill = level), alpha = .25) +
        geom_line(data = plotdf, aes(focal.seq, pe, fill=level, color=level), size=1) +
        
        ylim(min(data[,dv]-2*sd(data[,dv],na.rm = TRUE)),max(data[,dv]+2*sd(data[,dv],na.rm = TRUE))) +
        
        xlim(min(data[,foc], na.rm = TRUE),max(data[,foc], na.rm = TRUE)) +

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
        
        labs(x = foc, y = dv) +
        
        coord_cartesian() +
        
        ggtitle(paste0("Level of Moderator (",mod,")")) +
        
        #I provide horizonal lines at the observed minimum and maximum of my outcome  variable. This helps the reader understand whether they are extropolating their prediction lines outside the range of observed data. Again, I will be building this out to come up with an effective solution for doing this with focal and moderator variables as well.
        
        geom_hline(yintercept=c(max(data[,dv], na.rm = TRUE),min(data[,dv], na.rm = TRUE)),linetype=2) +
        
        facet_grid(~level) +
        scale_color_manual(values=df.cols$col) +
        scale_fill_manual(values=df.cols$colslight) +
        
        theme(plot.title = element_text(family="Helvetica", face="bold", size=14, hjust=0, color="black"))
    }
    
      data<-df() #make a copy of the data for variable rescaling and analyses
      dftrue<-df() #retain an original copy
      
      mod <- input$mod #define moderator
      foc <- input$foc #define focal predictor
      dv <- input$dv #define outcome variable

#Re-scale variables based on user input
      if (input$scalex == "cen"){
        data[,foc] <- scale(data[,foc],  scale = FALSE, center = TRUE)
        dftrue[,foc] <- scale(dftrue[,foc],  scale = FALSE, center = TRUE)
      }
      if (input$scalez == "cen" && input$cat==FALSE){
        data[,mod] <- scale(data[,mod],  scale = FALSE, center = TRUE)
        dftrue[,mod] <- scale(dftrue[,mod],  scale = FALSE, center = TRUE)
      }
        
      if (input$scaledv == "cen"){
        data[,dv] <- scale(data[,dv],  scale = FALSE, center = TRUE)
        dftrue[,dv] <- scale(dftrue[,dv],  scale = FALSE, center = TRUE)
      }
      
      if (input$scalex == "std"){
        data[,foc] <- scale(data[,foc],  scale = TRUE)
        dftrue[,foc] <- scale(dftrue[,foc],  scale = TRUE)
      }
      if (input$scalez == "std" && input$cat==FALSE){
        data[,mod] <- scale(data[,mod],  scale = TRUE)
        dftrue[,mod] <- scale(dftrue[,mod],  scale = TRUE)
      }
      if (input$scaledv == "std"){
        data[,dv] <- scale(data[,dv],  scale = TRUE)
        dftrue[,dv] <- scale(dftrue[,dv],  scale = TRUE)
      }

      values$modlevel<-input$modlevel
      if(input$cat == FALSE) (values$modlevel<- mean(data[,mod], na.rm = TRUE))
     
       modlevel <- values$modlevel
      
      if(input$cat==FALSE){data[,mod]<-data[,mod]-values$modlevel}
      
      cov<-paste(input$covars, collapse=" + ")
      
      if (input$poly == "quad"){
        focmod<-paste("+",
                      foc,"+",
                      "I(",foc,"^2)","+",
                      mod,"+",
                      foc,"*",
                      mod, "+",
                      "I(",foc,"^2)","*",
                      mod)
      }
      else{
        focmod<-paste("+",foc,"+",mod,"+",foc,"*",mod)}

      dep<-paste(input$dv, "~")
      
      values$form<-as.formula(paste(dep,cov,focmod))
      
      values$m<-lm(values$form,data, na.action = na.omit)
      values$mtrue<-lm(values$form,dftrue, na.action = na.omit)
      
      values$model<-as.formula(paste(dep,cov,focmod))
      
      m <- values$m
      mtrue <- values$mtrue
      
      data[,mod] <- as.numeric(data[,mod])
      dftrue[,mod] <- as.numeric(dftrue[,mod])
      
# Computing the crossover point
      
      values$co<- -(m$coefficients[mod]/(m$coefficients[paste0(foc,":",mod)]))
      if (input$poly == "quad")values$co<- -9999

      cov<-paste(input$covars, collapse="+")
      if (input$poly == "quad"){
        focmod2<-paste("+",
                       "s.foc","+",
                       "I(","s.foc","^2)","+",
                       "s.mod","+",
                       "s.foc","*",
                       "s.mod", "+",
                       "I(","s.foc","^2)","*",
                       "s.mod")
      }
      else{
        focmod2<-paste("+","s.foc","+","s.mod","+","s.foc","*","s.mod")
      }
      dep2<-paste(input$dv, "~")
      
      if(input$cat==TRUE){
        
        values$form2<-as.formula(paste(dep2,cov,focmod2))
        values$model2<-as.formula(paste(dep2,cov,focmod2))
        
        data$s.foc<-data[,foc]
        data$s.mod <- data[,mod]-min(unique(data[,mod]),na.rm=TRUE)
        values$m.cat0<-lm(values$form2,data, na.action = na.omit)
        data$s.mod <- data[,mod]-max(unique(data[,mod]),na.rm=TRUE)
        m.cat1<-lm(values$form2,data, na.action = na.omit)
        
      }
      else{
        
        values$form2<-as.formula(paste(dep2,cov,focmod2))
        values$model2<-as.formula(paste(dep2,cov,focmod2))
        
      }
      
      # MAKING RoS PLOT ---------------------------------------------------------
        if(input$cat == FALSE){
          df.ros<-dftrue
          df.ros[,foc] <- scale(df.ros[,foc])
          df.ros[,mod] <- scale(df.ros[,mod])
          
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
        }
      
      dfpoints<-as.data.frame(cbind(data[,foc],glm(values$form,data, na.action = na.omit,family="gaussian")$y,m$fitted.values))
      colnames(dfpoints)<-c("pred","y","fitted")
      

# Creating plot estimates -------------------------------------------------

      if(input$cat==TRUE){
        
        focal.seq <- c(0,seq(min(data[,foc], na.rm = TRUE),max(data[,foc], na.rm = TRUE),.1))
        mod.seq <- na.omit(unique(data[,mod]))
        
        modlevel<- list()
        ppoints<-as.data.frame(matrix(nrow=length(focal.seq),ncol=3))
        colnames(ppoints)<-c("pe","lower","upper")
        for (k in 1:length(mod.seq)){
          
          if(input$cat==FALSE)(data$s.mod<-data[,mod]-sd(data[,mod], na.rm=TRUE)*mod.seq[k])
          if(input$cat==TRUE)(data$s.mod<-values$s.mod<-data[,mod]-mod.seq[k])
          for (j in 1:length(focal.seq)){
            data$s.foc <- data[,foc]-sd(data[,foc], na.rm=TRUE)*focal.seq[j]
            
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
          if(input$cat==FALSE)(ppoints$level<-paste0(mod.seq[k]," SD\n",leveltemp))
          if(input$cat==TRUE)(ppoints$level<-paste0("Category ",mod.seq[k],"\n",leveltemp))
          modlevel[[k]]<-values$ppoints<-ppoints
        }
        data$s.mod<-data[,mod]
        data$s.foc<-data[,foc]
        
        if(input$cat==FALSE){
          df.staticplot<-as.data.frame(rbind(modlevel[[1]],
                                             modlevel[[2]],
                                             modlevel[[3]],
                                             modlevel[[4]],
                                             modlevel[[5]]))
        }
        if(input$cat==TRUE){

          df.staticplot<-as.data.frame(rbind(modlevel[[1]],
                                             modlevel[[2]]))
        }
        
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
        
# Create Static Plots
    
        values$dfpoints<-dfpoints
        
        df.colstemp<-df.staticplot[c("level","col","greycol")]
        df.cols<-df.colstemp[!duplicated(df.colstemp$level), ]
        df.cols$colslight<-sapply(df.cols$col, function(i) {df.cols$col<- lighten(i,pct=0.50)})
        df.cols$greycolslight<-sapply(df.cols$greycol, function(i) {df.cols$greycol<- lighten(i,pct=0.50)})
        df.cols<-as.data.frame(df.cols)
        values$df.cols<-df.cols
        values$data<-data
        values$df.staticplot<-df.staticplot
        
        staticplot<-interactiv.plot2(data=data,dfpoints=dfpoints,plotdf = df.staticplot)
        
        if(values$co <= max(df.staticplot$focal.seq) & values$co >= min(df.staticplot$focal.seq))
        {
          staticplot <- staticplot +
            annotate("point", x = values$co, y =
                       (m$coefficients["(Intercept)"] + values$co*m$coefficients[foc]),
                     color= "black", fill = brewer.pal(9,"Greys")[2], size = 2, shape = 23)
        }
        
        values$plot<-staticplot; staticplot
      }#end input$cat==TRUE
      

# Creating Plot for continuous focal and moderator variables --------------

      else{
        
        
        
        #SET UP DATA FRAMES
        
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
        
        staticplot<-interactiv.plot2(data=data,dfpoints=dfpoints,plotdf = df.staticplot)
        
        if(values$co <= max(df.staticplot$focal.seq) & values$co >= min(df.staticplot$focal.seq))
        {
          staticplot <- staticplot +
            annotate("point", x = values$co, y =
                       (m$coefficients["(Intercept)"] + values$co*m$coefficients[foc]),
                     color= "black", fill = brewer.pal(9,"Greys")[2], size = 2, shape = 23)
        }
        
        values$plot<-staticplot; staticplot
      }#end of else statement for defining static plots
      # }#End of else statement defining continuous moderator static plot
  })#signifies end of output$modplot
  output$results <- renderTable({
    if (is.null(input$file1))
      return(NULL)
    
    m <- values$m
    
    resultstable<-cbind(names(m$coefficients),c(" ",round(QuantPsyc::lm.beta(m),3)),round(summary(m)$coefficients,3))
    colnames(resultstable)<-c(" ","Beta","b","SE","t","Pr(>|t|)")
    resultstable
  })
  
  output$resultstitle<-reactive({
    if (is.null(input$file1))
      return("")
    
    else (paste("Model Coefficients Summary"))
    
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
})

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

output$test.tab <- renderPrint({
  if (is.null(input$file1))
    return(NULL)
  values$ppoints
  
})

output$rostest <- renderTable({
  if (is.null(input$file1))
    return(NULL)
  values$ros
})

output$rosplot <- renderPlot({
  if (is.null(input$file1))
    return(NULL)
  
  values$rosplot
})

output$plot.final.ros <- renderPlot({
  if (is.null(values$rosplot))
    return(NULL)
  plotfinal.ros<-values$rosplot +
    xlab(input$xaxislab.ros) +
    ylab(input$yaxislab.ros) +
    ggtitle(input$rostitle)
  values$plotfinal.ros<-plotfinal.ros
  plotfinal.ros
})

output$plot.final <- renderPlot({
  if (is.null(input$file1))
    return(NULL)
  
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
  } #END SERVER CODE

