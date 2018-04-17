#Server

options(digits=9)
options(warn=-1) #Suppress Warnings
options(shiny.maxRequestSize=20*1024^2) #max file size = 20 MB
require(ggplot2)
require(RColorBrewer)
require(foreign)
require(pander)
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

# END HELPER FUNCTIONS ----------------------------------------------------


server<-function(input, output, session) {
  values<-reactiveValues() #defines empty list that will take model-produced values
  plotelements<-reactiveValues() #defines empty list that will take model-produced values
  
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
  
  #Model Selection Options ------------------------------------
  
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
  
  output$ui.navals<-renderUI ({
    if (is.null(input$file1))
      return()
    
    textInput(inputId="naval",
              label="Missing (NA) values code:",
              value = "NA",
              width = "100%")  
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
      # validate(
      #   need(values$staticplot != "", "Please wait...")
      # )
      
      #Defining plot template function
      interactive.plot<-function(data, dfpoints, plotdf){
        ggplot() +
          
          geom_point(data=dfpoints, aes(x=pred,y=y),size = .75, alpha = .5) +
          geom_ribbon(data=plotdf, aes(x=focal.seq, y=pe, ymin = lower, ymax = upper, fill = level), alpha = .25) +
          geom_line(data = plotdf, aes(focal.seq, pe, fill=level, color=level), size=1) +
          
          ylim(min(c(plotdf[,"lower"],min(data[,dv]))),max(c(plotdf[,"upper"],max(data[,dv])))) +
          
          xlim(min(data[,foc], na.rm = TRUE),max(data[,foc], na.rm = TRUE)) +
          
          #thematic specifications of my graphic
          theme(text=element_text(family="Helvetica",size=10, color="black"),
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
                axis.line = element_line(size=.4),
                panel.grid.major = element_blank(),
                plot.background=element_rect(fill='white')) +
          labs(fill = mod, color = mod, linetype=mod) +
          
          labs(x = foc, y = dv) +
          
          coord_cartesian() +
          
          ggtitle(paste0("Level of Moderator (",mod,")")) +
          
          geom_hline(yintercept=c(max(data[,dv], na.rm = TRUE),min(data[,dv], na.rm = TRUE)),linetype=2) +
          
          facet_grid(~level) +
          # theme(strip.text.x = element_text(size = 12, colour = "black")) +
          scale_color_manual(values=df.cols$col) +
          scale_fill_manual(values=df.cols$colslight) +
          
          theme(plot.title = element_text(family="Helvetica", face="bold", size=14, hjust=0, color="black"))
      }
      
      
      # Analyses code -----------------------------------------------------------
      
      dftrue<-plotelements$data<-df() #retain an original copy
      
      #Replace all missing values in dataset with NA
      dftrue[dftrue==input$naval]<-NA
      
      data<-dftrue #make a copy of the data for variable rescaling and analyses
      
      
      mod <- input$mod #define moderator
      foc <- input$foc #define focal predictor
      dv <- input$dv #define outcome variable
      
      data<-as.data.frame(data.matrix(data))
      
      #Re-scale variables based on user input
      if (input$scalex == "cen"){

        
        data[,foc] <- scale(data[,foc],  scale = FALSE, center = TRUE)

      }
      if (input$scalez == "cen" && input$cat==FALSE){

        
        data[,mod] <- scale(data[,mod],  scale = FALSE, center = TRUE)
        
      }
      
      if (input$scaledv == "cen"){
        data[,dv] <- scale(data[,dv],  scale = FALSE, center = TRUE)
      }
      
      if (input$scalex == "std"){
        data[,foc] <- scale(data[,foc],  scale = TRUE)
      }
      if (input$scalez == "std" && input$cat==FALSE){
        data[,mod] <- scale(data[,mod],  scale = TRUE)
      }
      if (input$scaledv == "std"){
        data[,dv] <- scale(data[,dv],  scale = TRUE)
      }
      
      values$modlevel<-input$modlevel
      if(input$cat == FALSE) (values$modlevel<- mean(data[,mod], na.rm = TRUE))
      
      modlevel <- values$modlevel
      
      if(input$cat==FALSE){data[,mod]<-data[,mod]-values$modlevel}
      
      cov<-paste(input$covars, collapse=" + ")
      
      values$covars<-input$covars
      
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
      
      values$m<-plotelements$model<-lm(values$form,data, na.action = na.omit)

      values$model<-as.formula(paste(dep,cov,focmod))
      
      m <- values$m
      mtrue <- values$mtrue
      
      data[,mod] <- as.numeric(data[,mod])
      
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
        values$dep2<-dep2
        values$cov<-cov
        values$focmod2<-focmod2
        values$form2<-as.formula(paste(dep2,cov,focmod2))
        values$model2<-as.formula(paste(dep2,cov,focmod2))
        
      }
      
      #Results table creation code
      m <- values$m
      
      resultstable<-cbind(names(m$coefficients),c(" ",round(QuantPsyc::lm.beta(m),3)),round(summary(m)$coefficients,3))
      colnames(resultstable)<-c(" ","Beta","b","SE","t","Pr(>|t|)")
      
      values$resultstable<-resultstable
      
      
      # Regions-of-significance analyses and marginal effects plot code ---------------------------------------------------------
      
      if(input$cat == FALSE){
        df.ros<-as.data.frame(data.matrix(dftrue))
        
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
        
        
        values$ros<-plotelements$ros<-ros[c("hyp.Z","pe.X","LL","UL")]
        
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
        
        values$text.negneg<-values$text.negpos<-values$text.posneg<-values$text.pospos<-paste("")
        
        if(m$coefficients[paste0(foc,":",mod)]<0){
          
          if("not sig"%in%ros[which(ros$sign=="neg"),"significance"]==TRUE &
             "sig"%in%ros[which(ros$sign=="neg"),"significance"]==TRUE ){  
            
            sigline.negneg<-min(ros[which(ros$significance=="sig" & ros$sign=="neg"),"hyp.Z"])
            rosplot <- rosplot + 
              geom_vline(xintercept = sigline.negneg, linetype="dashed")
            pct.negneg<-(length(df.ros[which(df.ros[,mod]>sigline.negneg),mod])/length(df.ros[,mod]))*100
            
            values$text.negneg<-paste0("The simple slope of ",input$foc," on ",input$dv," is significant and negative when ",input$mod," is ",sigline.negneg," standard deviations away from the mean or further. ",round(pct.negneg,2),"% of observations are within this region.\n")
          }
          
          if("not sig"%in%ros[which(ros$sign=="pos"),"significance"]==TRUE &
             "sig"%in%ros[which(ros$sign=="pos"),"significance"]==TRUE){  
            
            sigline.negpos<-max(ros[which(ros$significance=="sig" & ros$sign=="pos"),"hyp.Z"])
            rosplot <- rosplot + 
              geom_vline(xintercept = sigline.negpos, linetype="dashed")
            pct.negpos<-(length(df.ros[which(df.ros[,mod]<sigline.negpos),mod])/length(df.ros[,mod]))*100
            
            values$text.negpos<-paste0("The simple slope of ",input$foc," on ",input$dv," is significant and positive when ",input$mod," is ",sigline.negpos," standard deviations away from the mean or further. ",round(pct.negpos,2),"% of observations in ",input$mod," are within this region.\n")
          }
        }
        else{
          if("not sig"%in%ros[which(ros$sign=="neg"),"significance"]==TRUE &
             "sig"%in%ros[which(ros$sign=="neg"),"significance"]==TRUE ){  
            
            sigline.posneg<-max(ros[which(ros$significance=="sig" & ros$sign=="neg"),"hyp.Z"])
            rosplot <- rosplot + 
              geom_vline(xintercept = sigline.posneg, linetype="dashed")
            pct.posneg<-(length(df.ros[which(df.ros[,mod]<sigline.posneg),mod])/length(df.ros[,mod]))*100
            
            values$text.posneg<-paste0("The simple slope of ",input$foc," on ",input$dv," is significant and negative when ",input$mod," is ",sigline.posneg," standard deviations away from the mean or further. ",round(pct.posneg,2),"% of observations in ",input$mod," are within this region.\n")
          }
          
          if("not sig"%in%ros[which(ros$sign=="pos"),"significance"]==TRUE &
             "sig"%in%ros[which(ros$sign=="pos"),"significance"]==TRUE){  
            
            sigline.pospos<-min(ros[which(ros$significance=="sig" & ros$sign=="pos"),"hyp.Z"])
            rosplot <- rosplot + 
              geom_vline(xintercept = sigline.pospos, linetype="dashed")
            pct.pospos<-(length(df.ros[which(df.ros[,mod]>sigline.pospos),mod])/length(df.ros[,mod]))*100
            
            values$text.pospos<-paste0("The simple slope of ",input$foc," on ",input$dv," is significant and positive when ",input$mod," is ",sigline.pospos," standard deviations away from the mean or further. ",round(pct.pospos,2),"% of observations in ",input$mod," are within this region.\n")
            
          }
        }
        values$rostext<-paste0(values$text.negneg,"\n",values$text.negpos,"\n",values$text.posneg,"\n",values$text.pospos)
        
        values$rosplot<-rosplot
        plotelements$rosplot<-rosplot + xlab("Moderator") + ylab("Simple Slope of Focal Predictor")
      }
      
      # Creating plot data (simple slope estimates and confidence regions) for each small multiple --------------
      
focal.seq <- sort(c(-1*sd(data[,foc],na.rm=T),
                    mean(data[,foc],na.rm=T),
                    1*sd(data[,foc],na.rm=T),
                    seq(min(data[,foc], na.rm = TRUE), 
                        max(data[,foc], na.rm = TRUE),
                        .1*sd(data[,foc],na.rm=T))))
      
      if(values$co <= max(focal.seq) & values$co >= min(focal.seq)){
        focal.seq<-sort(c(focal.seq,values$co))
      }
      sm1<-as.numeric(input$sm1)*(sd(data[,mod], na.rm=TRUE))
      sm2<-as.numeric(input$sm2)*(sd(data[,mod], na.rm=TRUE))
      sm3<-as.numeric(input$sm3)*(sd(data[,mod], na.rm=TRUE))
      sm4<-as.numeric(input$sm4)*(sd(data[,mod], na.rm=TRUE))
      sm5<-as.numeric(input$sm5)*(sd(data[,mod], na.rm=TRUE))
      
      xi<-rep(0,length(values$m$coefficients))
      
      matxi1<-as.data.frame(matrix(xi,length(focal.seq),length(xi)))
      names(matxi1)<-names(values$m$coefficients)
      if(length(input$covars)==1){
        covmeans<-mean(data[,input$covars],na.rm=TRUE)
        matxi1[,input$covars]<-rep(covmeans,length(focal.seq))
      }
      else{
        covmeans<-lapply(data[,input$covars],mean, na.rm=TRUE)
        for (i in 1:length(covmeans)){matxi1[,input$covars[i]]<-rep(covmeans[i],ncol(matxi1))}
      }
      values$covmeans<-covmeans
      matxi1[,foc]<-focal.seq
      matxi1$`(Intercept)`<-rep(1,nrow(matxi1))
      values$matxi1<-matxi1
      matxi2<-matxi3<-matxi4<-matxi5<-matxi1
      if(input$cat ==TRUE){
        mod.seq<-sort(unique(data[,mod]))
        matxi1[,mod]<-rep(mod.seq[1],nrow(matxi1))
        matxi2[,mod]<-rep(mod.seq[2],nrow(matxi2))
        matxi<-rbind(matxi1,matxi2)
      }
      else{
        mod.seq <- c(as.numeric(sm1),
                     as.numeric(sm2),
                     as.numeric(sm3),
                     as.numeric(sm4),
                     as.numeric(sm5))
        
        matxi1[,mod]<-rep(mod.seq[1],nrow(matxi1))
        matxi2[,mod]<-rep(mod.seq[2],nrow(matxi2))
        matxi3[,mod]<-rep(mod.seq[3],nrow(matxi3))
        matxi4[,mod]<-rep(mod.seq[4],nrow(matxi4))
        matxi5[,mod]<-rep(mod.seq[5],nrow(matxi5))
        
        matxi<-rbind(matxi1,matxi2,matxi3,matxi4,matxi5)
      }
      
      matxi[,paste0(foc,":",mod)]<-matxi[,foc]*as.numeric(matxi[,mod])
      Xmat<-as.data.frame(cbind(rep(1,nrow(values$m$model)),
                                values$m$model[,-1]))
      colnames(Xmat)<-c("(Intercept)",colnames(values$m$model[,-1]))
      Xmat[,paste0(foc,":",mod)]<-(Xmat[,foc])*(as.numeric(Xmat[,mod]))
      
      if(input$poly == "quad") {
        matxi[,paste0("I(",foc,"^2)")]<-matxi[,foc]*matxi[,foc]
        matxi[,paste0("I(",foc,"^2):",mod)]<-matxi[,foc]*matxi[,foc]*as.numeric(matxi[,mod])
        
        Xmat[,paste0("I(",foc,"^2)")]<-(Xmat[,foc])*(Xmat[,foc])
        Xmat[,paste0("I(",foc,"^2):",mod)]<-as.numeric(Xmat[,mod])*(Xmat[,foc])*(Xmat[,foc])
      }
      
      Xmat<-as.matrix(Xmat)
      lapply(Xmat,as.numeric)
      
      values$matxi<-matxi
      values$Xmat<-Xmat
      matquant<-solve((t(Xmat) %*% Xmat))
      values$matquant<-matquant
      y<-values$m$model[,1]
      
      A<-solve((t(Xmat) %*% Xmat))
      values$A<-A
      beta <- A %*% t(Xmat) %*%  y
      values$beta<-beta
      resids <- y - (Xmat %*% beta)
      values$resids
      sig<-sqrt(sum(resids^2) / (nrow(Xmat)-ncol(Xmat)))
      values$sig
      
      pes<-as.matrix(matxi)%*%beta
      values$pes<-pes
      SEys<-diag(sig*(sqrt(as.matrix(matxi)%*%matquant%*%t(as.matrix(matxi)))))#Warning is generated because of square-rooting negative values in the off-diagonals of the covariance matrix. This is OK since we are only taking the diagonals of this matrix, which will always be positive.
      fac <- qt(.975,values$m$df.residual)
      ylower<-pes - fac*SEys
      yupper<-pes + fac*SEys
      
      df.staticplot<-as.data.frame(cbind(focal.seq,matxi[,mod],pes,ylower,yupper))
      names(df.staticplot)<-c("focal.seq","modlevel","pe","lower","upper")
      df.staticplot<-as.data.frame(df.staticplot)
      
      pval<-level<-list()
      data$s.foc<-data[,foc]
      
      #percentiles corresponding with each small multiple value
      perc.sm1<-ecdf(scale(data[,mod]))(as.numeric(input$sm1))
      perc.sm2<-ecdf(scale(data[,mod]))(as.numeric(input$sm2))
      perc.sm3<-ecdf(scale(data[,mod]))(as.numeric(input$sm3))
      perc.sm4<-ecdf(scale(data[,mod]))(as.numeric(input$sm4))
      perc.sm5<-ecdf(scale(data[,mod]))(as.numeric(input$sm5))
      percentiles<-100*c(perc.sm1,perc.sm2,perc.sm3,perc.sm4,perc.sm5)
      
      for (k in 1:length(mod.seq)){
        
        data$s.mod<-data[,mod]-mod.seq[k]
        lm<-lm(values$form2,data, na.action = na.omit)
        pval[[k]]<-rep(summary(lm)$coefficients["s.foc","Pr(>|t|)"],length(focal.seq))
        if(input$poly=="lin"){
          leveltemp<-rep(paste0("\nb = ",
                                round(lm$coefficients["s.foc"],2),"\n95% CI = [",
                                round(confint(lm)["s.foc","2.5 %"],2),", ",
                                round(confint(lm)["s.foc","97.5 %"],2),"]"),length(focal.seq))
        }else(leveltemp<-rep("",length(focal.seq)))
        if(input$cat==TRUE){level[[k]]<-paste0("Category ",mod.seq[k],leveltemp)}
        else(level[[k]]<-paste0(mod.seq[k]/sd(data[,mod],na.rm=TRUE)," SD (PTCL = ",round(percentiles[k],2),")",leveltemp))
      }
      
      df.staticplot$p.val<-do.call(c,pval)
      df.staticplot$level<-do.call(c,level)
      values$df.staticplot<- df.staticplot
      
      #Define bluescale and greyscale color palletes
      bluespal<-c("#08306B","#2171B5","#4292C6","#9ECAE1","#C6DBEF")
      greyspal<-c("#000000","#252525","#525252","#737373","#969696")
      names(bluespal)<-c("less001","less01","less05","less10","greater10")
      names(greyspal)<-c("less001","less01","less05","less10","greater10")
      
      if(input$poly == "quad"){
        df.staticplot$col<-rep(bluespal["less05"],nrow(df.staticplot))
        df.staticplot$greycol<-rep(greyspal["less05"],nrow(df.staticplot))
      }
      else{
        
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
      }
      
      values$dfplot<-plotelements$dfplot<-df.staticplot
      
      # End Plot Data Creation Code ---------------------------------------------   
      
      
      # Creating a dataframe containing observed data for each small multiple---------------------------
      
      modindex<- as.data.frame(unique(df.staticplot[c("modlevel","p.val","level")]))
      values$modindex<-modindex
      
      #Define a reduced data frame for display of observed data
      dfpoints<-as.data.frame(cbind(data[,foc],glm(values$form,data, na.action = na.omit,family="gaussian")$y,m$fitted.values))
      colnames(dfpoints)<-c("pred","y","fitted")
      
      if(input$cat==FALSE){   
        
        modindex$scalelevel<-c(input$sm1,input$sm2,input$sm3,input$sm4,input$sm5)
        
        divides<-c(((as.numeric(input$sm2) - as.numeric(input$sm1))/2),
                   ((as.numeric(input$sm3) - as.numeric(input$sm2))/2),
                   ((as.numeric(input$sm4) - as.numeric(input$sm3))/2),
                   ((as.numeric(input$sm5) - as.numeric(input$sm4))/2))
        divides<-abs(divides)
        
        values$divides<-divides
        #Compute percentile scores corresponding with each small multiple
        
        perc.div1<-ecdf(scale(data[,mod]))(as.numeric(input$sm1)+divides[1])
        perc.div2<-ecdf(scale(data[,mod]))(as.numeric(input$sm2)+divides[2])
        perc.div3<-ecdf(scale(data[,mod]))(as.numeric(input$sm3)+divides[3])
        perc.div4<-ecdf(scale(data[,mod]))(as.numeric(input$sm4)+divides[4])
        
        dfpoints$p.val[(rank(data[,mod])/length(data[,mod]))<perc.div1]<-round(modindex$p.val[which(modindex$modlevel== sm1)],3)
        
        dfpoints$p.val[(rank(data[,mod])/length(data[,mod]))<perc.div2 & (rank(data[,mod])/length(data[,mod]))>=perc.div1] <-round(modindex$p.val[which(modindex$modlevel== sm2)],3)
        
        dfpoints$p.val[(rank(data[,mod])/length(data[,mod]))<perc.div3 & (rank(data[,mod])/length(data[,mod]))>=perc.div2]<-round(modindex$p.val[which(modindex$modlevel== sm3)],3)
        
        dfpoints$p.val[(rank(data[,mod])/length(data[,mod]))<perc.div4 & (rank(data[,mod])/length(data[,mod]))>=perc.div3]<-round(modindex$p.val[which(modindex$modlevel== sm4)],3)
        
        dfpoints$p.val[(rank(data[,mod])/length(data[,mod]))>perc.div4]<-round(modindex$p.val[which(modindex$modlevel== sm5)],3)
        
        #SM1
        dfpoints$level[(rank(data[,mod])/length(data[,mod]))<perc.div1]<-modindex$level[which(modindex$modlevel== sm1)]
        
        #SM2
        dfpoints$level[(rank(data[,mod])/length(data[,mod]))<perc.div2 & (rank(data[,mod])/length(data[,mod]))>=perc.div1]<-modindex$level[which(modindex$modlevel== sm2)]
        
        #SM3
        dfpoints$level[(rank(data[,mod])/length(data[,mod]))<perc.div3 & (rank(data[,mod])/length(data[,mod]))>=perc.div2]<-modindex$level[which(modindex$modlevel== sm3)]
        
        #SM4
        dfpoints$level[(rank(data[,mod])/length(data[,mod]))<perc.div4 & (rank(data[,mod])/length(data[,mod]))>=perc.div3]<-modindex$level[which(modindex$modlevel== sm4)]
        
        #SM5
        dfpoints$level[(rank(data[,mod])/length(data[,mod]))>perc.div4]<-modindex$level[which(modindex$modlevel== sm5)]
        
        dfpoints<-na.omit(dfpoints)
        
        dfpoints$level <- factor(dfpoints$level, levels = c(modindex$level[which(modindex$modlevel== sm1)],
                                                            modindex$level[which(modindex$modlevel== sm2)],
                                                            modindex$level[which(modindex$modlevel== sm3)],
                                                            modindex$level[which(modindex$modlevel== sm4)],
                                                            modindex$level[which(modindex$modlevel== sm5)]))
        
        df.staticplot$level <- factor(df.staticplot$level, levels = c(modindex$level[which(modindex$modlevel== sm1)],
                                                                      modindex$level[which(modindex$modlevel== sm2)],
                                                                      modindex$level[which(modindex$modlevel== sm3)],
                                                                      modindex$level[which(modindex$modlevel== sm4)],
                                                                      modindex$level[which(modindex$modlevel== sm5)]))
        
      }
      else{
        dfpoints$modlevel<-data[,mod]
        modindex$scalelevel<-c(mod.seq[1],mod.seq[2])
        dfpoints$level[dfpoints$modlevel==mod.seq[1]]<-modindex$level[which(modindex$scalelevel==mod.seq[1])]
        dfpoints$level[dfpoints$modlevel==mod.seq[2]]<-modindex$level[which(modindex$scalelevel==mod.seq[2])]
        dfpoints<-na.omit(dfpoints)
      }
      
      values$dfpoints<-plotelements$plotpoints<-dfpoints
      
      df.colstemp<-df.staticplot[c("level","col","greycol")]
      df.cols<-df.colstemp[!duplicated(df.colstemp$level), ]
      df.cols$colslight<-sapply(df.cols$col, function(i) {df.cols$col<- lighten(i,pct=0.50)})
      df.cols$greycolslight<-sapply(df.cols$greycol, function(i) {df.cols$greycol<- lighten(i,pct=0.50)})
      df.cols<-as.data.frame(df.cols)
      values$df.cols<-df.cols
      
      
      # End data display code ---------------------------------------------------
      
      #Specify a small multiples graphic based on the estimated point estimates and confidence regions (df.staticplot) as well as the observed data (dfpoints).
      staticplot<-interactive.plot(data=data,dfpoints=dfpoints,plotdf = df.staticplot)
      
      #Define a glyph of the crossover point if it exists within the observed data
      if(values$co <= max(df.staticplot$focal.seq) & values$co >= min(df.staticplot$focal.seq))
      {
        staticplot <- staticplot +
          annotate("point", x = values$co, y =
                     (df.staticplot[which(df.staticplot$focal.seq == values$co),"pe"]),
                   color= "black", fill = brewer.pal(9,"Greys")[2], size = 2, shape = 23)
      }
      
      
      values$plot<-plotelements$smplot<-staticplot; staticplot
      # }#end of else statement for defining static plots
      # }#End of else statement defining continuous moderator static plot
    })#signifies end of output$modplot
    
    #Read the above-defined elements into output in Shiny
    output$results <- renderTable({
      if (is.null(input$file1))
        return(NULL)
      
      values$resultstable
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
  
  output$rostest <- renderText({
    if (is.null(input$file1))
      return(NULL)
    values$rostext
  })
  
  output$rosplot <- renderPlot({
    if (is.null(input$file1))
      return(NULL)
    
    values$rosplot
  })
  
  #Creates plot outputs of a customized small multiples plot
  
  #Customized marginal effects plot
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
  
  #Customized small multiples plot
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
    values$plotfinal<-plotelements$plotfinal<-plotfinal
    values$plotfinal
  })
  
  output$downloadplotdata <- downloadHandler(
    filename = 'plotdata.rds',
    content = function(file) {
      saveRDS(shiny::reactiveValuesToList(plotelements), file = file)
    })
  
  output$ptclnote1<-output$ptclnote2<-renderText({
    paste0("Note: PTCL = percentile.")
  })
  
  output$cite<-renderUI({
    str1 <- paste0("If you are using output generated by the interActive utility in a published manuscript, please use the following citation:")
    str2 <- paste0("McCabe, C. J., Kim, D. S., & King, K. M. (In press). Improving present practices in the visual display of interactions. Advances in Methods and Practices in Psychological Science.")
    str3 <- paste0("If you encounter any errors using interActive, please contact cmccabe@uw.edu for assistance.")
    
    HTML(paste(str1, str2, str3, sep = '<br/><br/>'))
  })
  
} #END SERVER CODE

# ui.R
ui<-shinyUI(fluidPage(
  titlePanel("interActive: A tool for the visual display of interactions"),
  
  sidebarLayout(
    sidebarPanel(
      
      fileInput('file1', 'Choose File (max size = 30 MB)',
                accept=c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv',
                         '.xls',
                         '.xlsx',
                         '.sav')),
      
      tags$hr(),
      uiOutput("ui.categorical"),
      uiOutput("ui.foc"),
      uiOutput("ui.scalex"),
      uiOutput("ui.mod"),
      uiOutput("ui.scalez"),
      uiOutput("ui.dv"),
      uiOutput("ui.scaledv"),
      uiOutput("ui.covars"),
      uiOutput("ui.poly"),
      uiOutput("ui.navals"),
      uiOutput("ui.go"),
      tags$br(),
      tags$br(),
      htmlOutput("cite"),
      width = 3),
    
    mainPanel(em(h2("Select a dataset to begin.")),
              tags$style(type="text/css",
                         ".shiny-output-error { visibility: hidden; }",
                         ".shiny-output-error:before { visibility: visible; content: 'please wait...'; 
                         color: grey; }"
              ),
              tabsetPanel(id = "inTabset",
                          tabPanel("Data Descriptives",
                                   tableOutput("varnames")),   
                          tabPanel("Plot",
                                   
                                   column(7,
                                          fluidRow(
                                            column(2, align="center",
                                                   ""),
                                            column(2, align="center",
                                                   uiOutput("ui.sm1")),
                                            column(2, align="center",
                                                   uiOutput("ui.sm2")),
                                            column(2, align="center",
                                                   uiOutput("ui.sm3")),
                                            column(2, align="center",
                                                   uiOutput("ui.sm4")),
                                            column(2, align="center",
                                                   uiOutput("ui.sm5"))),
                                          plotOutput("modplot"),
                                          uiOutput("ui.downloadplot"),
                                          tags$br(),
                                          textOutput("ptclnote1")),
                                   column(4,
                                          textOutput("regFormula"),br(),
                                          textOutput("resultstitle"),
                                          tableOutput("results"),
                                          textOutput("co"),
                                          downloadButton('downloadplotdata', 'Download Plot Data')
                                   )
                          ),
                          tabPanel("Customize Plot",
                                   plotOutput("plot.final"),
                                   fluidRow(column(3,
                                                   uiOutput("ui.xlab"),
                                                   uiOutput("ui.ylab")),
                                            column(4,
                                                   uiOutput("ui.title")),
                                            column(5,
                                                   uiOutput("ui.greyscale"))
                                   ),
                                   downloadButton('downloadPlot.final', 'Download Plot Image'),
                                   tags$br(),
                                   tags$br(),
                                   textOutput("ptclnote2")
                          ),
                          tabPanel("Marginal Effects Plot",
                                   column(8,
                                          uiOutput("ui.rosgo"),
                                          plotOutput("plot.final.ros"),
                                          fluidRow(column(4,
                                                          uiOutput("ui.xlab.ros"),
                                                          uiOutput("ui.ylab.ros")),
                                                   column(5, uiOutput("ui.title.ros"))
                                          ),
                                          downloadButton('rosplot.final',"Download Marginal Effects Plot")
                                          ),
                                   column(4,
                                          textOutput("rostest")
                                   )),
                          tabPanel("Plot Estimates",
                                   column(8,
                                          tableOutput("plotdata"))
                          ),
                          tabPanel("Raw Data",tableOutput("datashow"))
                          
              )#tabsetPanel
    ) #mainpanel
    # )#column
  ) #sidebarlayout
))

shinyApp(ui = ui, server = server)
