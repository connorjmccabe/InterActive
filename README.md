# InterActive

The following are instructions for using the interActive data visualization application to analyze and create graphics. The application is located at the following link:

https://connorjmccabe.shinyapps.io/interactive/

## Using Interactive: Example

The following steps for using interActive are based on a simulated dataset of 150 observations. These data are provided in the GitHub repository, or accessible directly for download at the link below:

https://raw.githubusercontent.com/connorjmccabe/InterActive/master/Simulated%20Data/ex1_n150_AMPPS.csv

Before proceeding, be sure to download this data file to your local computer for upload to the interActive application.

### Step 1: select a data file by clicking the “browse” button on the upper left-hand side of the screen.

Acceptable file types are .csv, .xls, .xlsx, and .sav (SPSS data files).

![alt text](https://github.com/connorjmccabe/InterActive/blob/master/Picture1.png)

### Step 2: Select your focal, moderator, and dependent variables using the dropdown menus.

The model being estimated is *Y = X + Z + XZ*. This model can be specified in interActive using the dropdown menus provided (e.g., see below).

![alt text](https://github.com/connorjmccabe/InterActive/blob/master/Picture2.png)

Note that X and Z are centered and Y is untransformed (raw) by default, though multiple scaling options are available. No covariates or quadratic effects will be specified in the current example.

### Step 3: Click the “Run Analysis” button (depicted above) to produce a summary table of coefficients and interaction graphics.

This will generate a summary table of coefficients, conduct regions of significance analyses, and will create the interaction graphics.

### Step 4: Adjust the values of each small multiple as desired.

![alt text](https://github.com/connorjmccabe/InterActive/blob/master/Picture3.png)

Each multiple corresponds with a particular level of the moderator. By default, these range from -2 to 2 standard deviations because this will generally be representative of the moderator range given the moderator is approximately normal. However, we encourage you to modify these values to explore this functionality. Once small multiple values are decided upon, proceed to step 5 to customize the plot.

### Step 5: Click on the “Customize Plot” tab to change axes and plot titles, select the “greyscale” option, and download the finalized plot.

![alt text](https://github.com/connorjmccabe/InterActive/blob/master/Picture4.png)

Users may use these customization options to ready this plot for use in a manuscript. Clicking the “Download Plot” button will output a .png file.

## Additional Functions in interActive

### Examine the marginal effects plot.

Click on the “Marginal Effects Plot” tab to view a depiction of the regions of significance analyses. Note that this plot displays standardized versions of X and Z predictor variables. Customization options for this plot are provided on this tab as well, and users can download this plot using the “Download Marginal Effects Plot” button below the graphic.

### View the plot estimates and raw data.

Users may view the uploaded data by clicking on the “Raw Data” tab to help identify problems in their data (e.g., missing values, outliers, etc.). Users can also view the plot estimates directly by clicking on the “Plot Estimates” tab to view the estimates used to create the small multiples graphic. 

### Download the R objects used to generate plots and upload them directly to R/Rstudio.

Users can download an .rds file using the “Download Plot Data” button located on the upper-righthand side of the screen under the "Plot" tab. Advanced users can read this file into R or Rstudio to customize their plots or examine their models in greater detail. Example code for reading an .rds file into R or Rstudio is provided below:

```
readRDS("~/Downloads/plotdata.rds")
```

### Explore the interaction with skewed predictor variables.

Provided in this data file are positively-skewed predictor variables simulated from exponential distributions. We encourage the reviewers to conduct the above steps using these variables as well, by specifying the following model in interActive: 

yexpXZ = Xexp + Zexp + Xexp*Zexp

The screenshot below depicts the dropdown menu options for producing this graphic:

![alt text](https://github.com/connorjmccabe/InterActive/blob/master/Picture5.png)
