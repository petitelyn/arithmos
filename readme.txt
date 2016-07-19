ARITHMOS v0.1 DOCUMENTATION

CONTENTS OF THIS FILE
---------------------
 * Introduction
 * Requirements
 * Code Architecture
 * Configuration
 * Study Formatting
 * Credits 

INTRODUCTION 
---------------------

Arithmos is a private statistics tool for Danone Nutricia Research Centre. The goal of the project was to have in-house statistics software built on top of a private database so scientists could run analyses easily and without file manipulation. 
 
REQUIREMENTS
---------------------

Arithmos is built using the Shiny, a web application framework for R developed by RStudio. Along with the obvious requirements of R and Shiny server, the following packages are required to run the application: 
 
*XLConnectJars: Read xlsx and xlsm files
*XLConnect: Read xlsx and xlsm files
*psych: Gives the "describe" function which displays basic statistics of a variable such as mean and median
*ggplot2: To plot boxplot, scatterplot, barplot, correlation plot and PCA biplot
*RColorBrewer: Provides more options for type of colours used for heatmaps
*stringr: Deals with partial selection of a string
*reshape2: Converts variable to long/wide format
*plotly: Provides interactive plot
*heatmaply: Provides interactive heatmap
*gplots: Provides the function "heatmap.2'
*tidyr: For PCA biplot
*missMDA: Provides the function "imputePCA" to impute missing values of PCA
*VIM: Provides the function "kNN" to imput missing values for hierarchical clustering
*laeken: Provides the function "weightedMean" for "kNN" function
*qvalue: Computes Q value
*nnet: Provides the function "multinom" for multivariate logistic regression models
*car: Provides the function "ANOVA" for multivariate logistic regression models
*plyr: Provides the function "ddply" for the barplot and PCA biplot
*shiny: For shiny server
*shinyjs: Provides the "hidden" function
*shinyBS: Provides the "bsButton" feature
*plotly: Enables interactive plots on shiny
*RPostgreSQL: For communication with the database
*pracma: For the strcmp function

CODE ARCHITECTURE
---------------------

Arithmos follows Shiny's recommended structural pattern with a second layer of file separation for increased organization. Along with the required server.R and ui.R, code is loosely split up based on proximity in the UI. The code is comprised of the following files:

*server.R: Server-related code. Since shiny needs ALL non-UI code to be found here, server.R imports all other non-UI files 
*complexDatabaseCommunication.R: Any code involving multi-line database interactions
*startPanel.R: Code employed by the left-hand panel on the start page of the application, involving uploading files and loading data
*helpers.R: Imports and related helper functions are found
*analysisSidebar.R: Renderings for the sidebar during the analysis phase
*analysisPage.R: Code for rendering the different analysis
*statisticsTable.R: Contains all the functions used for statistics table
*statisticsBoxplot.R: Contains all the functions used for basic boxplot
*statisticsDiffAnal.R: Contains all the functions used for differntial analysis
*statisticsDiffVizu.R: Contains all the functions used for vizualization for differential analysis
*correlationTable.R: Contains all the functions used for correlation and p-value table
*correlationPlot.R: Contains all the functions used for correlation plot
*correlationSearch.R: Contains all the functions used for advance correlation search
*pca.R:Contains all the functions used for PCA
*hc.R: Contains all the functions used for HC
*mainControl.R: Contains all the functions used to build the sidebar panel for data analysis
*preProcessing.R: Contains all the functions used for pre processing
*searchAcross.R: Code related to searching across projects, the right-hand panel on the main page
*ui.R: Code for the UI built through shiny's custom functions
*www/all_css.css: All CSS is found here. Primarily a theme taken from bootswatch.com. Custom css is found at the beginning
*www/relative_x_scrolling.js: A javascript function that keeps the persistent side panel from staying persistent with horizontal scrolling
*www/switch.js: A javascript function that controls the UI flow
NOTE: All front-end files (javascript, css, resources) must be found in the www folder for access from ui.R

CONFIGURATION
---------------------

R, Shiny Server, Postgresql, and all packages listed in requirements must be installed in order to run the application. See the database design document for more information regarding the database.

STUDY FORMAT
---------------------

As of now, the format of studies that can be uploaded is extremeley specific. The file example_study in the miscellaneous folder is an example of the exact (bold) format a study must follow. Important qualifications include:
*The study must be an xlsm file that comprises of two sheets
*The first sheet must be the general information sheet
	*It must outline the ALL of the visit timings found in the data
	*Every word in the first column (ie Study code, Visit number) must be spelled correctly
	*Every category that is not defined must be filled in with an NA
	*The section order (General information, remarks, visit timings) must be the same
*The second sheet must be the data sheet
	*The first section (Analysis remark to No sample availabe) must be in at the start and each defined for every feature (or NA if not applicable)
	*Subject#, Group, Timepoint (Visit) must all be spelled identically 
	*Every variable should have a its full name outlined above it. If a variable does not have a full name, copy the variables regular name to the respective cell above it

In general, if you stick to the given format (which allows for an indiscriminate number of variables), the file should work. With the exception of the actual data, you need to have NAs in spots where the study did not define the specific value.	

CREDITS
---------------------

Arithmos v0.1 was built by interns Gabriel Gardner and Alfrad Chew, overseen by Mei Lyn Ong of the Immunology team at the Singapore R&D center for Danone Nutricia. Contact info follows:

Gabriel Gardner: Database Designer 
		CONTACT: gabrielgardner@icloud.com
Alfrad Chew: Statistician 
		CONTACT: cswa_1992@hotmail.com 

