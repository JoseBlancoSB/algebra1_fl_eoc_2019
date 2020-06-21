print("This is my original R script")
print("I am uploading it to github")


# 1)  Downloading Algebra 1, 2019 Data from Florida Dept. of Educ.
#
#   library(downloader)
#
#   url <- "https://www.fldoe.org/core/fileparse.php/5668/urlt/31Spring19Alg1SRS.xls"
#
#   filename <- basename(url)
#
#   set the working directory you want to download to.  In this case,
#   > setwd("/home/jose/R/data/MyAlgebra")
#
#   download(url, filename)
#
#  The following live code downloads the file, even without an open browser.

library(downloader)
url <- "https://www.fldoe.org/core/fileparse.php/5668/urlt/31Spring19Alg1SRS.xls"
filename <- basename(url)
setwd("/home/jose/R/data/MyAlgebra")
download(url, filename)

# 2) Import and Rename Original File to R/RStudio
#
#  install.packages("readxl")  #  If it is not installed already
#  library(readxl)
#
#   The readxl package allows R to read .xls files as .csv -- which are easy to use
#   in R.
#   
#   You have to look at the spreadsheet.  I don't know enough R to be able to 
#   automate this step.  When you look at the spreadsheet, you see that the first
#   four lines contain information that is unnecessary for data analysis, so we
#   skip them.  Use this command:
#
#   basefile <- read_excel("31Spring19Alg1SRS.xls", skip = 4)
#
#   This command creates a dataframe in R called "basefile"
#   in the working directory.  You can see that it has 1741 observations (rows) 
#   and 12 variables (columns).
#

library(readxl)
basefile <- read_excel("31Spring19Alg1SRS.xls", skip = 4)

#
#   3) Clean the dataframe so that analysis is easier.
#
#     For cleaning (or tidying) the data, we need the tidyverse package.
#   If it is not loaded, run this code:
#
#   install.packages("tidyverse")
#

library(tidyverse)

#  3) Clean the data, so that the dataframe is easier to work with.
#
#     What doe it mean to "clean the data"?  Well, at this stage of
#     my learning "cleaning the data" means:
#         A) Converting the dataframe to a tibble, if it is not
#           already a tibble.
#         B) Eliminate ("filter()") rows with NA or '*' values.
#         C) Rename the variables (columns) so that they are easy
#            to work with and R recognizes them.
#         D) Convert ("coerce")  the variables to the datatype that
#           allows for numerical manipulation.
#   
#   Let's explcitely convert the data frame to a tibble.
#   (No need to do this.  It is already a tibble.)
#   basefile <- tibble::as_tibble(basefile)
#
#   Now we want to filter out, or eliminate, the rows that have an asterisk ("*")
#   as a value.  Schools where fewer than 10 students took the Algebra 1 EOC have an
#   asterisk as the "Mean Scale Score"; we want to eliminate (filter) those rows ( also
#   called "observations" in R).
#
#
#   I am not sure, but I THINK that because the variable (row name) is made up of
#   multiple strings (Mean Scale Score), that I have to use the vanilla R column
#   selection -- basefile$'Mean Scale Score' notation to specify a column.  This
#

library(dplyr)
basefile <- filter(basefile, basefile$`Mean Scale Score` != "*")

#
#   It would be useful to give all of the column names a name that is made
#   up of a single character string.  The dplyr rename() method does so
#

col_names <- colnames(basefile)    # lists the names of the columns in basefile

#
#    [1] "District Number"                 "District Name"                  
#    [3] "School Number"                   "School Name"                    
#    [5] "Number of Students"              "Mean Scale Score"               
#    [7] "Percentage in\nLevel 3 or Above" "1"                              
#    [9] "2"                               "3"                              
#    [11] "4"                               "5"  
#
#   basefile <- rename(basefile, 'dist_num' = 'District Number',
#                              'dist_name' = 'District Name',
#                              'sch_num' = 'School Number',
#                              'sch_name' = 'School Name',
#                              'num_stu' = 'Number of Students',
#                              'mean_scl' = 'Mean Scale Score',
#                              'pass' = 'Percentage in\nLevel 3 or Above',
#                              'one' = '1',
#                              'two' = '2',
#                              'three' = '3',
#                              'four' = '4','five' = '5')

basefile <- rename(basefile, 'dist_num' = 'District Number',
                                 'dist_name' = 'District Name',
                                 'sch_num' = 'School Number',
                                 'sch_name' = 'School Name',
                                 'num_stu' = 'Number of Students',
                                 'mean_scl' = 'Mean Scale Score',
                                 'pass' = 'Percentage in\nLevel 3 or Above',
                                 'one' = '1',
                                 'two' = '2',
                                 'three' = '3',
                                 'four' = '4',
                                 'five' = '5')


#
#   In order to work with numerical data, we have to change ("coerce in R)
#   several columns from character data (chr) to numeric data, probably int.
#
#   I do not understand some of the following code, notably the word 'function(x)'
#   but I will find out.  It works.  This code coerces columns 6 - 12 in basefile
#   from chr to int.
#

i <- c(6:12)

#
#   I understand the above code.  It creates a variable 'i' which is a numeric
#   vector that represents columns 6, 7, 8, 9, 10, 11, and 12 in the basefile
#   data frame.  These are the columns we want to coerce from chr to int.
#

basefile[ ,i] <- apply(basefile[ , i], 2, function(x) as.integer(as.character(x)))

#
#   The number '2' above means that the function will be applied to columns.
#   The number '1' would apply to rows.  I get this!
#
#

#   4) Create the final dataframe that lends itself to our analysis.
#      This process involves summarising, using group_by(), filtering,
#      selecting.  Piping helps to avoid naming files like "test" and
#      "test1" but I am going to keep my original code here.
#
# Create a dataframe that has ONLY the state totals.

FL_totals_only_basefile <- filter(basefile, basefile$sch_num == '0000')

#   It is useful to get rid of (filter) the first row, which has the state totals
#

schools_basefile <- filter(basefile, basefile$sch_num != '0000')

#
#   The group_by() method in the command below tells R to consider commands
#   that are passed to it to act upon it, to "chunk" the dataframe into
#   individual counties when using the summarize() method, not the entire
#   data frame.
#

county <- group_by(basefile, dist_name, .add = TRUE)

#
#   Now that the dataframe is "chunked" by counties (or "dist_name"), we can
#   get summaries of the individual counties by using the summarise() method.
#   notice that summarise() does not act on the entire data frame, but only on
#   the "grouped" or "chunked" counties.  That is what the group_by() method
#   accomplished.  
#
#

fl_counties <- summarise(county,
                         mean_scl = mean(mean_scl), 
                         pass = mean(pass),
                         one = mean(one), 
                         two = mean(two), 
                         three = mean(three),
                         four = mean(four), 
                         five = mean(five))
                         
#
#   The following command creates a dataframe with two observations, 
#   Florida Totals and one school.  I have to figure out how to get a 
#   summary of the district totals in the dataframe.
#
#

one_school <- filter(basefile,
         (basefile$sch_num == '0000' |
          basefile$sch_num == '6071'))

#
#
#   The following code creates a dataframe that "joins" one_school and
#   fl_counties.
#

test <- full_join(one_school, fl_counties)

#
#   Creates a data frame withh the 3 observations I need:  state, county, school.
#   Now I have to clean it to use it.
#

test3 <- filter(test, dist_name == 'STATE TOTALS' | dist_name == 'MIAMI-DADE')

#
#   Creates a dataframe with only the relevant columns for analysis
#

my_data <- select(test3, sch_name, mean_scl, pass, one, two, three, four, five)

my_data[3, 1] <- 'Miami-Dade'
my_data[1, 1] <- 'Florida'
my_data[2, 1] <- 'G. W. Carver MS'
#
#   In order to work with numerical data, we have to change ("coerce in R)
#   several columns from 'double' data (dbl) to int data, probably int.
#
#   I do not understand some of the following code, notably the word 'function(x)'
#   but I will find out.  It works.  This code coerces columns 2 - 8 in my_data
#   from chr to int.
#

i <- c(2:8)

#
#   I understand the above code.  It creates a variable 'i' which is a numeric
#   vector that represents columns 2, 3, 4, 5, 6, 7, and 8 in the my_data
#   data frame.  These are the columns we want to coerce from dbl to int.
#
#   First, round the columns.
#

my_data[ ,i] <- apply(my_data[ , i], 2, function(x) round(as.double(x)))

#
#   Then, convert to integer

my_data[ ,i] <- apply(my_data[ , i], 2, function(x) as.integer(as.double(x)))

#
#   The number '2' above means that the function will be applied to columns.
#   The number '1' would apply to rows.  I get this!

#
#   5) Perform the data visualizations.
#
#############################3  Data Visualizations ##########################
#
#   Now that we have a dataframe we can work with, we have to communicate
#   the results in a meaningful way.  The best way is through visualizations.
#   We will use ggplot2 to great those visualizations.
#
##############################################################################
#
###################### a ggplot2 template ####################################
#
#   ggplot(data = <DATAFRAME>) +
#      <GEOM_FUNCTION>(mapping = aes(<MAPPINGS))
#
#   We substitute DATAFRAME, MAPPINGS, AND MAPPINGS with ggplot2 stuff
#
ggplot(data = my_data, aes(x=sch_name, y=mean_scl)) + 
  geom_bar(stat="identity", fill="steelblue", width = 0.5) +
  theme_minimal() +
  coord_cartesian(ylim = c(425, 575)) +
  ggtitle("Comparison of a Specific Florida School to State \n and County Results") +
  xlab(NULL) + 
  ylab("Mean Scale Score")

#   I would like to have more data visualizations, but for my initial analysis,
#   this one, based on Mean Scale Score, will do.
#
#   6) Create a github repository for this project.
#
#     If I am serious about developing as a data analyst, I have to get used
#     to working with git and github.  The 'git' program is version control
#     software, and github allows for collaboration or receiving and giving
#     help from others.

