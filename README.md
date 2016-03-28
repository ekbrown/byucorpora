# byucorpora
The purpose of this R package is to webscrape keyword-in-context (KWIC) results of the Brigham Young University (BYU) [corpora](http://corpus.byu.edu/), created by [Mark Davies](http://davies-linguistics.byu.edu/personal/). 

## install
> install.packages("devtools")  # if not already on the user's computer
> install_github("ekbrown/byucorpora")

## search functions
The output of the search functions is a data frame. This data frame, as with any data frame, can be viewed with R by typing the name of the variable into which the data frame was saved, or by using View(dfname) (where dfname is replaced with the actual variable name that the user specified). Further, the data frame can be saved to the hard drive of the user's computer and then imported into a spreadsheet for further analysis.
