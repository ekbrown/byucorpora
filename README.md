# byucorpora

NOTE 2018-03-06: Many thanks to Joseph Flanagan for the pull request that fixed the bugs caused by the interface update in May 2016. Also, if you know (some) Python, take a look at some of my [Python scripts](https://github.com/ekbrown/python_scripts), some of which use [Selenium](https://www.seleniumhq.org/), to do some of the things that this R package does.

The purpose of this R package is to webscrape keyword-in-context (KWIC) results of some of the Brigham Young University (BYU) [corpora](http://corpus.byu.edu/), created by [Mark Davies](http://davies-linguistics.byu.edu/personal/).

## Install
Copy and paste the following into the R console:
> install.packages("devtools")  
> library("devtools")  
> install_github("ekbrown/byucorpora")  
> library("byucorpora")  

## Set email and password
Before using the search functions to queries the corpora, the function `set_credentials` must be called to set the email and associated password for the current R session. If users aren't sure which email they used when registering for the BYU corpora, they can visit [corpus.byu.edu](http://corpus.byu.edu/login.asp) in order to figure it out. The function `get_credentials` returns the email currently set to be used for queries.

## Search functions

### Search the Corpus of Contemporary American English (COCA)
The function `search_coca` retrieves keyword-in-context results from the COCA. The argument `search_terms` takes a string, or a vector of strings, to be searched for, for example, `search_terms = "dude"` or `search_terms = c("dude", "narly")`. It should be noted that the COCA doesn't handle apostrophes in a way one might expect. For example, users can't directly search for `"I don't know"`, instead, they must transform it to `"I do n't know"`. If users have apostrophes in a search string, they should take a minute to manually search for it in a web browser to work out the exact syntax before specifying it in this function. The argument `section` specifies the section or sections of the COCA to search in. Options include `"all"` for all sections, `"spok"` for the spoken section (the default), `"fict"` for fiction, `"mag"` for magazine, `"news"` for newspaper, `"acad"` for academic. The argument `max_type` takes an integer and specifies the maximum number of unique types to return for each search string (results shown in the upper right portion of the COCA in a web browser). For example, searching for nouns with the search string `"[n*]"` could potentially return tens of thousands of unique types, but a user may only be interested in the 100 most frequent ones. The argument `max_per_term` specifies the maximum number of results to return for each search string. The argument `max_total_result` specifies the specifies the maximum number of total results to return. This number should be equal to or smaller than users' daily keyword-in-context view limit imposed by their category of researcher in their profile with the BYU corpora.

### Search the Corpus of Historical American English (COHA)
The function `search_coha` retrieves keyword-in-context results from the COHA. The argument `search_terms` takes a string, or a vector of strings, to be searched for, for example, `search_terms = "erstwhile"` or `search_terms = c("erstwhile", "ere")`. It should be noted that the COHA doesn't handle apostrophes in a way one might expect. For example, users can't directly search for `"I don't know"`, instead, they must transform it to `"I do n't know"`. If users have apostrophes in a search string, they should take a minute to manually search for it in a web browser to work out the exact syntax before specifying it in this function. The argument `section` specifies the section or sections of the COHA to search in, from among `section = "all"` for all sections, `"fict"` for the fiction section (the default), `"mag"` for magazine, `"news"` for newspaper, `"nf"` for NF Books. Also, a specific decade can be specified by the first year in the decade, for example, `1920`. A combination of genres and/or decades can be specified in a vector, for example, `section = c("1850", "1950")` or `section = c("mag", "news")`. The arguments `max_type`, `max_per_term`, and `max_total_result` have the same functionality as in `search_coca`.

### Search the British National Corpus (BNC)
The function `search_bnc` retrieves keyword-in-context results from the BNC-BYU. The argument `search_terms` takes a string, or a vector of strings, to be searched for, for example, `search_terms = "cheers"` or `search_terms = c("cheers", "bye")`. It should be noted that the BNC-BYU doesn't handle apostrophes in a way one might expect. For example, users can't directly search for `"I don't know"`, instead, they must transform it to `"I do n't know"`. If users have apostrophes in a search string, they should take a minute to manually search for it in a web browser to work out the exact syntax before specifying it in this function. The argument `section` specifies the section or sections of the BNC to search in, from among `section = "all"` for all sections, `"spok"` for the spoken section (the default), `"fict"` for the fiction section, `"mag"` for magazine, `"news"` for newspaper, `"non-acad"` for non-academic, `"acad"` for academic, and `"misc"` for miscellaneous. A combination of genres can be specified in a vector, for example, `section = c("mag", "news")`. The arguments `max_type`, `max_per_term`, and `max_total_result` have the same functionality as in `search_coca`.

### Search the Corpus del Español (CDE)
The function `search_cde` retrieves keyword-in-context results from the CDE. The argument `search_terms` takes a string, or a vector of strings, to be searched for, for example, `search_terms = "mujer"` or `search_terms = c("mujer", "hombre")`. The argument `section` specifies the section or sections of the CDE to search in, specified by a one-hundred-year period or periods, for example, `section = "1900s"` or `section = c("1900s", "1800s")`. Specific genres of the 1900s can be specified with `"oral"` for the oral section, `"fict"` for fiction, `"news"` for news, and `"acad"` for academic. Additionally, a combination of genres and/or one hundred year periods can be specified, for example, `c("1200s", "1500s", "1800s")` or `c("news", "acad")`. The arguments `max_type`, `max_per_term`, and `max_total_result` have the same functionality as in `search_coca`.

#### Output of search functions
The output of the search functions is a data frame. This data frame, as with any data frame, can be viewed by typing the name of the data frame at the console, or by using `View(dfname)` (where `dfname` is replaced by the actual variable name). Further, the data frame can be saved to the hard drive of the user's computer (for example, with `write.table` or `readr::write_csv`) and then imported into a spreadsheet for further analysis.
