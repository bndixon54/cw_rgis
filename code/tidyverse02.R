#install.packages("tidyverse")
#library(tidyverse)

#library(swirl)
#install_course_github("sysilviakim", "swirl-tidy")
#swirl()

#Bailey

##The question that kept getting me into a loop if I answered it wrong: 
##(I went into github to check what the correct answer was and it let me continue with no issue after this)
#Check that there are indeed only five different cut values using {table} on the `cut` column of either dataset.

#Answer: table(diamonds$cut)


#Using the same principle, create from "diamonds" (without using "diamonds_grouped") a summary dataframe that produces the average price by `clarity`. Notice that we are no longer interested in average price by `cut`. Name this summarized value to be `mean_price` as before.

#Answer:  diamonds %>%
#           group_by(clarity) %>%
#           summarize(mean_price = mean(price))

#Tip: once the group-level operations/calculations have been completed, make sure to {ungroup} your dataframes. Sometimes, you may forget that your dataframe is still grouped, creating errors in your calculations.

###Okay, I was able to finish this out without coming across the same issue. This "lesson" was definitely the trickiest so far. 
