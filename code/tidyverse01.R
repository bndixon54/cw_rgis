if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse)

set.seed(123)

iris_sub <- as_tibble(iris) %>% 
  group_by(Species) %>% 
  sample_n(3) %>% 
  ungroup()

print(iris_sub)


# graduate student --------------------------------------------------------
#library(swirl)
#install_course_github("sysilviakim", "swirl-tidy")
#swirl()

#2, 3, 4 and 7, 8, 9, 10

#csv_semicolon <- read_csv2(file.path(lesson_dir, "file2.csv")

#attempt: 
#test_delim <- read_delim(file.path(lesson_dir, "file.txt"))

#real answer: 
#test_delim <- read_delim(file.path(lesson_dir, "file.txt"), delim = "|")


#attempt:
#cvs_mess <- read_csv(file.path(lesson_dir, "file.mess.csv"))

#real: 
#csv_mess <- read_csv(file.path(lesson_dir, "file_mess.csv"))

#prompt: Read the same file with {read_csv}, but this time, specify `col_types = cols(.default = "c")` as an argument. Store it to "csv_mess3".
#answer:
#csv_mess3 <- read_csv(file.path(lesson_dir,"file_mess.csv"), col_types = cols(.default = "c"))

#example:
#cat(pipe_1)
#midwest %>%
  #select(county, state, poptotal)

#Question: Try excluding column `PID` from "midwest"
#answer:
  #midwest %>%
  #select(- PID)
  
