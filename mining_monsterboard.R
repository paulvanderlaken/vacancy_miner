### VACANCY MINING
### PAUL VAN DER LAKEN
### MONSTERBOARD

library("xml2")
library('rvest')
library('dplyr')
library('jsonlite')

#### SPECIFY SEARCH TERM
search.term = "HR data"

#### RETRIEVE JOB VACANCIES
mb.url.hr.stem <- paste0("https://www.monsterboard.nl/vacatures/zoeken/?q=",
                         search.term,
                         "&cy=nl&page=")

n.pages = 40 # number of overview pages

mb.job.json <- c() # create empty vector

# loop through vacancy overview pages
# and store information into json vector
for(i in seq_len(n.pages)){
  
  url <- paste0(mb.url.hr.stem, i) # set url
  
  cat("Iteration", paste0(i,":"), url, "\n") # print iteration & url

  page <- read_html(url) # read in page
  
  page %>% # extract the json scripts
    html_nodes(xpath = '//*[@id="main"]/script') %>%
    html_text() -> scripts
  cat(length(scripts), "scripts found.", "\n")
  
  if(length(scripts) == 1){
    page %>% # extract the json scripts
      html_nodes(xpath = '//*[@id="main"]/div/script') %>%
      html_text() -> scripts
    cat("-", length(scripts), "scripts found behind divider", "\n")
  }
  
  job.json <- scripts[grepl("JobPosting", scripts)]
  cat(length(job.json), "JobPosting json found.", "\n")
    
  mb.job.json <- c(mb.job.json, job.json)
  cat("Total at", length(mb.job.json), "\n")
  
  cat("\t","Iteration completed.","\n\n")
  
  Sys.sleep(sample(seq(0, 2.5, 0.5), 1)) # add random short break
  if(i %% 10 == 0) Sys.sleep(sample(3:10, 1)) # add long break
}

# transform json vector into dataframe
mb.joblist <- lapply(mb.job.json, function(x) fromJSON(x))


#### RETRIEVE JOB INFORMATION
#### FROM JOB DESCRIPTION WEBSITES
n <- length(mb.joblist) # number of job vacancies
filler <- rep(NA,n)
mb.job.df <- data.frame(title = filler,
                        url = filler,
                        datePosted = filler,
                        dateRetrieved = Sys.Date(),
                        employmentType = filler,
                        specialCommitments = filler,
                        industry = filler,
                        hiringOrganization.name = filler,
                        hiringOrganization.type = filler,
                        jobLocation.type = filler,
                        jobLocation.addressLocality = filler,
                        jobLocation.addressRegion = filler,
                        joblocation.address.postalCode = filler,
                        jobDescription = filler,
                        JSON = filler
                        )


for(i in seq_len(n)){
  # reset all variables
  list <- NULL
  job.json <- NULL
  job.text <- NULL
  page <- NULL

  
  # transfer list information
  list <- mb.joblist[[i]]
  cat("Iteration", paste0(i,":"), list$url, "\n") # print iteration & url
  mb.job.df[i, "url"] <- list$url
  mb.job.df[i, "title"] <- list$title
  mb.job.df[i, "datePosted"] <- list$datePosted
  mb.job.df[i, "employmentType"] <- list$employmentType
  mb.job.df[i, "specialCommitments"] <- list$specialCommitments
  mb.job.df[i, "industry"] <- list$industry
  mb.job.df[i, "hiringOrganization.name"] <- list$hiringOrganization$name
  mb.job.df[i, "hiringOrganization.type"] <- list$hiringOrganization$`@type`
  mb.job.df[i, "jobLocation.type"] <- list$jobLocation$`@type`
  mb.job.df[i, "jobLocation.addressLocality"] <- list$jobLocation$address$addressLocality
  mb.job.df[i, "jobLocation.addressRegion"] <- list$jobLocation$address$addressRegion
  mb.job.df[i, "joblocation.address.postalCode"] <- list$jobLocation$address$postalCode

  # try to read in page
  tryCatch({
    page <- read_html(list$url) # read in page
    
    # extract the json script
    page %>% 
      html_nodes(xpath = '//*[@id="JobViewContent"]/script') %>%
      html_text() -> job.json
    cat(length(job.json), "json scripts found.", "\n")
    mb.job.df[i, "JSON"] <- ifelse(length(job.json) > 0, job.json, NA)
    
    # extract the json scripts
    page %>% 
      html_nodes(xpath = '//*[@id="TrackingJobBody"]') %>%
      html_text() -> job.text
    cat(length(job.text), "job description found.", "\n")
    mb.job.df[i, "jobDescription"] <- ifelse(length(job.text) > 0, job.text, NA)

  }, error = function(e) { 
    cat("\n", "ERROR:", conditionMessage(e),"\n") # print error message
  })
  
  cat("\t","Iteration completed.","\n\n")
  Sys.sleep(sample(seq(0, 2.5, 0.5), 1)) # add random short break
  if(i %% 10 == 0) Sys.sleep(sample(3:10, 1)) # add long break
  if(i %% 100 == 0) Sys.sleep(30) # add very long break
}

write.csv(mb.job.df, paste("data/jobs.monsterboard", search.term, Sys.Date(), "csv", sep = '.'), row.names = F)

