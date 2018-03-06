### VACANCY MINING
### PAUL VAN DER LAKEN
### JOBBIRD

library("xml2")
library('rvest')
library('dplyr')
library('jsonlite')

#### SPECIFY SEARCH TERM
search.term = "HR"
positiongroup = 17 # relevant HR group on jobbird

#### RETRIEVE JOB VACANCIES
domain = "https://www.jobbird.com"
jb.url.stem <- paste0(domain,
                         "/nl/vacatures/?keyword=",
                         search.term,
                         ifelse(is.null(positiongroup), 
                                "",
                                paste0("&positiongroup=",positiongroup)),
                         "&page=")

# browseURL(paste(jb.url.stem, 1))

n.pages = 200 # number of overview pages
jb.jobs <- data.frame() # create empty vector

for(i in seq_len(n.pages)){
  
  url <- paste0(jb.url.stem, i) # set url
  cat("Iteration", paste0(i,":"), url, "\n") # print iteration & url
  
  page <- read_html(url) # read in page
  
  results <- page %>% # extract the json scripts
    html_nodes(xpath = '//*[@id="search-results"]')
  
  jobs = results %>%
    html_nodes('h3') %>%
    html_nodes('a') %>% 
    html_text()
  cat(length(jobs), "jobs found.", "\n")
  
  href = results %>%
    html_nodes('h3') %>%
    html_nodes('a') %>%
    html_attrs() %>%
    sapply(., function(x) x[names(x) == 'href']) %>%
    paste0(domain, .)
  
  jb.jobs = bind_rows(
    jb.jobs,
    data.frame(jobs, 
               href)
  )
  cat("Total at", nrow(jb.jobs),"\n")
  
  cat("\t","Iteration completed.","\n\n")
  Sys.sleep(sample(seq(0, 2.5, 0.5), 1)) # add random short break
  if(i %% 10 == 0) Sys.sleep(5) # add long break
}


#### RETRIEVE JOB INFORMATION
#### FROM JOB DESCRIPTION WEBSITES
jb.jobs$hosted <- grepl("vacature", jb.jobs$href)
jb.jobs$dateRetrieved <- Sys.Date()

n <- nrow(jb.jobs) # number of job vacancies
filler <- rep(NA,n)

jb.jobs$description <- filler

for(i in seq_len(n)){
  
  # only do if intenral job positing
  if(jb.jobs$hosted[i]){
    
    url <- jb.jobs$href[i]
    
    cat("Iteration", paste0(i,":"), url, "\n") # print iteration & url
    
    page <- NULL
    job.text <- NULL
    
    page <- read_html(url) # read in page
    
    page %>% # extract the json scripts
      html_nodes(css = '#job-text') %>%
      html_text() -> job.text
    
    cat(length(job.text), "job description found.", "\n")
    
    jb.jobs$description[i] <- job.text
    
    cat("\t","Iteration completed.","\n\n")
    Sys.sleep(sample(seq(0, 2.5, 0.5), 1)) # add random short break
  }
  
}

write.csv2(jb.jobs, paste("data/jobs.jobbird", search.term, Sys.Date(), "csv", sep = "."), row.names = F)

