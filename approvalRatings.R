library(rvest)
library(lubridate)
library(tabulizer)

gallup <- function() {
    data <- read_html('http://www.gallup.com/poll/201617/gallup-daily-trump-job-approval.aspx') %>%
    html_nodes("table") %>%
    html_table()
    data <- data[[1]]
    result <- data[1,"% Approve"]
    result
}

ras <- function() {
    data <- read_html('http://www.rasmussenreports.com/public_content/politics/trump_administration/trump_approval_index_history') %>%
    html_nodes("table") %>%
    html_table()
    data <- data[[1]]
    result <- data[1,"Total Approve"]
    result
}

reuters <- function() {
    if(wday(Sys.Date()) != 4) {
        date <- Sys.Date() - wday(Sys.Date() + 3)
        url <- sprintf("https://www.ipsos.com/sites/default/files/2017-06/Core%%20Political-Topline-%s.pdf", date)
        table <- extract_tables(url, pages=8)
        table <- table[[1]]
        rownames(table) <- table[,1]
        table <- table[,-1]
        result <- unname(table["TOTAL APPROVE", 1])
        result
    }
    else {
        date <- Sys.Date()
        url <- sprintf("https://www.ipsos.com/sites/default/files/2017-06/Core%%20Political-Topline-%s.pdf", date)
        table <- extract_tables(url, pages=8)
        table <- table[[1]]
        rownames(table) <- table[,1]
        table <- table[,-1]
        result <- unname(table["TOTAL APPROVE", 1])
        result
    }
}

yougov <- function() {
    page <- read_html("https://today.yougov.com/news/categories/politics/")
    pdfs <- html_nodes(page, ".pdf-title")
    target <- pdfs[grepl("Weekly Political", html_text(pdfs)) == TRUE] %>%
    html_attr("href")

    table <- extract_tables(target, pages=2)
    table <- table[[1]]
    result <- table[17,2]
    result
}

quin <- function() {
    page <- read_html("https://poll.qu.edu/national/")
    links <- html_nodes(page, ".resizeContent a")
    url <- "https://poll.qu.edu"
    pdf <- ""
    for(i in links) {
        newurl <- html_attr(i, "href")
        newurl <- paste(url, newurl, sep='')
        newpage <- read_html(newurl)
        found <- grepl("Do you approve or disapprove of the way Donald Trump is handling his job as President", newpage)
        if(found == TRUE) {
            newlinks <- html_nodes(newpage, ".PollPDF")
            pdf <- html_attr(newlinks[1], "href")
            break
        }
    }
    table <- extract_tables(pdf, pages=4)
    table <- table[[1]]
    result <- table[1,2]
    result
}

ibd <- function() {
    page <- read_html("http://www.investors.com/politics/ibdtipp-poll-presidential-approval-direction-of-country/")
    pdf <- html_nodes(page, "p:nth-child(8) a") %>%
    html_attr("href")

    table1 <- extract_tables(pdf, pages=1:5)
    table1 <- table1[grep("Table Q9",table1)]
    table1 <- table1[[1]]
    pagenum <- table1[table1[,1]=="Table Q9",2]
    pagenum = as.numeric(sub('Page\ ','',pagenum))
    
    #To be honest at this point it's just educated guessing, the list at the 
    #beginning of the PDF is of variable length between publications, and their 
    #page count is based on Page 1 being the first page after the initial list, 
    #not the first page of the PDF file. 
    #I've seen this list between 3 and 5 pages long, so it's a little overboard 
    #here, but at least it's reasonably faster than extracting every single table 
    #in the sometimes >100 page PDF
    table2 <- extract_tables(pdf, pages = (min(pagenum) + 3):(min(pagenum) + 10))
    table2 <- table2[grepl("Approve", table2)]
    table2 <- table2[[1]]
    result <- table2[which(table2 == "Approve") + 1, 2]
    result
}
