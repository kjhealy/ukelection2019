library(tidyverse)
library(rvest)

parse_results <- function(party) {

    party_name <- party %>%
        html_nodes(".ge2019-constituency-result__party-name") %>%
        html_text()

    candidate <- party %>%
        html_nodes(".ge2019-constituency-result__candidate-name") %>%
        html_text()

    keys <- party %>%
        html_nodes(".ge2019-constituency-result__details-key") %>%
        html_text() %>%
        janitor::make_clean_names()

    values <- party %>%
        html_nodes(".ge2019-constituency-result__details-value") %>%
        html_text() %>%
        str_remove_all(",") %>%
        str_remove_all("\\+") %>%
        as.numeric()

    names(values)  <- keys

    out <- tibble(party_name, candidate, keys, values) %>%
        pivot_wider(names_from = keys,
                    values_from = values)
    out
}


parse_constituency <- function(page) {
    constituency <- page %>%
        html_nodes(".constituency-title__title") %>% html_text()

    electorate <-  page %>%
        html_nodes(".ge2019-constituency-result-turnout__electorate") %>%
        html_nodes(".ge2019-constituency-result-turnout__value") %>%
        html_text() %>%
        str_remove_all(",") %>%
        as.integer()

    parties <- page %>%
        html_nodes(".ge2019-constituency-result__list") %>%
        html_nodes(".ge2019__party--border")

    results <- map_df(parties, parse_results)
    out <- data.frame(constituency, electorate, results)
    as_tibble(out)
}


get_first_name <-  function(x){
    sp <- stringr::str_split(x, " ")
    fname <- sapply(sp, "[", 1)
    return(fname)
}

get_last_name <-  function(x){
    sp <- stringr::str_split(x, " ")
    fname <- unlist(lapply(sp, tail, 1))
    return(fname)
}


## Convenience function to check color labels
pal <- function(col, border = "light gray")
{
  n <- length(col)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1), axes=FALSE, xlab = "", ylab = "")
  rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}


uk_colors <- tibble(party =
                        c("Conservative",
                          "Labour",
                          "SNP",
                          "DUP",
                          "Lim-Dems",
                          "Sinn Fein",
                          "Plaid Cymru",
                          "SDLP",
                          "UUP",
                          "Greens",
                          "Independent",
                          "UKIP"),
                        party_color=c(
                            "#1577C7", # Conservative
                            "#E8251F", # Labour
                            "#EAC135", # SNP
                            "#BC1D40", # DUP
                            "#FA8324", # Lim-Dems
                            "#126140", # Sinn Fein
                            "#559D37", # Plaid Cymru
                            "#6AA769", # SDLP
                            "#6EB2E4", # UUP
                            "#7EC031", # Greens
                            "#999999", # Independent
                            "#6E3485" # UKIP
                            ),
                        stringsAsFactors = FALSE)

pal(uk_colors$party_color)


## Look up party colors as needed
pc_lookup <- function(parties){
    x <- match(parties, uk_colors$party)
    colors <- uk_colors$party_color[x]
    return(colors)
}


###--------------------------------------------------
### Read the constitutencies and their URLS
###--------------------------------------------------

constituencies <- read_html("http://www.bbc.com/news/politics/constituencies") %>%
    html_nodes(".az-table__row th")

constituency_names <- constituencies %>%
    html_text()

urls <- constituencies %>%
    html_nodes("a") %>% html_attrs() %>%
    paste("http://bbc.com", ., sep="")

names(urls) <- constituency_names

## The BBC URL is also the topoJSON identifier; useful as an ID string
cid <- stringr::str_remove(urls, "http://bbc.com/news/politics/constituencies/")

con_table <- tibble(cid = cid, constituency = constituency_names)

## We should save this.
write_csv(con_table, path = "data-raw/constituency-ids.csv")


###--------------------------------------------------
### Scrape the results from each constituency page
###--------------------------------------------------

constituency_pages <- urls %>%
  map(~ {
    message(glue::glue("* parsing: {.x}"))
    Sys.sleep(5) # try to be polite
    safely(read_html)(.x)
  })

###--------------------------------------------------
### Save all the pages locally
###--------------------------------------------------

## Drop the safely() error codes from the initial scrape, and
## and also drop any NULL entries
page_list <- pluck(constituency_pages, "result") %>%
  compact()

names(page_list) <- stringr::str_remove(names(page_list), "data-raw/constituencies")

## Make a vector of clean file names of the form "data-raw/constituencies/constituency_name.html"
## One for every constituency. Same order as the constituency_pages list.
fnames <-paste0(here(), "/data-raw/constituencies/",
                janitor::make_clean_names(names(page_list)))

fnames <- str_replace(fnames, "_html", ".html")

## Walk the elements of the page list and the file names to
## save each HTML file under is respective clean file name
walk2(page_list, fnames, ~ write_xml(.x, file = .y))


### Now henceforth you can just do this:

## The filenames we just created
local_urls <- fs::dir_ls("data-raw/constituencies/")


constituency_pages <- local_urls %>%
  map(~ {
    message(glue::glue("* parsing: {.x}"))
    safely(read_html)(.x)
  })

page_list <- pluck(constituency_pages, "result") %>%
  compact()


###--------------------------------------------------
### Parse the results from each constituency
###--------------------------------------------------

df <- map_df(page_list, parse_constituency) %>%
    mutate(votes = as.integer(votes))

df %>% filter(constituency == "Chorley")
df$party_name[df$candidate == "Lindsay Hoyle"] <- "Conservative"


df <- df %>%
    group_by(constituency) %>%
    mutate(total_votes_cast = sum(votes),
           vrank = row_number(desc(votes)),
           turnout = total_votes_cast / electorate)


by_seats <- df %>%
    group_by(constituency) %>%
    filter(votes==max(votes)) %>%
    group_by(party_name) %>%
    tally() %>%
    arrange(desc(n))

mps <- df %>% group_by(constituency) %>%
    filter(votes==max(votes))  %>%
        ungroup() %>%
        arrange(desc(vote_share_percent))


df <- df %>%
    group_by(candidate) %>%
    mutate(fname = get_first_name(candidate),
           lname = get_last_name(candidate))

df <- left_join(df, con_table)
df <- df %>% select(cid, constituency, everything())


ukvote2019 <- ungroup(df)

usethis::use_data(ukvote2019, overwrite = TRUE)


## drat::insertPackage("../ukelection2019_0.0.0.9000.tar.gz", "/Users/kjhealy/Documents/source/drat", commit = TRUE)

not_gb <- c("Democratic Unionist Party",
            "Sinn Fein", "Social Democratic & Labour Party",
            "Ulster Unionist Party", "Independent")
gb_colors <- uk_colors %>% filter(party %nin% not_gb)


exclude <- c("Democratic Unionist Party", "Sinn Fein",
             "Social Democratic & Labour Party",
            "Ulster Unionist Party")



mps$Shire <- str_detect(mps$constituency, "shire")
mps$Field <- str_detect(mps$constituency, "field")
mps$Dale <- str_detect(mps$constituency, "dale")
mps$Pool <- str_detect(mps$constituency, "pool")
mps$Ham <- str_detect(mps$constituency, "(ham$)|(ham )")
mps$Ton <- str_detect(mps$constituency, "(ton$)|(ton )")
mps$Wood <- str_detect(mps$constituency, "(wood$)|(wood )")
mps$Saint <- str_detect(mps$constituency, "(St )|(Saint)")
mps$Port <- str_detect(mps$constituency, "(Port)|(port)")
mps$Ford <- str_detect(mps$constituency, "(ford$)|(ford )")
mps$By <- str_detect(mps$constituency, "(by$)|(by )")
mps$Boro <- str_detect(mps$constituency, "(boro$)|(boro )|(borough$)|(borough )")
mps$Ley <- str_detect(mps$constituency, "(ley$)|(ley )|(leigh$)|(leigh )")


by_name <- mps %>%
    filter(party_name %in% c("Conservative", "Labour", "Green", "Liberal Democrat")) %>%
    select(constituency, party_name, Shire:Ley)

by_name <- gather(by_name, Name, Present, Shire:Ley)

by_name <- by_name %>% group_by(Name, party_name) %>%
    summarize(count = sum(Present)) %>%
        mutate(freq = round(count/sum(count)*100, 1)) %>%
            group_by(Name) %>%
                mutate(total=sum(count)) %>%
                    group_by(Name, party_name) %>%
                        filter(count>0) %>%
                            ungroup() %>% arrange(desc(total))


tmp <- by_name %>% filter(party_name == "Conservative") %>% select(Name, freq)
ind <- match(by_name$Name, tmp$Name)
by_name$con_tot <- tmp$freq[ind]

by_name <- by_name %>% filter(party_name %in% c("Conservative", "Labour") & total>9)


p <- ggplot(by_name, aes(x=reorder(as.character(Name), con_tot, order=TRUE), y=freq, fill=party_name))
p + geom_bar(stat="identity", position="stack") +
    scale_fill_manual(values=pc_lookup(c("Conservative", "Labour"))) +
    coord_flip() +
    theme(legend.position="top") +
    guides(fill = guide_legend(label.position = "bottom", title.position = "top",
                               keywidth = 3, title.hjust = 0.5)) +
    labs(x=NULL,
         y="Percent of Seats Won",
         fill="Winning Party",
         title = "Politics and the English Landscape, 2019:\nConservatives vs Labour Wins by Constituency Toponym",
         caption  = "English Constituencies only. Toponyms found in 10 or more Constituencies shown.\n Non-exclusive categories. 'Ley' includes 'Leigh'. Kieran Healy :: http://kieranhealy.org")


party_fn <- df %>%
    group_by(party_name, fname) %>%
    tally() %>%
    mutate(pct = round(n/sum(n)*100,1)) %>%
    arrange(desc(pct)) %>%
    filter(cume_dist(desc(pct))<0.05) %>%
    filter(party_name %in% c("Conservative",
                        "Labour",
                        "Liberal Democrat"))


p <- ggplot(data = party_fn,
            mapping = aes(x = n, y = reorder(fname, n)) +
           geom_point() +
           facet_wrap(~ party_name, scales = "free_y")

party_lnames <- df %>%
    group_by(party_name, lname) %>%
    tally() %>%
    mutate(pct = round(n/sum(n)*100,1)) %>%
    arrange(desc(pct)) %>%
    filter(cume_dist(desc(pct))<0.05) %>%
    filter(party_name %in% c("Conservative",
                        "Labour",
                        "Liberal Democrat"))




