
######################################################################
#                 Map schools in Africa using OpenStreetMap data in R
#                 Milos Popovic
#                 2022/11/04
######################################################################
# libraries we need
libs <- c(
    "tidyverse", "stringr", "sf",
    "giscoR", "httr", "XML", "lwgeom"
)

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
    install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

# Montserrat font 
# comment out these lines if you work on Linux or MacOS
sysfonts::font_add_google("Montserrat", "Montserrat")
showtext::showtext_auto()

# 1. DOWNLOAD AFRICA DATA
#------------------------
# Africa URL
url <- paste0("https://download.geofabrik.de/africa.html")
# download standard OSM country files
get_africa_links <- function() {
    # make http request
    res <- httr::GET(url)
    # parse data to html format
    parse <- XML::htmlParse(res)
    # scrape all the href tags
    links <- XML::xpathSApply(parse, path = "//a", XML::xmlGetAttr, "href")
    # make all links and store in a list
    for (l in links) {
        all_links <- paste0(url, links)
    }

    africa_links <- all_links[grepl("latest-free.shp.zip", all_links)] %>%
        stringr::str_remove(".htmlafrica")

    return(africa_links)
}

africa_links <- get_africa_links()

# download files
for (a in africa_links) {
    download.file(a, destfile = basename(a), mode = "wb")
}

# 2. UNZIP AFRICAN PLACES
#------------------------
# unzipping and renaming
main_path <- getwd()

zip_files <- list.files(
    path = main_path,
    pattern = "*.shp", full.names = T
)

new_dir <- "unzipped_africa_pois_osm"

dir.create(path = paste0("../", new_dir))

out_dir <- main_path %>%
    stringr::str_remove("africa") %>%
    paste0(new_dir)

setwd(out_dir) # setwd for rename/remove functions to work

for (z in 1:length(zip_files)) {
    zip_names <- grep("pois_free", unzip(zip_files[z], list = T)$Name,
        ignore.case = T, value = T
    )

    unzip(zip_files[z], files = zip_names, exdir = out_dir, overwrite = F)
    x <- sample(1:length(zip_files), 1, replace = T)
    file_old <- c(list.files(out_dir)) # existing file names
    file_new <- c(paste0(x, "_", file_old))
    file.rename(
        paste0(out_dir, "/", file_old),
        paste0(out_dir, "/", file_new)
    )
    rm(file_old)
    rm(file_new) # Delete vectors from the environment
}

# 3. FILTER AFRICAN SCHOOLS
#--------------------------
get_africa_schools <- function() {
    pois_files <- list.files(
        path = out_dir,
        pattern = "*.shp", full.names = T
    )

    pois_list <- lapply(pois_files, sf::st_read)

    africa_pois <- do.call(rbind, pois_list)

    africa_schools <- africa_pois %>%
        dplyr::filter(fclass == "school")


    return(africa_schools)
}

africa_schools <- get_africa_schools()

# 4. MAP OF AFRICA
#-----------------
# load national map
get_africa_map <- function() {
    africa_map <- giscoR::gisco_get_countries(
        year = "2016",
        epsg = "4326",
        resolution = "3",
        region = "Africa"
    )

    return(africa_map)
}

africa_map <- get_africa_map()

# 5. MAP
#-------
p <- ggplot() +
    geom_sf(
        data = africa_map,
        fill = "transparent", color = "#0FAAB8", size = .1
    ) +
    geom_sf(
        data = africa_schools,
        color = "#B82178", size = .05, fill = "#B82178",
        alpha = .45
    ) +
    theme_minimal() +
    theme(
        text = element_text(family = "Montserrat"), # remove this line if you work on Linux or MacOS
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(
            size = 35, color = "grey90", hjust = 0.25, vjust = 220
        ),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_line(color = "#032326", size = 0),
        panel.grid.minor = element_blank(),
        plot.title = element_text(
            face = "bold", size = 100, color = "grey90", hjust = .25,
            vjust = -100
        ),
        plot.margin = unit(
            c(t = -10, r = -10, b = -10, l = -10), "lines"
        ),
        plot.background = element_rect(fill = "#032326", color = NA),
        panel.background = element_rect(fill = "#032326", color = NA),
        legend.background = element_rect(fill = "#032326", color = NA),
        panel.border = element_blank()
    ) +
    labs(
        x = "©2022 Milos Popovic (https://milospopovic.net) | Data: ©OpenStreetMap contributors",
        y = NULL,
        title = "Primary/secondary schools in Africa",
        subtitle = "",
        caption = ""
    )

ggsave(
    filename = "africa_schools.png",
    width = 8.5, height = 7, dpi = 600, device = "png", p
)
