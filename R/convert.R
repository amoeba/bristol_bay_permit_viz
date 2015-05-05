#' convert.R
#' Convert CSV to JSON
#' Format is:
#' nodes : [
#'   { name, group }],
#' links : [
#'   {source, target, value}]

library(stringr)
library(jsonlite)


# Read in permits csv
permits <- read.csv("data/Permit_Download.csv")
nrow(permits)

# First row is bogus
permits <- permits[-1, ]

# Add full name (unique ID)
permits$full_name <- paste(permits$First.Name, permits$Middle, permits$Last.Name)
# And trim them of whitespace
permits$full_name <- str_trim(permits$full_name) 

# Create nodes and links in two passes
# First pass: Nodes
# Second pass: Links


# Create central nodes (last name)
nodes <- data.frame(last_name = unique(permits$Last.Name),
                    full_name = NA,
                    color = 0)

# Create child nodes and their links
links <- data.frame()

for (i in 1:nrow(nodes)) {
  cat(paste0(i,"\n"))
  
  # Collect people with same last name
  same_last_name <- subset(permits, Last.Name == nodes[i,"last_name"])
  
  # Iterate over each person with that last name
  if(nrow(same_last_name) > 0) {
    for (j in 1:nrow(same_last_name)) {
      # Create a node for them
      nodes <- rbind(nodes,
                     data.frame(last_name = nodes[i,"last_name"],
                                full_name = same_last_name[j,"full_name"],
                                color = 1))
      
      # Create a link for them
      links <- rbind(links,
                     data.frame(
                       "source" = i - 1,
                       "target" = nrow(nodes) - 1))
    }
  }
}

output_csv <- file("./permits.json")
writeLines(toJSON(list("nodes" = nodes, "links" = links), pretty = TRUE), output_csv)
close(output_csv)
