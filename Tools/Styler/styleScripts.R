library(styler)
rm(list = ls())

# Input
script.dir = "../../Scripts"

# Load
script.files = list.files(path = script.dir, pattern = ".*.R$", full.names = T, recursive = T)

# Calculate
for(script.file in script.files){
  test = style_file(script.file, style = tidyverse_style, scope = "tokens", strict = TRUE, indent_by = 2, start_comments_with_one_space = TRUE)
}
