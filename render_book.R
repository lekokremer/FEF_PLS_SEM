# render_book.R
# This script renders the bookdown project.

# Load necessary library
library(bookdown)

# Render the book
render_book("index.Rmd")

# To update bookdown link, go to terminal and navigate to 
#.../models_analysis/PLS_SEM/PLS_SEM_bookdown
# git add .
# git commit -m 'dateAMPM'
# git push origin main
