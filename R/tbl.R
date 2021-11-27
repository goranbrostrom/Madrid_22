tbl <- function(tt, caption = "", fs = 11, rownames = FALSE, align = NULL){
    library(kableExtra)
    kbl(tt, caption = caption, booktabs = TRUE, row.names = rownames, 
        format = "latex", align = align) %>%
        row_spec(row = 0, bold = TRUE) %>%
        kable_styling(full_width = FALSE, font_size = fs,
                    position = "center")
}
