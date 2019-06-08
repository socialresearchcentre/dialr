
library(hexSticker)

imgurl <- "https://publicdomainvectors.org/download.php?file=Brick_Phone__Arvin61r58.svg"
sticker(imgurl,
        package = "dialr",
        p_x = 0.957, p_y = 1.5, p_size = 24,
        s_x = 1    , s_y = 1,   s_width = 0.25,
        h_fill = "#027FC2", h_color = "#000000",
        p_color = "#000000")

# colours
# "#027FC2" dark 
# "#3896DB" medium
# "#5BB1F8" light

sticker(imgurl,
        package = "dialr",
        p_x = 0.8, p_y = 1.3, p_size = 40, p_family = "serif",
        l_x = 0.8, l_y = 1.3, l_alpha = 0.35,
        s_x = 1.27, s_y = 0.5, s_width = 0.4,
        h_fill = "#5BB1F8", h_color = "#000000",
        p_color = "#000000", spotlight = T, white_around_sticker = T)


img <- "~/Downloads/Bell_telephone_magazine_(1922)_(14569733868).jpg"

sticker(img,
        package = "dialr",
        p_x = 0.6, p_y = 1.31, p_size = 30, p_family = "serif",
        s_x = 1, s_y = 1, s_width = 1,
        h_fill = "#5BB1F8", h_color = "#000000",
        p_color = "#DDDDDD", spotlight = F, white_around_sticker = T)
