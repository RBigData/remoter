hexSticker::sticker("remoter.png", package="remoter",
  p_size = 7, p_y = 1.6, s_x = 1, s_y = 0.85, s_width = 0.65,
  h_fill = "#448cff", h_color = "#2d3542",
  filename = "remoter-hex.pdf")

system("convert -density 90 -trim remoter-hex.pdf -quality 100 -flatten -sharpen 0x1.0 remoter-hex.png")
