
# magick::image_scale関数 ---------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(magick)


# チェック用
library(ggplot2)


?magick::image_scale


### ・画像の作成 -----

# x軸とy軸の値を作成
x_vals <- seq(from = 1, to = 4, by = 1)
y_vals <- seq(from = 1, to = 3, by = 1)

# 作図用のデータフレームを作成
df <- tidyr::expand_grid(x = x_vals, y = y_vals) |> # 格子点を作成
  dplyr::mutate(z = x + y)

# ヒートマップを作成
g <- ggplot(data = df, mapping = aes(x = x, y = y, fill = z)) + 
  geom_tile(show.legend = FALSE) + # ヒートマップ
  geom_text(mapping = aes(label = paste0("(", x, ", ", y, ")")), color = "white", size = 10) + # 座標ラベル
  scale_x_continuous(breaks = x_vals) + 
  scale_y_continuous(breaks = y_vals) + 
  scale_fill_gradient(low ="gold", high = "red") + # グラデーション
  coord_equal(clip = "off", expand = FALSE) + # アスペクト比
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(size = 20), 
        title = element_text(size = 20)) + # 図の体裁
  labs(title = "Heatmap", 
       subtitle = "(x, y)")
g

# 書き出し先のフォルダパスを指定
dir_path <- "../figure/tmp_folder"

# 画像を書き出し
ggplot2::ggsave(
  filename = paste0(dir_path, "/heatmap.png"), plot = g, 
  width = max(x_vals)*300, height = max(y_vals)*300, units = "px", dpi = 100
)


### ・画像サイズの変換 -----

# 読み込み元のフォルダパスを指定
dir_path <- "../figure/tmp_folder"

# 画像を読み込み
pic_data <- magick::image_read(path = paste0(dir_path, "/heatmap.png"))
pic_data

# 画像情報を取得
pic_info <- magick::image_info(image = pic_data)
pic_info


# 横サイズを指定してリサイズ:アスペクト比固定
pic_scaled_w <- magick::image_resize(image = pic_data, geometry = "1000")
pic_scaled_w |> 
  magick::image_info()

# 縦サイズを指定してリサイズ:アスペクト比固定
pic_scaled_h <- magick::image_resize(image = pic_data, geometry = "x1000")
pic_scaled_h |> 
  magick::image_info()

# 縦横サイズを指定してリサイズ:アスペクト比固定
pic_scaled_wh <- magick::image_resize(image = pic_data, geometry = "1500x1500")
pic_scaled_wh |> 
  magick::image_info()

# 横サイズを指定してリサイズ:アスペクト比変更
pic_scaled_wh <- magick::image_resize(image = pic_data, geometry = "1500x1500!")
pic_scaled_wh |> 
  magick::image_info()


