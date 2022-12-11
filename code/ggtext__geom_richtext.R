
# ggtext::geom_richtext関数 -------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(ggtext)

# チェック用
library(ggplot2)


?ggtext::geom_richtext()


### ・デフォルト -----

# ダミーのデータフレーム
dummy_df <- tidyr::tibble(v = 0)

# デフォルトのラベルを確認
ggplot(data = dummy_df) + 
  ggtext::geom_richtext(x = 0, y = 0, label = "label text") + # ラベル
  coord_equal() + # アスペクト比
  lims(x = c(-1, 1), y = c(-1, 1)) + # 表示範囲
  labs(title = "ggtext::geom_richtext()")

# デフォルトのラベルを確認
ggplot(data = dummy_df) + 
  ggtext::geom_richtext(x = 0, y = 0, label = "label text", 
                        fill = NA, label.color = NA) + # ラベル
  coord_equal() + # アスペクト比
  lims(x = c(-1, 1), y = c(-1, 1)) + # 表示範囲
  labs(title = "ggtext::geom_richtext()")


### ・label -----

# ラベルテキストを指定
label_df <- tidyr::tibble(
  str = c("label text", "label\ntext", "label<br>text"), 
  label = paste0('label = "', stringr::str_replace(str, pattern = "\n", replacement = "\\\\n"), '"'), 
  y = dplyr::row_number(label)
)

# ラベルテキストを確認
ggplot(data = label_df, mapping = aes(y = y)) + 
  ggplot2::geom_text(mapping = aes(label = label, x = 0)) + 
  ggplot2::geom_label(mapping = aes(label = str, x = 1)) + 
  ggtext::geom_richtext(mapping = aes(label = str, x = 2)) + 
  scale_y_reverse() + # y軸を判定
  scale_x_continuous(breaks = 0:2, 
                     labels = c("argment", "ggplot2::geom_label()", "ggtext::geom_richtext()"), 
                     limits = c(-0.5, 2.5)) + # x軸目盛ラベルを指定
  theme(axis.title = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        panel.grid.minor.y = element_blank()) # 軸線を非表示化


### ・label.color -----

# プロット位置を指定
point_df <- tidyr::tibble(x = 0)

# 色を指定
ggplot(data = point_df, mapping = aes(x = x)) + 
  ggtext::geom_richtext(y = 0, label = 'color = "red", fill = "yellow"', 
                        color = "red", fill = "yellow", 
                        label.padding = unit(0.8, units = "lines"), label.size = 2) + 
  ggtext::geom_richtext(y = 1, label = 'color = "red", fill = "yellow", label.color = "wheat1"', 
                        color = "red", fill = "yellow", label.color = "wheat1", 
                        label.padding = unit(0.8, units = "lines"), label.size = 2) + 
  ggtext::geom_richtext(y = 2, label = 'text.color = "red", label.color = "wheat1", fill = "yellow"', 
                        text.color = "red", label.color = "wheat1", fill = "yellow", 
                        label.padding = unit(0.8, units = "lines"), label.size = 2) + 
  theme(axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + # 軸線を非表示化
  lims(x = c(-0.5, 0.5), y = c(-0.5, 2.5)) + # 表示範囲
  labs(title = "ggtext::geom_richtext()")


### ・label.padding -----

# 4方向の値を指定
pad_df <- tidyr::expand_grid(
  x = seq(from = -2, to = 2, by = 0.5), 
  y = seq(from = -2, to = 2, by = 0.5)
) |> 
  dplyr::mutate(
    top = dplyr::if_else(y > 0, true = y, false = 0), 
    right = dplyr::if_else(x > 0, true = x, false = 0), 
    bottom = dplyr::if_else(y < 0, true = -y, false = 0), 
    left = dplyr::if_else(x < 0, true = -x, false = 0), 
    label = paste0(
      "c(", 
      stringr::str_pad(top, width = 3, side = "left"), ", ", 
      stringr::str_pad(right, width = 3, side = "left"), ", ", 
      stringr::str_pad(bottom, width = 3, side = "left"), ", ", 
      stringr::str_pad(left, width = 3, side = "left"), 
      ")"
    ) # 確認用ラベル
  )

# 文字列と枠線の間を確認
g <- ggplot() + 
  theme(axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + # 軸線を非表示化
  lims(x = c(min(pad_df[["x"]])-1, max(pad_df[["x"]])+1), 
       y = c(min(pad_df[["y"]])-1, max(pad_df[["y"]])+1)) + # 表示範囲
  labs(title = 'ggtext::geom_richtext(label.padding = unit(c(top, right, bottom, left), "lines"))')
for(i in 1:nrow(pad_df)) {
  # 値ごとにラベルを重ねて描画
  g <- g + 
    ggtext::geom_richtext(data = pad_df[i, ], mapping = aes(x = x, y = y, label = label), 
                          label.padding = unit(as.numeric(pad_df[i, c("top", "right", "bottom", "left")]), units = "lines"))
}
g


### ・label.margin -----

# 4方向の値を指定
margin_df <- tidyr::expand_grid(
  margin_x = seq(from = -40, to = 40, by = 20), 
  margin_y = seq(from = -40, to = 40, by = 10)
) |> 
  dplyr::mutate(
    top = dplyr::if_else(margin_y > 0, true = margin_y, false = 0), 
    right = dplyr::if_else(margin_x > 0, true = margin_x, false = 0), 
    bottom = dplyr::if_else(margin_y < 0, true = -margin_y, false = 0), 
    left = dplyr::if_else(margin_x < 0, true = -margin_x, false = 0), 
    label = paste0(
      "c(", 
      stringr::str_pad(top, width = 3, side = "left"), ", ", 
      stringr::str_pad(right, width = 3, side = "left"), ", ", 
      stringr::str_pad(bottom, width = 3, side = "left"), ", ", 
      stringr::str_pad(left, width = 3, side = "left"), 
      ")"
    ) # 確認用ラベル
  )

# ダミーのデータフレーム
dummy_df <- tidyr::tibble(v = 0)

# ラベル外側の余白を確認
g <- ggplot() + 
  #lims(x = c(-1, 1), y = c(-1, 1)) + # 表示範囲
  lims(x = c(-10, 10), y = c(-10, 10)) + # 表示範囲
  labs(title = 'ggtext::geom_richtext(label.margin = unit(c(top, right, bottom, left), "lines"))')
for(i in 1:nrow(margin_df)) {
  # 値ごとにラベルを重ねて描画
  g <- g + 
    ggtext::geom_richtext(data = margin_df[i, ], mapping = aes(label = label), 
                          x = 0, y = 0, hjust = 0.5, vjust = 0.5, 
                          label.margin = unit(as.numeric(margin_df[i, c("top", "right", "bottom", "left")]), units = "lines"))
}
g <- g + 
  geom_point(data = dummy_df, x = 0, y = 0, color = "red", shape = 13, size = 13) # プロット位置を確認
g


