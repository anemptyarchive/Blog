
# ggtext::geom_textbox関数 -------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(ggtext)

# チェック用
library(ggplot2)


?ggtext::geom_textbox()


### ・デフォルト -----

# ダミーのデータフレーム
dummy_df <- tidyr::tibble(v = 0)

# デフォルトのラベルを確認
ggplot(data = dummy_df) + 
  ggtext::geom_textbox(x = 0, y = 0, label = "label text") + # ラベル
  coord_equal() + # アスペクト比
  lims(x = c(-1, 1), y = c(-1, 1)) + # 表示範囲
  labs(title = "ggtext::geom_textbox()")


### ・width -----

# 文字列を指定
char_df <- tidyr::tibble(y = 1:26) |> 
  dplyr::group_by(y) |> 
  dplyr::mutate(label = stringr::str_flatten(LETTERS[1:y])) |> # 確認用ラベル
  dplyr::ungroup()

# 横サイズを確認
ggplot(data = char_df, mapping = aes(y = y, label = label)) + 
  ggtext::geom_textbox(x = 0) + # デフォルト
  ggtext::geom_textbox(x = 1, width = unit(3, units = "inch")) + # 横サイズ指定
  ggtext::geom_textbox(x = 2, hjust = 0.5, width = NULL) + # 横サイズ可変・中央揃え
  ggtext::geom_textbox(x = 3, hjust = 0, width = NULL) + # 横サイズ可変・左揃え
  scale_x_continuous(breaks = 0:3, 
                     labels = c("default", 'width = unit(3, "inch")', "width = NULL, hjust = 0.5", "width = NULL, hjust = 0"), 
                     limits = c(-0.5, 4)) + 
  labs(title = "ggtext::geom_textbox()")


### ・height -----

# 文字列を指定
char_df <- tidyr::tibble(x = 1:10) |> 
  dplyr::group_by(x) |> 
  dplyr::mutate(label = stringr::str_c(LETTERS[1:x], collapse = "<br>")) |> # 確認用ラベル
  dplyr::ungroup()

# 縦サイズを確認
ggplot(data = char_df, mapping = aes(x = x, label = label)) + 
  ggtext::geom_textbox(y = 0, width = NULL) + # デフォルト
  ggtext::geom_textbox(y = 1, width = NULL, height = unit(1, units = "inch")) + # 縦サイズ指定
  ggtext::geom_textbox(y = 2, vjust = 1, width = NULL, height = NULL) + # 縦サイズ可変・上揃え
  scale_y_reverse(breaks = 0:-2, 
                     labels = c("default", 'width = unit(1, "inch")', "height = NULL, hjust = 1"), 
                     limits = c(-3, 0.5)) + 
  labs(title = "ggtext::geom_textbox()")


### ・halign, valign -----

# 揃え位置を指定
align_df <- tidyr::expand_grid(
  h = c(0, 0.5, 1), 
  v = c(0, 0.5, 1)
) |> # 格子点を作成
  dplyr::mutate(
    x = h, 
    y = v, 
    label = paste0(
      "halign = ", stringr::str_pad(h, width = 3, side = "right"), "<br>", 
      "valign = ", stringr::str_pad(v, width = 3, side = "right")
    ) # 確認用ラベル
  )

# 揃え位置を確認
ggplot(data = align_df, mapping = aes(x = x, y = y)) + 
  ggtext::geom_textbox(mapping = aes(label = label, halign = h, valign = v), 
                       width = unit(5, units = "lines"), height = unit(5, units = "lines")) + 
  geom_point(color = "red", size = 3) + # プロット位置を確認
  lims(x = c(-0.5, 1.5), y = c(-0.5, 1.5)) + # 表示範囲
  labs(title = "ggtext::geom_textbox()")


### ・box.margin -----

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
  lims(x = c(-1, 1), y = c(-1, 1)) + # 表示範囲
  #lims(x = c(-10, 10), y = c(-10, 10)) + # 表示範囲
  labs(title = 'ggtext::geom_textbox(box.margin = unit(c(top, right, bottom, left), "lines"), width = NULL, height = NULL)')
for(i in 1:nrow(margin_df)) {
  # 値ごとにラベルを重ねて描画
  g <- g + 
    ggtext::geom_textbox(data = margin_df[i, ], mapping = aes(label = label), 
                         x = 0, y = 0, hjust = 0.5, vjust = 0.5, 
                         width = NULL, height = NULL, 
                         box.margin = unit(as.numeric(margin_df[i, c("top", "right", "bottom", "left")]), "lines"))
}
g <- g + 
  geom_point(data = dummy_df, x = 0, y = 0, color = "red", shape = 13, size = 13) # プロット位置を確認
g


# ダミーのデータフレーム
dummy_df <- tidyr::tibble(v = 0)

# ラベルサイズと余白の関係を確認
ggplot(data = dummy_df) + 
  ggtext::geom_textbox(label = 'width = unit(25, "lines")<br>height = unit(20, "lines")<br>box.margin = unit(c(0, 0, 0, 0), "lines")', 
                       x = 0, y = 0, hjust = 0.5, vjust = 0.5, halign = 0.5, valign = 1, 
                       width = unit(25, units = "lines"), height = unit(20, units = "lines"), 
                       box.margin = unit(c(0, 0, 0, 0), units = "lines"), 
                       color = "red", fill = NA, box.size = unit(1, units = "lines")) + # 余白なし
  ggtext::geom_textbox(label = 'width = unit(25, "lines")<br>height = unit(20, "lines")<br>box.margin = unit(c(5, 5, 5, 5), "lines")', 
                       x = 0, y = 0, hjust = 0.5, vjust = 0.5, halign = 0.5, valign = 0.5, 
                       width = unit(25, units = "lines"), height = unit(20, units = "lines"), 
                       box.margin = unit(c(5, 5, 5, 5), units = "lines"), 
                       color = "gold", box.size = unit(1, units = "lines")) + # 余白あり
  lims(x = c(-1, 1), y = c(-1, 1)) + # 表示範囲
  labs(title = 'ggtext::geom_textbox(box.margin = unit(c(top, right, bottom, left), "lines"))')


### ・orientation引数 -----

# 表示角度を指定
angle_df <- tibble::tibble(
  x = c(0, 0, 1, 1), 
  y = c(0, 1, 0, 1), 
  angle = c("left-rotated", "upright", "inverted", "right-rotated"), 
  label = paste0('orientation = "', angle, '"') # 確認用ラベル
)

# 表示角度を確認
ggplot() + 
  ggtext::geom_textbox(data = angle_df, mapping = aes(x = x, y = y, label = label, orientation = angle), 
                       width = NULL, box.padding = unit(rep(2, times = 4), units = "lines")) + 
  lims(x = c(-0.5, 1.5), y = c(-0.5, 1.5)) + # 表示範囲
  theme(axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        panel.grid.minor = element_blank()) + # 軸線を非表示化
  labs(title = 'ggtext::geom_textbox()')


