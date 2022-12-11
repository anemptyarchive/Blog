
# ggplot2::geom_label関数 ------------------------------------------------------------

# 利用パッケージ
library(tidyverse)

# チェック用
library(ggplot2)


?ggplot2::geom_label()


### ・デフォルト -----

# ダミーのデータフレームを作成
dummy_df <- tidyr::tibble(v = 0)

# デフォルトのラベルを確認
ggplot(data = dummy_df) + 
  geom_label(x = 0, y = 0, label = "label text") + # ラベル
  coord_equal() + # アスペクト比
  lims(x = c(-1, 1), y = c(-1, 1)) + # 表示範囲
  labs(title = "ggplot2::geom_label()")


### ・hjust, vjust -----

# 配置位置を指定
just_df <- tidyr::expand_grid(
  h = c(0, 0.5, 1), 
  v = c(0, 0.5, 1)
) |> # 格子点を作成
  dplyr::mutate(
    x = h, 
    y = v, 
    label = paste0(
      "hjust = ", stringr::str_pad(h, width = 3, side = "right"), "\n", 
      "vjust = ", stringr::str_pad(v, width = 3, side = "right")
    ) # 確認用ラベル
  )

# 配置位置を確認
ggplot(data = just_df, mapping = aes(x = x, y = y)) + 
  geom_label(mapping = aes(label = label, hjust = h, vjust = v)) + 
  geom_point(color = "red", size = 3) + # プロット位置を確認
  coord_equal() + # アスペクト比
  labs(title = "ggplot2::geom_label()")


### ・size -----

# ラベルのサイズを指定
size_df <- tibble::tibble(
  s = seq(from = 0.1, to = 1.5, by = 0.1), 
  y = dplyr::row_number(s), 
  label = paste0("size = ", stringr::str_pad(round(s, digits = 1), width = 3, side = "right")) # 確認用ラベル
)

# ラベルのサイズを確認
ggplot(data = size_df, mapping = aes(y = y, label = label, size = s)) + 
  geom_label(x = 1, hjust = 0, show.legend = FALSE) + # 左揃え
  geom_label(x = 2, hjust = 0.5, show.legend = FALSE) + # 中央揃え
  geom_label(x = 3, hjust = 1, show.legend = FALSE) + # 右揃え
  theme(axis.title = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank()) + # 軸線を非表示化
  scale_x_continuous(breaks = 1:3, 
                     labels = c("hjust = 0", "hjust = 0.5", "hjust = 1"), 
                     limits = c(0.5, 3.5)) + 
  labs(title = "ggplot2::geom_label()")


### ・label.size -----

# 枠線の太さを指定
size_df <- tibble::tibble(
  s = seq(from = 0.1, to = 1.5, by = 0.1), 
  x = 0,
  y = dplyr::row_number(s) - 1, 
  label = paste0("label.size = ", stringr::str_pad(round(s, digits = 1), width = 3, side = "right")) # 確認用ラベル
)

# 枠線の太さを確認
g <- ggplot() + 
  theme(axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + # 軸線を非表示化
  lims(x = c(-0.5, 0.5)) + # 表示範囲
  labs(title = "ggplot2::geom_label()")
for(i in 1:nrow(size_df)) {
  # 値ごとにラベルを重ねて描画
  g <- g + 
    geom_label(data = size_df[i, ], mapping = aes(x = x, y = y, label = label), 
               label.size = size_df[["s"]][i])
}
g


### ・label.padding -----

# 文字列と枠線の間を指定
pad_df <- tibble::tibble(
  pad = seq(from = 0, to = 1.5, by = 0.1), 
  y = dplyr::row_number(pad), 
  label = paste0(
    "label.padding = unit(", 
    stringr::str_pad(round(pad, digits = 1), width = 3, side = "right"), 
    ', "lines")'
  ) # 確認用ラベル
)

# 文字列と枠線の間を確認
g <- ggplot() + 
  theme(axis.title = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank()) + # 軸線を非表示化
  scale_x_continuous(breaks = 1:3, 
                     labels = c("hjust = 0", "hjust = 0.5", "hjust = 1"), 
                     limits = c(0.5, 3.5)) + 
  labs(title = "ggplot2::geom_label()")
for(i in 1:nrow(pad_df)) {
  # 値ごとにラベルを重ねて描画
  g <- g + 
    geom_label(data = pad_df[i, ], mapping = aes(y = y, label = label), 
               x = 1, hjust = 0, 
               label.padding = unit(pad_df[["pad"]][i], units = "lines")) + # 左揃え
    geom_label(data = pad_df[i, ], mapping = aes(y = y, label = label), 
               x = 2, hjust = 0.5, 
               label.padding = unit(pad_df[["pad"]][i], units = "lines")) + # 中央揃え
    geom_label(data = pad_df[i, ], mapping = aes(y = y, label = label), 
               x = 3, hjust = 1, 
               label.padding = unit(pad_df[["pad"]][i], units = "lines")) # 右揃え
}
g


### ・label.r -----

# 角の半径を指定
r_lines_df <- tibble::tibble(
  r = seq(from = 0, to = 2, by = 0.1), 
  y = dplyr::row_number(r), 
  label = paste0(
    "label.r = unit(", 
    stringr::str_pad(round(r, digits = 1), width = 3, side = "right"), 
    ', "lines")'
  )
)
r_points_df <- tibble::tibble(
  r = seq(from = 0, to = 20, by = 1), 
  y = dplyr::row_number(r), 
  label = paste0(
    "label.r = unit(", 
    stringr::str_pad(round(r, digits = 1), width = 3, side = "right"), 
    ', "points")'
  )
)

# 角の半径を確認
g <- ggplot() + 
  theme(axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + # 軸線を非表示化
  lims(x = c(0.5, 2.5)) + # 
  labs(title = "ggplot2::geom_label()")
for(i in 1:nrow(r_lines_df)) {
  # 値ごとにラベルを重ねて描画
  g <- g + 
    geom_label(data = r_lines_df[i, ], mapping = aes(y = y, label = label), x = 1, 
               label.r = unit(r_lines_df[["r"]][i], units = "lines"), label.padding = unit(10, units = "points")) + 
    geom_label(data = r_points_df[i, ], mapping = aes(y = y, label = label), x = 2, 
               label.r = unit(r_points_df[["r"]][i], units = "points"), label.padding = unit(10, units = "points"))
}
g


### ・color, fill -----

# プロット位置を指定
point_df <- tidyr::tibble(x = 0, y = 0)

# 色を指定
ggplot() + 
  geom_label(data = point_df, mapping = aes(x = x, y = y), 
             label = 'fill = "yellow", color = "red"', 
             fill = "yellow", color = "red", 
             label.padding = unit(0.8, "lines"), label.size = 2) + 
  labs(title = "ggplot2::geom_label()")


# 線と塗りつぶしの色を指定
color_df <- tibble::tibble(
  x = 0, 
  y = 1:3, 
  color = c("blue", "yellow", "green"), 
  fill = c("red", "gold", "white"), 
  label = paste0(
    "color = ", stringr::str_pad(color, width = 6, side = "right"), "\n", 
    "    fill = ", stringr::str_pad(fill, width = 6, side = "right")
  ) # 確認用ラベル
)

# ラベルの色を確認:(失敗)
ggplot() + 
  geom_label(data = color_df, mapping = aes(x = x, y = y, label = label, color = color, fill = fill), 
             size = 5, label.padding = unit(1, "lines"), label.size = 2) + 
  theme(axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + # 軸線を非表示化
  lims(x = c(-0.5, 0.5), y = c(0.5, 3.5)) + # 表示範囲
  labs(title = "ggplot2::geom_label()")

# ラベルの色を確認
ggplot() + 
  geom_label(data = color_df, mapping = aes(x = x, y = y, label = label, color = color, fill = fill), 
             size = 5, label.padding = unit(1, "lines"), label.size = 2) + 
  scale_color_manual(breaks = unique(color_df[["color"]]), 
                     values = c("blue", "yellow", "green")) + # 線の色
  scale_fill_manual(breaks = unique(color_df[["fill"]]), 
                    values = c("red", "gold", "white")) + # 塗りつぶし色
  theme(axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + # 軸線を非表示化
  lims(x = c(-0.5, 0.5), y = c(0.5, 3.5)) + # 表示範囲
  labs(title = "ggplot2::geom_label()")


# 塗りつぶし用の値を指定
fill_df <- tidyr::expand_grid(
  x = -5:5, 
  y = -5:5
) |> # 格子点を作成
  dplyr::mutate(
    z = x + y, 
    label = paste0(
      "fill = ", stringr::str_pad(z, width = 2, side = "right")
    ) # 確認用ラベル
  )

# 塗りつぶし色を確認
ggplot() + 
  geom_label(data = fill_df, mapping = aes(x = x, y = y, label = label, fill = z), 
             color = "white", label.padding = unit(0.8, "lines")) + 
  coord_equal() + # アスペクト比
  labs(title = "geom_label()", fill = "z=x+y")


