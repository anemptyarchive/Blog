
# magickパッケージによるアニメーションの作成 ---------------------------------------------------

?magick::image_animate
?magick::image_write_gif
?magick::image_write_video


# 画像サイズが共通の場合 ---------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(patchwork)
library(magick)

# チェック用
library(ggplot2)
library(patchwork)
library(MASS)

### ・画像の作成 -----

# ガウス-ガンマ分布のパラメータを指定
m    <- 0
beta <- 2
a    <- 5
b    <- 6

# ガウス-ガンマ分布の確率変数として利用する値を指定
lambda_i <- seq(from = 0, to = 2.5, length.out = 101)
mu_i     <- seq(from = -1.5, to = 1.5, length.out = 101)

# ガウス分布のパラメータを格納
param_df <- tibble::tibble(
  mu = mu_i, 
  lambda = lambda_i
)

# フレーム数を設定
frame_num <- nrow(param_df)

# ガウス-ガンマ分布の確率変数の期待値を計算
E_mu     <- m
E_lambda <- a / b

# ガウス-ガンマ分布の確率変数の値を作成
mu_vals <- seq(
  from = E_mu - 1/sqrt(beta*E_lambda) * 4, 
  to = E_mu + 1/sqrt(beta*E_lambda) * 4, 
  length.out = 200
)
lambda_vals <- seq(from = 0, to = E_lambda * 3, length.out = 200)

# ガウス-ガンマ分布を計算
gaussian_gamma_df <- tidyr::expand_grid(
  mu = mu_vals, # 確率変数μ
  lambda = lambda_vals # 確率変数λ
) |> # 確率変数(μとλの格子点)を作成
  dplyr::mutate(
    N_dens = dnorm(x = mu, mean = m, sd = 1/sqrt(beta*lambda)), # μの確率密度
    Gam_dens = dgamma(x = lambda, shape = a, rate = b), # λの確率密度
    density = N_dens * Gam_dens # 確率密度
  )

# ガウス分布の確率変数の値を作成
x_vals <- mu_vals

# パラメータの期待値によるガウス分布を計算
E_gaussian_df <- tidyr::tibble(
  x = x_vals, # 確率変数
  density = dnorm(x = x_vals, mean = E_mu, sd = 1/sqrt(E_lambda)) # 確率密度
)


# 保存用のフォルダを作成
dir_path  <- "rtweet_to_magick/figure/tmp_distribution"
file.remove(path = dir_path)
dir.create(path = dir_path)

# パラメータのサンプルごとに作図
for(i in 1:frame_num) {
  # i番目のパラメータを取得
  mu     <- mu_i[i]
  lambda <- lambda_i[i]
  
  # パラメータラベルを作成
  hyparam_text <- paste0(
    "list(", 
    "m==", m, ", beta==", beta, ", a==", a, ", b==", b, 
    ", E(mu)==", E_mu, ", E(lambda)==", round(E_lambda, 3), 
    ")"
  )
  param_text <- paste0(
    "list(mu==", round(mu, 2), ", lambda==", round(lambda, 2), ")"
  )
  
  # ガウス-ガンマ分布を作図
  gaussian_gamma_graph <- ggplot() + 
    geom_contour_filled(data = gaussian_gamma_df, mapping = aes(x = mu, y = lambda, z = density, fill = ..level..), 
                        alpha = 0.8) + # パラメータの生成分布
    geom_point(mapping = aes(x = E_mu, y = E_lambda), 
               color = "red", size = 5, shape = 4) + # パラメータの期待値
    geom_point(data = param_df[i, ], mapping = aes(x = mu, y = lambda), 
               color = "gold", size = 5) + # パラメータのサンプル
    labs(title = "Gaussian-Gamma Distribution", 
         subtitle = parse(text = hyparam_text), 
         fill = "density", 
         x = expression(mu), y = expression(lambda))
  
  # ガウス分布を計算
  gaussian_df <- tibble::tibble(
    x = x_vals, # 確率変数
    density = dnorm(x = x_vals, mean = mu, sd = 1/sqrt(lambda)) # 確率密度
  )
  
  # ガウス分布を作図
  gaussian_graph <- ggplot() + 
    geom_line(data = E_gaussian_df, mapping = aes(x = x, y = density), 
              color = "red", size = 1, linetype = "dashed") + # 期待値による分布 
    geom_line(data = gaussian_df, mapping = aes(x = x, y = density), 
              color = "gold", size = 1) + # サンプルによる分布
    coord_cartesian(ylim = c(0, 0.7)) + # 描画範囲
    labs(title = "Gaussian Distribution", 
         subtitle = parse(text = param_text), 
         x = expression(x), y = "density") # ラベル
  
  # グラフを並べて描画
  graph <- gaussian_gamma_graph / gaussian_graph + 
    patchwork::plot_layout(guides = "collect")
  
  # ファイルを書き出し
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = 3, pad = "0"), ".png") # widthはframe_numの桁数
  ggplot2::ggsave(filename = file_path, plot = graph, width = 800, height = 800, units = "px", dpi = 100)
  
  # 途中経過を表示
  print(paste0(i, " (", round(i/frame_num*100, 1), "%)"))
}


### ・動画の作成 -----

# 読み込み元フォルダを指定
dir_path  <- "rtweet_to_magick/figure/tmp_distribution"

# ファイル名を取得
file_name_vec <- list.files(dir_path)

# ファイルパスを作成
file_path_vec <- paste0(dir_path, "/", file_name_vec)

# 画像ファイルを読み込み
pic_data_vec <- magick::image_read(path = file_path_vec)

# gif画像を作成
gif_data <- magick::image_animate(image = pic_data_vec, fps = 1, dispose = "previous")

# gifファイルを書き出し
magick::image_write_gif(image = pic_data_vec, path = "rtweet_to_magick/figure/animation/GaussianGamma_write.gif", delay = 0.1)
magick::image_write_gif(image = gif_data, path = "rtweet_to_magick/figure/animation/GaussianGamma_animate.gif", delay = 0.1)

# mp4ファイルを書き出し
magick::image_write_video(image = pic_data_vec, path = "rtweet_to_magick/figure/animation/GaussianGamma_write.mp4", framerate = 10)
magick::image_write_video(image = gif_data, path = "rtweet_to_magick/figure/animation/GaussianGamma_animate.mp4", framerate = 10)


# 画像サイズがバラバラの場合 -----------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(magick)

# チェック用
library(ggplot2)


### ・画像の作成 -----

# フレーム数を指定
frame_num <- 10

# 座標の下限を指定
lower_x <- 4
lower_y <- 3

# 値を作成
x_vals <- seq(from = 1, to = lower_x+frame_num, by = 1)
y_vals <- seq(from = 1, to = lower_y+frame_num, by = 1)

# 作図用のデータフレームを作成
df <- tidyr::expand_grid(x = x_vals, y = y_vals) |> 
  dplyr::mutate(z = x + y)

# ヒートマップを確認
graph <- ggplot(data = df, mapping = aes(x = x, y = y, fill = z)) + # ヒートマップ
  geom_tile(show.legend = FALSE) + 
  geom_text(mapping = aes(label = paste0("(", x, ", ", y, ")")), color = "white", size = 5) + # 座標ラベル
  scale_fill_gradient(low ="gold", high = "red") + # グラデーション
  scale_x_continuous(breaks = x_vals) + 
  scale_y_reverse(breaks = y_vals) + # y軸を反転
  coord_equal(clip = "off", expand = FALSE) + # 余白を非表示
  labs(title = "Heatmap", 
       subtitle = parse(text = paste0("list(x==list(1, cdots, ", lower_x+frame_num, ")", 
                                      ", y==list(1, cdots, ", lower_y+frame_num, "))")))

# 保存先フォルダを作成
dir_path  <- "rtweet_to_magick/figure/tmp_heatmap"
file.remove(path = dir_path)
dir.create(path = dir_path)

# フレームごとに作図
for(i in 1:frame_num) {
  # i番目のデータを抽出
  tmp_df <- df |> 
    dplyr::filter(x <= lower_x+i, y <= lower_y+i)
  
  # ヒートマップを作成
  g <- ggplot(data = tmp_df, mapping = aes(x = x, y = y, fill = z)) + 
    geom_tile(show.legend = FALSE) + # ヒートマップ
    geom_text(mapping = aes(label = paste0("(", x, ", ", y, ")")), color = "white", size = 5) + # 座標ラベル
    scale_fill_gradient(low ="gold", high = "red") + # グラデーション
    scale_x_continuous(breaks = x_vals) + 
    scale_y_reverse(breaks = y_vals) + # y軸を反転
    coord_equal(clip = "off", expand = FALSE) + # 余白を非常時
    labs(title = "Heatmap", 
         subtitle = parse(text = paste0("list(x==list(1, cdots, ", lower_x+i, ")", 
                                        ", y==list(1, cdots, ", lower_y+i, "))")))
  
  # ファイルを書き出し
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = 2, pad = "0"), ".png") # widthはframe_numの桁数
  ggplot2::ggsave(
    filename = file_path, plot = g, 
    width = (lower_x+i)*100, height = (lower_y+i)*100, units = "px", dpi = 100
  )
  
  # 途中経過を表示
  print(paste0(i, " (", round(i/frame_num*100, 1), "%)"))
}


### ・動画の作成：加工処理なし -----

# 読み込み元フォルダを指定
dir_path  <- "rtweet_to_magick/figure/tmp_heatmap"

# ファイル名を取得
file_name_vec <- list.files(dir_path)

# ファイルパスを作成
file_path_vec <- paste0(dir_path, "/", file_name_vec) # 小 → 大
#file_path_vec <- rev(file_path_vec) # 大 → 小

# 画像ファイルを読み込み
pic_data_vec <- magick::image_read(path = file_path_vec)

# gif画像を作成
gif_data <- magick::image_animate(image = pic_data_vec, fps = 1, dispose = "previous")

# gifファイルを書き出し
magick::image_write_gif(image = pic_data_vec, path = "rtweet_to_magick/figure/animation/heatmap_StoL_write.gif", delay = 0.4) # 1枚目サイズ依存の自動リサイズ
magick::image_write_gif(image = gif_data, path = "rtweet_to_magick/figure/animation/heatmap_StoL_animate.gif", delay = 0.4) # 1枚目サイズ依存の自動トリミング
file_path_vec |> 
  rev() |> 
  magick::image_read() |> 
  magick::image_write_gif(path = "rtweet_to_magick/figure/animation/heatmap_LtoS_write.gif", delay = 0.4) # 1枚目サイズ依存の自動リサイズ
file_path_vec |> 
  rev() |> 
  magick::image_read() |> 
  magick::image_animate() |> 
  magick::image_write_gif(path = "rtweet_to_magick/figure/animation/heatmap_LtoS_animate.gif", delay = 0.4) # 1枚目サイズ依存の自動トリミング


### ・動画の作成：リサイズ -----

# 読み込み元フォルダを指定
dir_path  <- "rtweet_to_magick/figure/tmp_heatmap"

# ファイルパスを作成
file_path_vec <- dir_path |> 
  list.files() |> 
  (\(.){paste0(dir_path, "/", .)})()

# 画像情報を取得
pic_info_df <- file_path_vec |> 
  magick::image_read() |> 
  magick::image_info()

# 縦横の最大サイズを取得
max_w <- max(pic_info_df[["width"]])
max_h <- max(pic_info_df[["height"]])

# 最大サイズに変形して確認
file_path_vec |> 
  magick::image_read() |> 
  magick::image_scale(geometry = paste0(max_w, "x", max_h)) |> 
  magick::image_info()

# 最大サイズに変形してgif画像を作成
file_path_vec |> 
  #rev() |> 
  magick::image_read() |> 
  magick::image_scale(geometry = paste0(max_w, "x", max_h)) |> 
  #magick::image_animate(fps = 1, dispose = "previous") |> 
  magick::image_write_gif(path = "rtweet_to_magick/figure/animation/heatmap_resize.gif", delay = 0.4)


### ・動画の作成：背景挿入 -----

# 読み込み元フォルダを指定
dir_path  <- "rtweet_to_magick/figure/tmp_heatmap"

# ファイルパスを作成
file_path_vec <- dir_path |> 
  list.files() |> 
  (\(.){paste0(dir_path, "/", .)})()

# 画像情報を取得
pic_info_df <- file_path_vec |> 
  magick::image_read() |> 
  magick::image_info()

# 縦横の最大サイズを取得
max_w <- max(pic_info_df[["width"]])
max_h <- max(pic_info_df[["height"]])

# 背景用の画像を読み込み
background_data <- magick::image_read(path = "rtweet_to_magick/data/picture/background.png") |> 
  magick::image_scale(geometry = paste0(max_w, "x", max_h, "!"))
background_data |> 
  magick::image_info()


# 画像ごとに背景を挿入
for(i in seq_along(file_path_vec)) {
  # i番目のファイルパスを抽出
  file_path <- file_path_vec[i]
  
  # 画像を読み込み
  pic_data <- magick::image_read(path = file_path)
  
  if(i == 1) {
    # 背景画像に重ねる
    pic_data_vec <- magick::image_mosaic(image = c(background_data, pic_data))
  } else {
    # 背景画像に重ねる
    pic_back_data <- magick::image_mosaic(image = c(background_data, pic_data))
    
    # ベクトルに格納
    pic_data_vec <- c(pic_data_vec, pic_back_data)
  }
}

# gif画像を作成
pic_data_vec |> 
  magick::image_animate(fps = 1, dispose = "previous") |> 
  magick::image_write_gif(path = "rtweet_to_magick/figure/animation/heatmap_background.gif", delay = 0.4)

