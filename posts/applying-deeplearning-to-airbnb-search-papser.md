---
title: "'Applying Deep Learning To Airbnb Search'  を読んだ"
date: 2018-11-21T12:52:58.000Z
tags: ["DeepLearning", "MachineLearning", "論文読み"]
---

[[1810.09591] Applying Deep Learning To Airbnb Search](https://arxiv.org/abs/1810.09591) を読んだときのメモをそのまま出してみます。面白かった。
本当にメモなので、詳細は原文を読んでください。

airbnb の search ranking に deep learning を導入していく過程を論文っぽくしたもの。

おしゃれなモデリング手法の提案とかじゃなくて、現実の問題に対して NN を適用していくにあたって発見した良かったこと・悪かったことについてまとめた文章になっている。

## Motivation

もともと Gradient Boosted Decision Tree でやっていて結構うまく行っていたが、gain が停滞してきたのでそれの突破口を探していた。

## Model Evolution

評価指標は NDCG (Normalized Discounted Cumulative Gain) を使っている。

![f:id:agtn:20181121214034p:plain](/i/20181121214034.png "f:id:agtn:20181121214034p:plain")

"Convolutional Neural Networks for Visual Recognition" を書いた A. Kaypathy が "don't be a hero" と言っている（複雑なモデルを扱えると思わないほうがいいよ、みたいな意味？）が、"Why can't we be heroes?" と言いながら複雑なモデルに爆進したらしい

その結果、無限に時間を持っていかれて全然うまくいかなかった。

結局、最初に production に入ったモデルはめちゃくちゃシンプルな NN だった。

> a simple single hidden layer NN with 32 fully connected ReLU activations that proved booking neutral against the GBDT model.

入力や目的関数も GBDT と全く同じにしている。（booking するかしないかの L2 regression loss）

ものすごく gain があったわけではないけれど、NN が production でうごく、live traffic をちゃんとさばけるという pipeline を整えるためにこの step 自体は良かったと言っている。

やりすぎないことで先に進むことはできたが、すごくよくもなっていなかった。つぎの breakthrough は LambdaRank + NN を組み合わせたことだった。LambdaRank は Learning to Rank のアルゴリズムで、簡単にいえばロス関数に直接評価指標（ここでは NDCG）を組み込める（ここまでは learning to rank 的なアプローチはまったく取っていなかった。単にクリック率をよく予測し、それを上から出すことで NDCG を最適化していた）。

これらをやりながらも Factorization Machine と GBDT は research を続けていて、NN と comparable な成果が出せることはわかっていた。comparable な成果が出ている一方で出力される list は全然別物だったので、組み合わせたらもっと良くなるのではということで、FM や GBDT の結果を特徴量に含む NN を学習させて利用することにした。

![f:id:agtn:20181121214010p:plain](/i/20181121214010.png "f:id:agtn:20181121214010p:plain")

この時点でもうモデルの複雑さは結構なものになっていて、機械学習の技術的負債問題が顔をだしつつあった。そこで、 ensamble などをすべて捨てて、単に DNN を大量のデータ（いままでの 10x）で学習させるというシンプルな解法に舵を切った（DNN とはいえ 2 hidden layers）。入力の次元は 195 次元、1st hidden layer = 127, 2nd = 83 というモデル。入力の特徴量はシンプルなもので、価格、アメニティ、過去の booking 数、などなど。ほぼ feature engineering をせずに入力した（これが DNN にする目的だった）。

## Failed Models

失敗についても言及してくれていてすごく嬉しい。

- ID を使ったモデルは過学習がひどくて使えなかった
  - 扱っているアイテムの都合上、ある ID に対してたくさんのデータが取れることがない
    - どれだけ人気でも年間 365 までしかコンバージョンしない
- 詳細ページへの遷移は booking よりはるかに多いし dense なので、それを使うモデルも作ったがうまくいかなかった
  - multi task 学習で、booking prediction と long view prediction を予測させた

あとは NN は特徴量を normalize したほうがいいよねとか、特徴量の distribution を観察しましょうとか、Java 上で TensorFlow のモデル動かすの大変でしたとか、いろいろ現実っぽい話が並ぶ。
もっといろいろ言ってるけど、詳細は本文を読んだほうが良い。

画像とかじゃない領域で DNN を production にいれるにはこういうステップを経るんだなというのが伝わってくるし、この話から得るべき学びがかなりある。光景が目に浮かぶ良い文章だなと思いました。

---

---
