---
title: "【論文紹介】Concurrent Spatial and Channel Squeeze & Excitation in Fully Convolutional Networks"
date: 2019-01-04T14:51:07+09:00
tags: ["Python", "DeepLearning", "Keras", "TensorFlow", "論文読み"]
url: https://qiita.com/agatan/items/61546d71e7ea7ad14b11
---

Fully Convolutional Network (FCN) の性能を enhance する Concurrent Spatial and Channel Squeeze & Excitation (scSE) というモジュールを提案した論文です。
既存の良いとされてきたモデルたちに計算量をそこまで増やさずに & 簡単に組み込むことができ、 Image Segmentation などのタスクで性能を向上させることができます。

[ILSVRC 2017 画像分類 Top の手法 Squeeze-and-Excitation Networks - Qiita](https://qiita.com/agatan/items/8cf2566908228eaa5450) で紹介した SE モジュールの後継にあたります。

![image.png](https://qiita-image-store.s3.amazonaws.com/0/39030/f606f8d0-9510-f251-31d8-e9091ed031b9.png)

## Reference

- Abhijit Guha Roy, et al., MICCAI 2018
- https://arxiv.org/abs/1803.02579

文中の図表は論文から引用しています。

この記事は、Wantedly の勉強会で取り上げられた論文・技術をまとめたものです。
[2018 年に読んだ機械学習系論文・技術まとめ at Wantedly Advent Calendar 2018 - Qiita](https://qiita.com/advent-calendar/2018/wantedly_ml)

## Squeeze and Excitation を Image Segmentation に応用する

Squeeze and Excitation (SE) モジュールは、[Squeeze-and-Excitation Networks](https://arxiv.org/abs/1709.01507) で提案されたもので、 ILSVRC 2017 でトップのスコアを記録しています。
SE は Channel 間の関係性を考慮できるようにしたい、というモチベーションで、 チャンネルごとに画像全体の activation の平均を取り（Squeeze)、それをもとにチャンネル間の Attention をとる（Excitation）というものでした。（[ILSVRC 2017 画像分類 Top の手法 Squeeze-and-Excitation Networks - Qiita](https://qiita.com/agatan/items/8cf2566908228eaa5450)）
本論文ではこのオリジナルの SE モジュールのことを channel SE (Spatial Squeeze and Channel Excitation, cSE) と呼んでいます。

```python
from tensorflow.python.keras.layers import GlovelAveragePooling2D, Dense, multiply

def spatial_squeeze_and_channel_excitation(x, ch, ratio=16):
    squeeze = GlobalAveragePooling2D()(x)
    z = Dense(ch // ratio, activation='relu')(squeeze)
    excitation = Dense(ch, activation='sigmoid')(x)
    return multiply([x, excitation])
```

本論文では Image Classification の性能を大きく向上した SE モジュールを、 Image Segmentation に応用することを考えます。
Image Segmentation のタスクでは、Fully Convolutional な Architecture がよく採用されます。
この論文では、U-Net[^1] やそこから派生した SkipDeconv-Net[^2]， Fully Convolutional DenseNet[^3] などに対して SE モジュール的な考え方で性能を向上できないか実験しています。

[^1]: Ronneberger O, Fischer P, Brox T. U-net: Convolutional networks for biomedical image segmentation. In Proc. MICCAI, Springer 2015, pp. 234-241.
[^2]: Roy, A.G., Conjeti, S., Sheet, D., Katouzian, A., Navab, N. and Wachinger, C., 2017, September. Error Corrective Boosting for Learning Fully Convolutional Networks with Limited Data. In MICCAI, pp. 231-239, Springer.
[^3]: J ́egou, S., Drozdzal, M., Vazquez, D., Romero, A. and Bengio, Y., 2017, July. The one hundred layers tiramisu: Fully convolutional densenets for semantic segmentation. In CVPR Workshop, pp. 1175-1183, IEEE.

が、実際に SE モジュールをこれらの FCN に組み込んでみると、 Image Classification のときよりも性能が上がりづらいという結果が得られています。
この論文では、 「Image Segmentation は pixel-wise の情報が重要であり、チャンネルごとに画像全体から平均を取る cSE ではピクセル単位の情報をうまく enhance できていないのでは」という仮説を立てています。

## Channel Squeeze and Spatial Excitation Block (sSE)

そこでこの論文で提案されているので、 sSE です。
名前のとおりですが、 Channel 方向に Squeeze し、Pixel ごとに Excitation を計算します。
cSE は画像全体（Spatial）で Squeeze し、Channel ごとの Excitation を計算しているので、その逆をやっているというイメージです。

<img src="https://qiita-image-store.s3.amazonaws.com/0/39030/7c6f2822-ff51-c4f6-2d3f-6c0422dca3fa.png" width="60%">

実装はものすごく単純です。以下に `tf.keras` をつかった場合の実装例を載せます。

```python
import tensorflow as tf

def channel_squeeze_and_spatial_excitation(x):
    excitation = tf.keras.layers.Conv2D(filters=1, kernel_size=1, activation='sigmoid')(x)
    return tf.keras.layers.multiply([x, excitation])
```

`Conv2D(filters=1, kernel_size=1, activation='sigmoid')` で、pixel ごとに 1 チャンネルの値を 0~1 で出力させます。
これが「ある pixel における excitation」になります。出力は、入力である feature map と excitation の element-wise な積です。

## Spatial and Channel Squeeze & Excitation (scSE)

また、提案手法である sSE とオリジナルの cSE は conflict しないので、両方採用してしまおう、というのが scSE です。

<img src="https://qiita-image-store.s3.amazonaws.com/0/39030/fcbc2c76-3947-f587-eb8a-4422b4a28b7e.png" width="70%">

この図の上部が sSE、下部が cSE です。同じ入力からそれぞれを計算し、最後に単純に足し算したものを scSE と呼んでいます。

```python
def _concurrent_spartial_and_channel_se(input_feature, ch, ratio=16):
    cse = _spatial_squeeze_and_channel_excitation(input_feature, ch, ratio=ratio)
    sse = _channel_squeeze_and_spatial_excitation(input_feature)
    return tf.keras.layers.Add()([cse, sse])
```

この論文で実験に使われている U-Net の場合、 scSE を使った場合でも計算量は 1.5% 程度の増加で済んでいます。

## Experiments

いくつかのネットワークについて、「素の状態」「cSE」「sSE」「scSE」の 4 パターンで実験しています。
ここでは DenseNet のケースについてまとめた図を論文中の Fig.2 から引用します。
詳細や全体像は論文を参照してください。

![image.png](https://qiita-image-store.s3.amazonaws.com/0/39030/31a9cee5-5d22-b682-1400-b3f98bfe5ae3.png)

横軸はタスク名です。
タスクにもよりますが、概ね `DenseNets` < `DenseNets + cSE` < `DenseNets + sSE` < `DenseNets + scSE` になっているように見えます。
cSE だけをいれると素の状態より性能が悪くなっているケースも見られるのが面白いところです。

## まとめと感想

タスクの特性を見て仮説を立て、実際にそれがうまくハマっているという論文で、よみやすいし納得感のある論文でした。
実装の容易さと試しやすさ（既存モデルへ着脱できる）がうれしい手法で、実際に活用しているモデルに組み込まれています。
