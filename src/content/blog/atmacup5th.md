---
title: "atmaCup5 に参加してきて Private 29 位（Public 27 位）でした！"
postSlug: atmacup5th
pubDatetime: 2020-06-07T11:00:00.000Z
tags: ["atmaCup", "データ分析コンペ"]
---

atmaCup #5 に参加してきました！
[atma.connpass.com](https://atma.connpass.com/event/175139/)

Kaggle 以外のコンペに参加したのは初めてだったのですが、お祭り感があってとても楽しかったです！
参加者・運営全体で盛り上げていく雰囲気があったので、初参加でしたが最後までモチベーション高く取り組み続けることができました。
Twitter TL でよく見かける方々と競えるので燃えました。

運営のみなさま、本当にありがとうございました！ぜひまた参加したいです！

### 問題設定

2 値分類タスクで、評価指標は PR-AUC でした。
正例が少なく、指標も PR-AUC だったので、CV / LB が安定しなかったのが悩ましいところでした。

### コンペ中の動き

1 週間の開催で短期決戦だったので、ドメイン知識の獲得から始める余裕はないと判断して、CNN 様に抽出していただく戦略を取りました。（結果的にそこそこ NN チューニングに時間を使ったので、ドメイン知識をちゃんと学びに行けばよかったとやや後悔しています）
また、短期決戦前提の書き殴り実験をしまくってしまったので、自分が何をやっていたのか正確な記録が無く終盤若干混乱しました。1 週間だったのでギリギリなんとかなりましたが、Kaggle みたいな長期戦は厳しそうなので、そっちに挑む際はもうちょっと丁寧に生きた方が良さそう。仮に入賞したとしても再現できるようにするコストがものすごく高い実装になってしまっていました...

実験管理を mlflow でやったのですが、結構よかったです。
最終日はローカルを投げ捨てて GPU 使い始めたので、今までローカルに積み上げてきた実験との比較が若干厄介でした。（統合せず、別の `mlflow ui` をタブで開いて眺めていました...）
ただ、CV 戦略を変えたり評価指標をいじったときに、同条件で単純比較できない実験にもかかわらずそれが同じテーブルに並んでしまうのが悩ましいなと思いました。
時系列で並んでいるうちは良いのですが、指標ごとにソートすると本当にどれが信じられる結果なのかわかり辛くなってしまいました。
このあたりはタグなどを活用すると良いのかもしれない？もしくは CV などの設計が変わった時点できちんと experiment のレベルで分離すべきだった気がします。

また、実は事前にちょこっとだけ準備していて、「学習データ、テストデータ、CV、モデル、評価関数」あたりを渡すと mlflow にログを書きつつ CV 評価して oof と test の prediction をはく薄い wrapper を書いていました。
最初はまぁ使えていたんですが、NN のことを全く想定していなかったので NN に移った時点で全く使えなくなってしまったのと、細かいことをやりだすとやっぱり wrap された内部に手を入れたくなってしまって厳しかったです。
予想はしていたので相当薄めに作ったつもりだったのですが、それでもこうなっちゃうか〜という感じでちょっと辛い。
ライブラリとしての作りにしたのが間違いだったのかもと思っています。ライブラリだと内部にコンペ固有の変更を入れるのは厳しいので。
単なるスニペット集にしておいて、コンペ中はゴリゴリ内部も書き換える前提で使った方が良さそう。

### やったこと

以下は Discussion にも投稿した内容とほぼ同じですが、こちらにも書いておきます。

[ぐるぐる](https://guruguru.ml/competitions/10/discussions/34d99be1-ab52-4868-a46c-45a24fac8308/)

中盤まで CV スコアすら安定せず、ちょっとシードを変えるだけで大きくスコアが変動してしまっていました。
そのため、正直何が効いていて何が効かなかったのかの判断を誤っていた可能性が高いです...

何をやっても安定しなかったので、最終盤にやけになってアンサンブルで誤魔化す作戦に出たのですが、これは結果的によかったと思います。
特に、CV がある程度安定して比較可能になったおかげで、打つべき手の選択を見誤り辛くなったのが大きかったです。
逆にいえばもっと早くこれをやっておけば、もう少し良いモデルを作れたかも？と反省しています。次回に活かしたいです。

最終サブは CNN と、それをベースに stacking した LightGBM の 2 つを出していました。

#### CNN

- 正規化 / Scaling を全くせずナイーブに Conv1D ベースの NN に突っ込む
  - `((Skip Connection + Conv1D (kernel_size=5) → BatchNorm (or GroupNorm) → ReLU) * 2 → AveragePooling1D (pool_size=2, strides=2)) * 3 → GlobalAveragePooling と GlobalMaxPooling の concat`
- テーブル特徴量は MLP を通して CNN の出力に Concatenate した
  - 採用した特徴量は beta, rms, params1 ~ 6, tsfresh の特徴量たちの中から LightGBM での feature importance が上位 100 以内だったものを取ってきただけ
- テーブル特徴量を MLP に通したものを CNN の途中でに足したらなぜかスコアが上がったので採用
  - 本当はテーブル特徴量を使って、どの領域に注目するかの Attention の計算をしようと思っていたが、そちらはスコアが上がらず...
  - 悔しいので惰性で試した加算がなぜか効いた
- 始めは BN を使っていたが安定しなかった。極端に大きい入力が入ってきたときに BN の statistics がぶっ飛ぶのが悪いのでは、とあたりをつけて GN に変更したところ、伸びはしなかったが安定性が増したように見えたので採用。
- Optimizer は AdamW を使っていたが安定しなかったので、SWA / Lookahead を試したところどちらも安定性の向上を確認できた。最終性能はほぼ変わらなかったが、Lookahead の方が収束が速かったのと個人的に好きだったので採用。
  - Lookahead 採用後は BN でも安定したので、最終的には BN / GN 両方のモデルを作って rank average した。
  - & CosineDecay with linear warmup
  - val prauc で early stopping
- 学習中に checkpoint をとっておき、val loss がよかった 5 epoch 分のモデルの出力の平均を使用
- class weight つき binary crossentropy loss
  - class imbalance については [https://www.tensorflow.org/tutorials/structured_data/imbalanced_data#train_on_the_oversampled_data](https://www.tensorflow.org/tutorials/structured_data/imbalanced_data#train_on_the_oversampled_data) をベースに戦略を立てていました
  - いくつか試しましたが、結局シンプルな class weight が一番よかったです
- CV 戦略は StratifiedKFold(k=5)
  - タスク的には StratifiedKFold じゃまずそうと思いつつも、Group, StratifiedGroup は CV / LB の相関が取れなかったのと、seed を変えた時の暴れ方がすごくて断念

#### LightGBM

こちらは最終日にもう何も思い付かず、とはいえサブを余らせて終わるのも悔しかったので、やってみるかぁという惰性で挑戦したものでした。
CV は Stacking した LightGBM の方がかなり高かったので興奮したのですが、流石に怖かったので CNN も提出していました。
結論としてはどちらも Public / Private 共にほぼ差はなかったです。

時間がなかったのであまり検証はできず、勘で良さそうな構成を選ぶしかなかったのですが、最終的に採用したのは以下のような構成です。

- CNN (BN, GN) の出力を rank にしたもの + テーブル特徴量全部盛り
- optuna の LightGBMCVTuner でハイパラ選択
- 2 Seed Average (Rank Average)
- imbalance 対策は undersampling + bagging
  - 1 : 10 になるように undersampling したデータで普通に 20 モデル作り、単純に平均をとった
  - 1:1 にしたり `is_unbalance=True` にしたりいくつか実験しましたが、これがもっとも CV が高かったです

### うまくいかなかったこと

- Attention
  - Transformer ベースのモデルと、Conv ベースモデルに MultiHeadAttention を足したものの両方を試しましたが、どちらも work せず
- Squeeze and Excitation
- SeparableConv1D にして層を増やす
- Kernel Size を増やす
- NN で Undersampling + Bagging
- 生データの scaling
  - Standardize や 99.9%ile で clip した方が学習は安定したが、スコアがものすごく下がった...
- (Denoising) Variational Auto Encoder
  - train/test で chip が違うことがわかっていたので、教師なし事前学習 → finetune したら LB 上がるのでは、という目論見
  - Reconstruction Error と Encoder の出力 64 次元ベクトルを LightGBM に食わせるのも試したが work せず
- 微分して入力チャネルに追加して CNN に通す

---

---
