---
title: "atmaCup #5 に参加してきて Private 29 位（Public 27 位）でした！"
date: 2020-06-07T11:00:00.000Z
tags: []
---

<p>atmaCup #5 に参加してきました！
<iframe src="https://hatenablog-parts.com/embed?url=https%3A%2F%2Fatma.connpass.com%2Fevent%2F175139%2F" title="【おうちで】atmaCup オンサイトデータコンペ#5 (2020/05/29 18:00〜)" class="embed-card embed-webcard" scrolling="no" frameborder="0" style="display: block; width: 100%; height: 155px; max-width: 500px; margin: 10px 0px;"></iframe><cite class="hatena-citation"><a href="https://atma.connpass.com/event/175139/">atma.connpass.com</a></cite></p>

<p>Kaggle 以外のコンペに参加したのは初めてだったのですが、お祭り感があってとても楽しかったです！
参加者・運営全体で盛り上げていく雰囲気があったので、初参加でしたが最後までモチベーション高く取り組み続けることができました。
<a class="keyword" href="http://d.hatena.ne.jp/keyword/Twitter">Twitter</a> TL でよく見かける方々と競えるので燃えました。</p>

<p>運営のみなさま、本当にありがとうございました！ぜひまた参加したいです！</p>

<h3>問題設定</h3>

<p>2 値分類タスクで、評価指標は PR-AUC でした。
正例が少なく、指標も PR-AUC だったので、CV / LB が安定しなかったのが悩ましいところでした。</p>

<h3>コンペ中の動き</h3>

<p>1 週間の開催で短期決戦だったので、<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%C9%A5%E1%A5%A4%A5%F3">ドメイン</a>知識の獲得から始める余裕はないと判断して、CNN 様に抽出していただく戦略を取りました。（結果的にそこそこ NN チューニングに時間を使ったので、<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%C9%A5%E1%A5%A4%A5%F3">ドメイン</a>知識をちゃんと学びに行けばよかったとやや後悔しています）
また、短期決戦前提の書き殴り実験をしまくってしまったので、自分が何をやっていたのか正確な記録が無く終盤若干混乱しました。1 週間だったのでギリギリなんとかなりましたが、Kaggle みたいな長期戦は厳しそうなので、そっちに挑む際はもうちょっと丁寧に生きた方が良さそう。仮に入賞したとしても再現できるようにするコストがものすごく高い実装になってしまっていました...</p>

<p>実験管理を mlflow でやったのですが、結構よかったです。
最終日はローカルを投げ捨てて <a class="keyword" href="http://d.hatena.ne.jp/keyword/GPU">GPU</a> 使い始めたので、今までローカルに積み上げてきた実験との比較が若干厄介でした。（統合せず、別の <code>mlflow ui</code> をタブで開いて眺めていました...）
ただ、CV 戦略を変えたり評価指標をいじったときに、同条件で単純比較できない実験にもかかわらずそれが同じテーブルに並んでしまうのが悩ましいなと思いました。
時系列で並んでいるうちは良いのですが、指標ごとにソートすると本当にどれが信じられる結果なのかわかり辛くなってしまいました。
このあたりはタグなどを活用すると良いのかもしれない？もしくは CV などの設計が変わった時点できちんと experiment のレベルで分離すべきだった気がします。</p>

<p>また、実は事前にちょこっとだけ準備していて、「学習データ、テストデータ、CV、モデル、評価関数」あたりを渡すと mlflow にログを書きつつ CV 評価して oof と test の prediction をはく薄い wrapper を書いていました。
最初はまぁ使えていたんですが、NN のことを全く想定していなかったので NN に移った時点で全く使えなくなってしまったのと、細かいことをやりだすとやっぱり wrap された内部に手を入れたくなってしまって厳しかったです。
予想はしていたので相当薄めに作ったつもりだったのですが、それでもこうなっちゃうか〜という感じでちょっと辛い。
ライブラリとしての作りにしたのが間違いだったのかもと思っています。ライブラリだと内部にコンペ固有の変更を入れるのは厳しいので。
単なる<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B9%A5%CB%A5%DA%A5%C3%A5%C8">スニペット</a>集にしておいて、コンペ中はゴリゴリ内部も書き換える前提で使った方が良さそう。</p>

<h3>やったこと</h3>

<p>以下は Discussion にも投稿した内容とほぼ同じですが、こちらにも書いておきます。</p>

<p><a href="https://guruguru.ml/competitions/10/discussions/34d99be1-ab52-4868-a46c-45a24fac8308/">&#x3050;&#x308B;&#x3050;&#x308B;</a></p>

<p>中盤まで CV スコアすら安定せず、ちょっとシードを変えるだけで大きくスコアが変動してしまっていました。
そのため、正直何が効いていて何が効かなかったのかの判断を誤っていた可能性が高いです...</p>

<p>何をやっても安定しなかったので、最終盤にやけになってアンサンブルで誤魔化す作戦に出たのですが、これは結果的によかったと思います。
特に、CV がある程度安定して比較可能になったおかげで、打つべき手の選択を見誤り辛くなったのが大きかったです。
逆にいえばもっと早くこれをやっておけば、もう少し良いモデルを作れたかも？と反省しています。次回に活かしたいです。</p>

<p>最終サブは CNN と、それをベースに stacking した LightGBM の 2 つを出していました。</p>

<h4>CNN</h4>

<ul>
<li>正規化 / Scaling を全くせずナイーブに Conv1D ベースの NN に突っ込む

<ul>
<li><code>((Skip Connection + Conv1D (kernel_size=5) → BatchNorm (or GroupNorm) → ReLU) * 2 → AveragePooling1D (pool_size=2, strides=2)) * 3 → GlobalAveragePooling と GlobalMaxPooling の concat</code></li>
</ul>
</li>
<li>テーブル特徴量は <a class="keyword" href="http://d.hatena.ne.jp/keyword/MLP">MLP</a> を通して CNN の出力に Concatenate した

<ul>
<li>採用した特徴量は beta, <a class="keyword" href="http://d.hatena.ne.jp/keyword/rms">rms</a>, params1 ~ 6, tsfresh の特徴量たちの中から LightGBM での feature importance が上位 100 以内だったものを取ってきただけ</li>
</ul>
</li>
<li>テーブル特徴量を <a class="keyword" href="http://d.hatena.ne.jp/keyword/MLP">MLP</a> に通したものを CNN の途中でに足したらなぜかスコアが上がったので採用

<ul>
<li>本当はテーブル特徴量を使って、どの領域に注目するかの Attention の計算をしようと思っていたが、そちらはスコアが上がらず...</li>
<li>悔しいので惰性で試した加算がなぜか効いた</li>
</ul>
</li>
<li>始めは BN を使っていたが安定しなかった。極端に大きい入力が入ってきたときに BN の statistics がぶっ飛ぶのが悪いのでは、とあたりをつけて GN に変更したところ、伸びはしなかったが安定性が増したように見えたので採用。</li>
<li>Optimizer は AdamW を使っていたが安定しなかったので、SWA / Lookahead を試したところどちらも安定性の向上を確認できた。最終性能はほぼ変わらなかったが、Lookahead の方が収束が速かったのと個人的に好きだったので採用。

<ul>
<li>Lookahead 採用後は BN でも安定したので、最終的には BN / GN 両方のモデルを作って rank average した。</li>
<li>&amp; CosineDecay with linear warmup</li>
<li>val prauc で early stopping</li>
</ul>
</li>
<li>学習中に checkpoint をとっておき、val loss がよかった 5 epoch 分のモデルの出力の平均を使用</li>
<li>class weight つき binary crossentropy loss

<ul>
<li>class imbalance については <a href="https://www.tensorflow.org/tutorials/structured_data/imbalanced_data#train_on_the_oversampled_data">https://www.tensorflow.org/tutorials/structured_data/imbalanced_data#train_on_the_oversampled_data</a> をベースに戦略を立てていました</li>
<li>いくつか試しましたが、結局シンプルな class weight が一番よかったです</li>
</ul>
</li>
<li>CV 戦略は StratifiedKFold(k=5)

<ul>
<li>タスク的には StratifiedKFold じゃまずそうと思いつつも、Group, StratifiedGroup は CV / LB の相関が取れなかったのと、seed を変えた時の暴れ方がすごくて断念</li>
</ul>
</li>
</ul>

<h4>LightGBM</h4>

<p>こちらは最終日にもう何も思い付かず、とはいえサブを余らせて終わるのも悔しかったので、やってみるかぁという惰性で挑戦したものでした。
CV は Stacking した LightGBM の方がかなり高かったので興奮したのですが、流石に怖かったので CNN も提出していました。
結論としてはどちらも  Public / Private 共にほぼ差はなかったです。</p>

<p>時間がなかったのであまり検証はできず、勘で良さそうな構成を選ぶしかなかったのですが、最終的に採用したのは以下のような構成です。</p>

<ul>
<li>CNN (BN, GN) の出力を rank にしたもの + テーブル特徴量全部盛り</li>
<li>optuna の LightGBMCVTuner でハイパラ選択</li>
<li>2 Seed Average (Rank Average)</li>
<li>imbalance 対策は undersampling + bagging

<ul>
<li>1 : 10 になるように undersampling したデータで普通に 20 モデル作り、単純に平均をとった</li>
<li>1:1 にしたり <code>is_unbalance=True</code> にしたりいくつか実験しましたが、これがもっとも CV が高かったです</li>
</ul>
</li>
</ul>

<h3>うまくいかなかったこと</h3>

<ul>
<li>Attention

<ul>
<li>Transformer ベースのモデルと、Conv ベースモデルに MultiHeadAttention を足したものの両方を試しましたが、どちらも work せず</li>
</ul>
</li>
<li>Squeeze and Excitation</li>
<li>SeparableConv1D にして層を増やす</li>
<li>Kernel Size を増やす</li>
<li>NN で Undersampling + Bagging</li>
<li>生データの scaling

<ul>
<li>Standardize や 99.9%ile で clip した方が学習は安定したが、スコアがものすごく下がった...</li>
</ul>
</li>
<li>(Denoising) Variational Auto Encoder

<ul>
<li>train/test で chip が違うことがわかっていたので、教師なし事前学習 → <a class="keyword" href="http://d.hatena.ne.jp/keyword/finetune">finetune</a> したら LB 上がるのでは、という目論見</li>
<li>Reconstruction Error と Encoder の出力 64 次元ベクトルを LightGBM に食わせるのも試したが work せず</li>
</ul>
</li>
<li><a class="keyword" href="http://d.hatena.ne.jp/keyword/%C8%F9%CA%AC">微分</a>して入力チャネルに追加して CNN に通す</li>
</ul>

---

---
