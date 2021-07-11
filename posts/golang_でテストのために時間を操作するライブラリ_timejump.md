---
title: "golang でテストのために時間を操作するライブラリ timejump"
date: 2017-12-14T14:21:24.000Z
tags: []
---

<p>現在時刻に依存するコードをテストするとき，<a class="keyword" href="http://d.hatena.ne.jp/keyword/golang">golang</a> で <code>time.Now</code> を普通に使っているとモックできずうまくテストが書けないという問題があります．
時間の操作は time パッケージをそのまま使えば良いのですが，time.Now だけはモックできるようにしたいところです．</p>

<p>解決方法としては，<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B0%A5%ED%A1%BC%A5%D0%A5%EB%CA%D1%BF%F4">グローバル変数</a>に <code>var NowFunc func() time.Time</code> を置いておいて，テスト時に入れ替えるという方法があり，ORM である gorm などが実際にこれを行っています．</p>

<p><a href="https://github.com/jinzhu/gorm/blob/2a1463811ee1dc85d168fd639a2d4251d030e6e5/utils.go#L21">gorm/utils.go at 2a1463811ee1dc85d168fd639a2d4251d030e6e5 &middot; jinzhu/gorm &middot; GitHub</a></p>

<p>例:</p>

<pre class="code lang-go" data-lang="go" data-unlink><span class="synStatement">var</span> NowFunc = time.Now

<span class="synStatement">func</span> Do() <span class="synType">string</span> {
    <span class="synStatement">return</span> NowFunc().String()
}

<span class="synStatement">func</span> TestDo(t *testing.T) {
    now := time.Date(<span class="synConstant">2009</span>, time.November, <span class="synConstant">10</span>, <span class="synConstant">23</span>, <span class="synConstant">0</span>, <span class="synConstant">0</span>, <span class="synConstant">0</span>, time.UTC)
    NowFunc = <span class="synType">func</span>() time.Time {
        <span class="synStatement">return</span> now
    }
    got := Do()
    <span class="synStatement">if</span> got != now.String() {
        t.Fail()
    }
}
</pre>

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/golang">golang</a> の使用上，<a class="keyword" href="http://d.hatena.ne.jp/keyword/Ruby">Ruby</a> の timecop のようなことは出来ないので，こういう工夫をするしかありません．</p>

<p>なんとなく<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B0%A5%ED%A1%BC%A5%D0%A5%EB%CA%D1%BF%F4">グローバル変数</a>をテストのために置いて書き換えるのが嫌なのと，なんにも考えずに <code>t.Parallel()</code> を置けなくなるのがちょっと嫌だなと思っていました．
また，時間経過をシミュレーションしたい場合は，そういう <code>NowFunc</code> を毎回書く必要があり，結構面倒です．
あとパッケージをまたぐと厄介だし毎回書くのも嫌．</p>

<p>そこで，<a class="keyword" href="http://d.hatena.ne.jp/keyword/Ruby">Ruby</a> の timecop のように現在時刻をいじくり回せるようにするライブラリを作ってみました．</p>

<p><iframe src="https://hatenablog-parts.com/embed?url=https%3A%2F%2Fgithub.com%2Fagatan%2Ftimejump" title="agatan/timejump" class="embed-card embed-webcard" scrolling="no" frameborder="0" style="display: block; width: 100%; height: 155px; max-width: 500px; margin: 10px 0px;"></iframe><cite class="hatena-citation"><a href="https://github.com/agatan/timejump">github.com</a></cite></p>

<p><iframe src="https://hatenablog-parts.com/embed?url=https%3A%2F%2Fgodoc.org%2Fgithub.com%2Fagatan%2Ftimejump" title="Package timejump" class="embed-card embed-webcard" scrolling="no" frameborder="0" style="display: block; width: 100%; height: 155px; max-width: 500px; margin: 10px 0px;"></iframe><cite class="hatena-citation"><a href="https://godoc.org/github.com/agatan/timejump">godoc.org</a></cite></p>

<p>使用する際は <code>time.Now</code> をすべて <code>timejump.Now</code> に置き換える必要があります．（ <a class="keyword" href="http://d.hatena.ne.jp/keyword/Ruby">Ruby</a> と違って <code>time.Now</code> を直接上書きできないので...）
普段は <code>timejump.Now</code> と <code>time.Now</code> は <code>if !active { ... }</code> が一段挟まるだけなのでパフォーマンスに影響はほとんどないはずです．</p>

<p>テスト時は，<code>timejump.Now</code> の挙動を変えたいテストの頭で</p>

<pre class="code lang-go" data-lang="go" data-unlink><span class="synStatement">func</span> TestDo(t *testing.T) {
    timejump.Activate()
    <span class="synStatement">defer</span> timejump.Deactivate()
    ...
}
</pre>

<p>とします．</p>

<p><code>timejump.Activate</code> な<a class="keyword" href="http://d.hatena.ne.jp/keyword/%B6%E8%B4%D6">区間</a>はロックをとっているので，テストを並列で走らせても並列に走らなくなります．</p>

<p><code>timejump.Stop()</code> で時間停止，<code>timejump.Jump</code> で時間移動，<code>timejump.Move</code> で<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%BF%A5%A4%A5%E0%A5%BE%A1%BC%A5%F3">タイムゾーン</a>の移動，<code>timejump.Scale</code> で時間の経過速度をいじれます．</p>

<p>時間を止めたいだけの場合は<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B0%A5%ED%A1%BC%A5%D0%A5%EB%CA%D1%BF%F4">グローバル変数</a>に <code>NowFunc</code> を持っておいて <code>t.Parallel</code> を間違って置かないように気をつけるほうが正直楽だとは思いますが，時間経過をテストしたい場合にはちょっと楽になるはずです．</p>

<p>もともとあるパッケージのテストをするために書いたパッケージだったのですが，目的だったテストを書く前にテストしたいパッケージが御役御免になってしまったので，timejump も御役御免になってしまいました．
いつか使う日が来る気がするので，ここに寝かせておきます．</p>

---

---
