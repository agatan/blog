---
title: "NCurses の Crystal binding を作った"
date: 2016-12-08T13:33:17.000Z
tags: ["Crystal"]
---

<p>この記事は、 Crystal Advent Calendar 2016 の８日目の記事です。
<iframe src="//hatenablog-parts.com/embed?url=http%3A%2F%2Fqiita.com%2Fadvent-calendar%2F2016%2Fcrystal" title="Crystal Advent Calendar 2016 - Qiita" class="embed-card embed-webcard" scrolling="no" frameborder="0" style="display: block; width: 100%; height: 155px; max-width: 500px; margin: 10px 0px;"></iframe><cite class="hatena-citation"><a href="http://qiita.com/advent-calendar/2016/crystal">qiita.com</a></cite></p>

<p>ncurses という <a class="keyword" href="http://d.hatena.ne.jp/keyword/CUI">CUI</a> を作るためにスクリーンやキー入力を扱う有名なライブラリの Crystal binding を作りました。</p>

<p><iframe src="//hatenablog-parts.com/embed?url=https%3A%2F%2Fgithub.com%2Fagatan%2Fncurses.cr" title="agatan/ncurses.cr" class="embed-card embed-webcard" scrolling="no" frameborder="0" style="display: block; width: 100%; height: 155px; max-width: 500px; margin: 10px 0px;"></iframe><cite class="hatena-citation"><a href="https://github.com/agatan/ncurses.cr">github.com</a></cite></p>

<p>ほとんど C の ncurses と同じ感じで使えるようになっていると思います．
<a class="keyword" href="http://d.hatena.ne.jp/keyword/Ruby">Ruby</a> や <a class="keyword" href="http://d.hatena.ne.jp/keyword/Python">Python</a> の <a class="keyword" href="http://d.hatena.ne.jp/keyword/curses">curses</a> ライブラリを参考にしつつ，なるべく使用感がかわらないようにしています．</p>

<h2>examples</h2>

<pre class="code lang-crystal" data-lang="crystal" data-unlink><span class="synPreProc">require</span> “ncurses”

<span class="synType">NCurses</span>.open <span class="synStatement">do</span>
  <span class="synType">NCurses</span>.cbreak
  <span class="synType">NCurses</span>.noecho
  <span class="synType">NCurses</span>.move(<span class="synConstant">x</span>: <span class="synConstant">30</span>, <span class="synConstant">y</span>: <span class="synConstant">20</span>)
  <span class="synType">NCurses</span>.addstr(“<span class="synType">Hello</span>, world!”)
  <span class="synType">NCurses</span>.refresh

  <span class="synType">NCurses</span>.notimeout(<span class="synConstant">true</span>)
  <span class="synType">NCurses</span>.getch
<span class="synStatement">end</span>
</pre>

<p><code>NCurses.addstr</code> とか <code>NCurses.move</code> とかは ncurses で言う <code>addstr</code> や <code>move</code> に当たる関数で，<code>stdscr</code> に対して <code>waddstr</code> とか <code>wmove</code> するやつです．</p>

<p><code>w = NCurses::Window.new(height: height, width: width)</code> とすることで subwindow が作れます．
<code>w.addstr</code> や <code>w.move</code> という形で <code>w</code> prefix な関数たちが呼べるようになっています．</p>

<p><code>pad</code> や <code>attron</code> / <code>attroff</code> などなども使えます．</p>

<p>詳細な例は <code>example/</code> 以下のにおいてあります</p>

<h2>なぜ作ったのか</h2>

<p>実は ncurses bindings for Crystal はすでにあります．(<a href="https://github.com/jreinert/ncurses-crystal">https://github.com/jreinert/ncurses-crystal</a>)<br/>
<a class="keyword" href="http://d.hatena.ne.jp/keyword/curses">curses</a> が Crystal の標準ライブラリから外されることが決まったときの <a class="keyword" href="http://d.hatena.ne.jp/keyword/CHANGELOG">CHANGELOG</a> を見ると，今後はそっちを使ってねと書いてあったりします．</p>

<p>じゃあなんでわざわざ別で作ったのかという話ですが，<a class="keyword" href="http://d.hatena.ne.jp/keyword/API">API</a> が足りない &amp; 提供する <a class="keyword" href="http://d.hatena.ne.jp/keyword/API">API</a> の形を変えたいと思ったからです．</p>

<p>単純に bind されている <a class="keyword" href="http://d.hatena.ne.jp/keyword/API">API</a> が少なくてやりたいことができなかったので，最初は追加して PR を出そうと思っていたのですが，すでに提供されている <a class="keyword" href="http://d.hatena.ne.jp/keyword/API">API</a> が割と高級になっていて 1 : 1 で C <a class="keyword" href="http://d.hatena.ne.jp/keyword/API">API</a> と対応していない感じでした．
個人的には C library の wrapper にはなるべく薄くなっていてもらって基本的な <a class="keyword" href="http://d.hatena.ne.jp/keyword/API">API</a> は覚え直さないでも使えるようになっていてほしいというふうに思ったので，C <a class="keyword" href="http://d.hatena.ne.jp/keyword/API">API</a> と 1 : 1 で対応した形の <a class="keyword" href="http://d.hatena.ne.jp/keyword/API">API</a> を提供する wrapper を作ってみようという経緯で新しく作ることにしました．</p>

<h2>おまけ</h2>

<p>C bindings を書くときに，wrapper <a class="keyword" href="http://d.hatena.ne.jp/keyword/API">API</a> として <code>LibC::Int</code> が登場しちゃうのがなんとなく嫌で，<code>LibC::Int</code> を C が要求してくる関数を呼ぶ wrapper 関数には型指定をあえてしないという選択をしたんですがどうなんでしょう．</p>

<pre class="code lang-crystal" data-lang="crystal" data-unlink><span class="synPreProc">lib</span> <span class="synType">LibNCurses</span>
  <span class="synPreProc">fun</span> <span class="synIdentifier">wmove</span>(<span class="synIdentifier">w</span> : <span class="synType">Window</span>, y : <span class="synType">LibC</span>::<span class="synType">Int</span>, x : <span class="synType">LibC</span>::<span class="synType">Int</span>) : <span class="synType">Result</span>
<span class="synPreProc">end</span>
</pre>

<pre class="code lang-crystal" data-lang="crystal" data-unlink><span class="synPreProc">class</span> <span class="synType">Window</span>
  <span class="synPreProc">def</span> <span class="synIdentifier">move</span>(y, x)
     <span class="synType">LibNCurses</span>.wmove(<span class="synIdentifier">@win</span>, y, x)
  <span class="synPreProc">end</span>
<span class="synPreProc">end</span>
</pre>

<p>みたいな感じです．（多少簡略化しています）</p>

<p>これどうなんですかね．なるべく外に見せる <a class="keyword" href="http://d.hatena.ne.jp/keyword/API">API</a> には型を明記するようにしたかったのですが，<code>LibC:Int</code> 系は環境によって異なるのでそういうわけにも行かず…</p>

<p><code>y : LibC::Int, x : LibC::Int</code> とかは別に良いんですが，ncurses は文字と属性をくっつけた <code>chtype</code> なる型を持っていてこれが結構厄介というか混乱を招くのではと思っています．
<code>chtype</code> は <code>char</code> ではなく <code>unsigned int</code> で，文字と属性を bitor でくっつけたものになっています．<code>addch</code> のように <code>char</code> をとることを連想させる関数の引数が実は <code>chtype = unsigned int</code> でしかも Crystal の文字型 <code>Char</code> は 32bit なのでものすごく混乱します…</p>

<p>C は型変換を勝手にやってくれるので，<code>unsigned int</code> を返す関数から受け取った値を <code>short</code> を受け取る関数に渡すみたいなことをよくやっていて，Crystal のような型変換を暗黙にやらない言語から使おうとすると難しいんだなぁと思いました．
なにか良い方法があればぜひ知りたいです．</p>

---

---
