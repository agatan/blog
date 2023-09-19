---
title: "Type Erasure による Visitor パターンの実装"
postSlug: Type_Erasure_による_Visitor_パターンの実装
pubDatetime: 2016-01-25T11:07:38.000Z
tags: ["C++"]
---

プログラミングしていて，木構造をうまく扱いたいという状況は結構良くあると思います．

代数的データ型とパターンマッチを持つ言語であればとても美しく完結に表現できる木構造ですが，オブジェクト指向言語でやろうと思うと結構たいへんです．

典型的には Visitor パターンというやつを用います．[デザインパターン - Visitor パターン再考 - Qiita](http://qiita.com/lyrical_logical/items/bc6126f34a571a2c4f97)が非常にわかりやすく，理解の助けになりました．ありがとうございます．

一方，C++ の有名なライブラリ，Boost には Boost.Variant というモジュールがあり，これまたとても美しく Visitor っぽいことが出来ます．

```
#include <boost/variant.hpp>
#include <string>
#include <iostream>

using sample = boost::variant<int, double, std::string>;

sample s1 = 1;
sample s2 = 2.0;
sample s3 = "sample3";

boost::apply\_visitor([](auto const& v) { std::cout << v << std::endl; }, s1); // => 1
boost::apply\_visitor([](auto const& v) { std::cout << v << std::endl; }, s2); // => 2.0
boost::apply\_visitor([](auto const& v) { std::cout << v << std::endl; }, s3); // => sample3

```

しかし，Boost.Variant は非常に高機能ですが，テンプレートをガンガン使っていたりするので，コンパイルコストが大きいという問題があります．

そこで，Type Erasure を使って visitor パターンをうまく表せれば，コンパイルコストを下げられるのでは？というお話です．

Type Erasure は「型消去」とかでググると色々解説してくださっている記事などが出てくると思います．（ありがとうございます）

この話，私が考えたわけではなくて，どこかのソースコードで見たようなきがするんですが，当時は Type Erasure とか意味不明だったのでスルーしていました．

今ならなんとなくやりたいことは出来るような気がするので（＆ちょうど必要になったので）記事にしてみていますが，もしオリジナルっぽいものや同じようなことを提案しているソースコード・記事を見かけた方は是非ご連絡いただけると嬉しいです．

## 1st step

### Visitor

今回表現したいデータ構造をまず定めます．簡単のために，足し算・掛け算・整数定数の 3 種類のノードを持つ木構造を考えます．

`(1 + 2) * 3` なら， `mul( add(1, 2), 3 )` みたいな感じです．

この構造を visit する Visitor クラスから先に考えます．

Visitor クラスは，`visit` というメンバ関数をもつ型の値を，型を消去して保持させます．

```
class visitor {
private:
  class visitor\_base\_holder {
  public:
    virtual void visit(add &) = 0;
    virtual void visit(mul &) = 0;
    virtual void visit(constant &) = 0;

    virtual ~visitor\_base\_holder() = default;
  };

  template <typename V> class visitor\_holder : public visitor\_base\_holder {
  private:
    V &v;

  public:
    visitor\_holder(V &v) : v(v) {}

    void visit(add &a) override { v(a); }
    void visit(mul &a) override { v(a); }
    void visit(constant &a) override { v(a); }

    virtual ~visitor\_holder() = default;
  };

  std::unique\_ptr<visitor\_base\_holder> holder;

public:
  template <typename V>
  visitor(V &v)
      : holder(std::make\_unique<visitor\_holder<V>>(v)) {}

  template <typename Visitable> void visit(Visitable &v) { holder->visit(v); }
};

```

今回は `const` 修飾についてすべて無視しています．( `const` を考慮するならば，各 `visit` について，visitor の `const` 性と node の `const` 性を考える必要があります．つまり 4 種類のメンバ関数を定義しなければなりません．）

visit した対象となるそれぞれのデータについてオーバーロードする形で `visit` を定義しています．

`visitor` のコンストラクタに，`operator()(add&)`, `operator()(mul&)`, `operator()(constant&)` を全て持つオブジェクト（C++14 のジェネリックラムダでも OK）を渡すことで，型消去された visitor が出来上がります．

`visitor` のコンストラクタにどんな型の値を渡しても，出来上がる `visitor` にはその型情報は含まれないので，様々な visitor を統一して扱う（ `vector` に突っ込むとか）事ができるようになります．

### Node

次にノードの方について考えます． 通常，Visitor パターンでは， visit される側のクラスに `accept` を実装します．

visit される側のデータを統一的に扱う（ `vector` に突っ込むとか）ためには，継承やインターフェースを用いるのが普通です．

C++ では，Visitor 側に使った Type Erasure のテクニックが使えます．

`std::vector<node>` などのように，統一的にノードを扱いつつも，visit される際には，`visit(add&)` や `visit(mul&)` のような適切なオーバーロード関数を呼び出すようにしてやればオッケーです．

```
class node {
private:
  class node\_base\_holder {
  public:
    virtual void accept(visitor &v) = 0;

    virtual ~node\_base\_holder() = default;
  };

  template <typename T> class node\_holder : public node\_base\_holder {
  public:
    node\_holder(T const &n) : node(n) {}
    node\_holder(T &&n) : node(n) {}

    void accept(visitor &v) override { v.visit(node); }

    ~node\_holder() = default;

  private:
    T node;
  };

  std::shared\_ptr<node\_base\_holder> holder;

public:
  template <typename Node>
  node(Node const &n)
      : holder(std::make\_shared<node\_holder<Node>>(n)) {}

  template <typename Node>
  node(Node &&n)
      : holder(std::make\_shared<node\_holder<Node>>(n)) {}

  void accept(visitor &v) { holder->accept(v); }

  template <typename Visitor> void accept(Visitor &v) {
    visitor visit(v);
    holder->accept(visit);
  }
};

```

これ結構わかりにくと思うのですが，自分でもコンパイラに怒られながら書いたのでいまいちよく分かってません．

先ほどの `visitor` の場合と異なり，`node` には特別満たすべきインターフェースは有りません．

Type Erasure を使う理由は，適切な `visit` 関数へのディスパッチのためです．

### 使う

`visitor` と `node` が出来たので，使ってみます．

その前にデータ構造を定義しておきます．

```
struct constant {
  int value;
};

struct add {
  node lhs;
  node rhs;
};

struct mul {
  node lhs;
  node rhs;
};

```

`add` や `mul` のフィールドに，`node` が使用されている点が大事です．

`add.lhs` や `mul.rhs` には，`constant` が来るか `add` が来るか `mul` が来るか分かりません．

そこで，visit 可能な型なら何でも OK という意味で，`node` 型の値をフィールドとします．

```
node n = mul{add{constant{1}, constant{2}}, constant{3}};

```

これで，`(1 + 2) * 3` が表現できています．
`add` や `constant` から `node` へと暗黙変換が行われていることに注意してください．

次に visitor を定義します．これは，`operator()` をオーバーロードした関数オブジェクトです．

式を出力する `printer` と 式を計算する `calculator` を定義します．

```
struct printer {
  void operator()(add &a) {
    std::cout << "(";
    a.lhs.accept(*this);
    std::cout << ")";
    std::cout << "+";
    std::cout << "(";
    a.rhs.accept(*this);
    std::cout << ")";
  }

  void operator()(mul &a) {
    std::cout << "(";
    a.lhs.accept(*this);
    std::cout << ")";
    std::cout << "*";
    std::cout << "(";
    a.rhs.accept(*this);
    std::cout << ")";
  }

  void operator()(constant &c) { std::cout << c.value; }
};

struct calculator {
  int result;
  void operator()(add &a) {
    calculator l, r;
    a.lhs.accept(l);
    a.rhs.accept(r);
    result = l.result + r.result;
  }

  void operator()(mul &m) {
    calculator l, r;
    m.lhs.accept(l);
    m.rhs.accept(r);
    result = l.result * r.result;
  }

  void operator()(constant &c) { result = c.value; }
};

```

こんな感じです．

`visit` や `accept` を `void` を返す関数として定義したので，`calculator` は自前のフィールドに結果を保持する必要があります．
(あとで改善します)

使い方は

```
  node n = mul{add{constant{1}, constant{2}}, constant{3}};
  printer p;
  n.accept(p);
  calculator calc;
  n.accept(calc);
  std::cout << std::endl;
  std::cout << calc.result << std::endl;
  return 0;

```

です．

# まとめ

この方法の利点としては，データの定義そのものに Visitor パターンのためのノイズが入らないことが挙げられます．

普通の Visitor パターンでは継承必須ですし．

`const` つけてないせいで一時オブジェクトが使えないので `printer p;` という行が必要になってしまっています．これは`const`をがんばってつけるだけなのでまぁ問題有りません．

一方，`calculator` の方はダサいですね．値を返す visitor も定義できるようにしたい．

`visitor` の定義もツライです．`const` を考慮した場合，同じような内容のメンバ関数を 4 回ずつ書く必要がある．

このへんの問題点は解決可能な気がするので出来たら後で記事にするつもりです．

難しすぎて普通の visitor パターンで良くね？感出てきた

---

---
