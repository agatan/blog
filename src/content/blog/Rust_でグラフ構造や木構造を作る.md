---
title: "Rust でグラフ構造や木構造を作る"
postSlug: Rust_でグラフ構造や木構造を作る
pubDatetime: 2017-01-16T06:17:45.000Z
tags: ["Rust"]
---

プログラムを書いていると何かしら木構造っぽいものやグラフっぽいものを作りたい場面が多々あると思います．
Rust は所有権や `Size` の都合で，これらを作ろうと思うと地味にハマるのでまとめておきます．

### Rust で木構造

最も単純な木構造は Rust だと

```
enum Tree<T> {
    Leaf(T),
    Node(Box<Tree<T>>, Box<Tree<T>>),
}

```

といった形で表せます．
Rust では明示的に boxing してあげないと再帰的なデータ構造は作れないのでちょっと複雑に見えるかもしれませんが，まぁ単純です．

この木構造を書き換えたい場合は，ownership をとって書き換えた値を返すこともできますし，`&mut Tree<T>` をとって in-place に書き換えることもできます．

```
fn inc(&mut self) {
    match *self {
        Tree::Leaf(ref mut i) => *i = *i + 1,
        Tree::Node(ref mut left, ref mut right) => {
            left.inc();
            right.inc();
        }
    }
}

fn inc2(self) -> Tree<i32> {
    match self {
        Tree::Leaf(i) => Tree::Leaf(i + 1),
        Tree::Node(left, right) => Tree::Node(Box::new(left.inc2()), Box::new(right.inc2())),
    }
}

```

### Rust で有向非巡回グラフ

有向非巡回グラフ構造は，グラフのエッジに向きがあり，かつ循環がない構造で，これは割りと単純に表現できます．

```
struct Node<T>(T, Vec<Rc<Node<T>>>);

```

`Node` は自身の値 `T` と，つながっているノードの `Vec` を持ちます．(対象問題によっては `Vec` ではなくて `HashMap` とか `HashSet` とか)

`Rc` は参照カウント方式のスマートポインタです．
グラフでは，`Node` は複数の `Node` から参照される可能性があるので， `Box` は使えません．

これを変更可能にしたい場合はちょっと面倒ですが `RefCell` を使う必要があります．

Rust では基本的に mutable borrow は常にひとつしか存在できず，mutable borrow が生きている間は immutable borrow もつくることができません．
`Rc` から mutable な参照を取り出すこともできません．

そこで `RefCell` を使うことで borrow check をランタイムに行うようにします．
`RefCell` は immutable な参照から mutable な参照を取り出せるようにする働きをしますが，
mutable な参照を取っている間に，さらに mutable な参照を作ろうとしたり immutable な参照を作ろうとすると，ランタイムに `panic` します．
コンパイル時検査ではなくランタイム検査になるので，プログラマの責任できちんと管理しないと死にます．

`RefCell` 版がこちら

```
struct Node<T>(T, Vec<Rc<RefCell<Node<T>>>>);

impl Node<i32> {
    fn inc(&mut self) {
        self.0 += 1;
        for n in &self.1 {
            n.borrow\_mut().inc();
        }
    }
}

```

### Rust で巡回有向グラフ

循環がある場合は厄介です．
Rust で参照を共有するための `Rc` は参照カウントなので，循環参照があるとリークします．
したがって循環のある構造を表すために `Rc` は使えません．

木構造で親を参照するポインタを子に持たせておきたいといったケースでは，親は子を `Rc` で持ち，子は親を `Weak` で持つという形で対応できますが，
グラフだとそういうわけにもいきません．

そこで出て来るのが `Arena` という方法です．

オブジェクトの実体は `Arena` の中に作り，グラフにはその ID を持たせて管理します．

```
type NodeId = usize;

struct NodeArena<T> {
    arena: Vec<Node<T>>,
}

impl<T> NodeArena {
    fn alloc(&mut self, value: T) -> NodeId {
        let id = self.arena.len();
        let node = Node(id, value, Vec::new());
        self.arena.push(node);
        id
    }
    fn get(&self, id: NodeId) -> &Node<T> {
        &self.arena[id]
    }
    fn get\_mut(&mut self, id: NodeId) -> &mut Node<T> {
        &mut self.arena[id]
    }
}

struct Node<T>(NodeId, T, Vec<NodeId>);

```

こんな感じです．

こうすることでコンパイル時の borrow check を諦めることなくグラフ構造を作ることができます．

(`i` と `i+1` 番目のノードを同時に mutable に参照したいとかは苦しいですが)

欠点としては単純に間接的な表現でめんどくさいというのもありますが，GC がないので参照されなくなったオブジェクトも `Arena` 上で生き続けてしまうことです．
そのため，動的に要素が生きたり死んだりするケースには使いにくいです．

ノードグラフの構築が終わったら `Arena` をリフレッシュするみたいなことをすると良いのかもしれません．
(構築中は `Vec` で `Arena` を表現して，構造が固まったら `HashMap` を使った `Arena` に切り替えて参照されている id だけを残すみたいな)

それでも構築中はオブジェクトがあまりまくるので辛いですね...

---

---
