---
title: "C++ のテンプレートの実装"
postSlug: C++_のテンプレートの実装
pubDatetime: 2016-06-13T07:59:27.000Z
tags: ["C++"]
---

C++ のテンプレートがなぜ必要で，どんな構文・種類のものがあるかについては前回までにまとめました。

というわけで次は C++ ではテンプレートという機能を使用するとどんなバイナリが生成されるのかについて見ていきます。

C++ のテンプレートの強力さとか勘所みたいなものを把握するために非常に重要な部分なので、覚えておくとよいと思います。

## C++ でテンプレートをコンパイルしてみる

早速ですが、実際に C++ でテンプレートを使っているコードをコンパイルしてみます。

アセンブリよりも LLVM IR のほうがわかりやすいかな？と思うので、LLVM IR を clang++ で生成させてみます。
（LLVM IR については [大学院生のための LLVM | インフラ・ミドルウェア | POSTD](http://postd.cc/llvm-for-grad-students/) あたりを読んでおくとなんとなく概念がつかめると思います。公式は[LLVM Language Reference Manual — LLVM 3.9 documentation](http://llvm.org/docs/LangRef.html)）

対象となるコードはこちら。

```
// main.cpp
template <typename T>
T identity(T x) {
  return x;
}

int main() {
  float f = 0.0f;
  identity(f);

  int d = 0;
  identity(d);
  return 0;
}

```

clang++ で LLVM IR を生成させるには，`-S -emit-llvm` をオプションに指定します。また、今回のコードは最適化されてしまうとほとんどコードが残らないので、最適化を抑制するよう、`-O0` を付けます。

```
$ clang++ -O0 -S -emit-llvm main.cpp
```

すると `main.ll` というファイルが出来ています。これが LLVM IR です。

## IR を読む

LLVM IR は，LLVM というコンパイラ基盤技術における中間表現 (Intermediate Representation) です。

ざっくり言うと、アーキテクチャに依存しない、読みやすいアセンブリです。

`main.ll` はそこまで長くないですが、エッセンスだけ抜粋します。

```
define i32 @main() #0 { ;; main 関数
  %1 = alloca i32, align 4
  %f = alloca float, align 4
  %d = alloca i32, align 4
  store i32 0, i32* %1
  store float 0.000000e+00, float* %f, align 4
  %2 = load float* %f, align 4
  %3 = call float @\_Z8identityIfET\_S0\_(float %2)
  store i32 0, i32* %d, align 4
  %4 = load i32* %d, align 4
  %5 = call i32 @\_Z8identityIiET\_S0\_(i32 %4)
  ret i32 0
}

define linkonce\_odr float @\_Z8identityIfET\_S0\_(float %x) #1 { ;; identity<float> の実体
  %1 = alloca float, align 4
  store float %x, float* %1, align 4
  %2 = load float* %1, align 4
  ret float %2
}

define linkonce\_odr i32 @\_Z8identityIiET\_S0\_(i32 %x) #1 { ;; identity<int> の実体
  %1 = alloca i32, align 4
  store i32 %x, i32* %1, align 4
  %2 = load i32* %1, align 4
  ret i32 %2
}
```

コメントでも書きましたが、3 つの関数が定義されていることがわかると思います。
ここで重要なのは、 **identity<int>と identity<float>がそれぞれ別の関数として定義されている** ことです。

`identity<int>` と `identity<float>` は `main` の中で使われています。

つまり、テンプレートは、「使った分だけ実体が作られる。かつその処理はコンパイル時に終わる。」ということがわかります。

たとえばソースコード中に `identity<bool>` の実体化を要求するコードがあれば、その時はじめて `identity<bool>` が作られます。
独自定義でも構いません。`identity<MyClass>` の実体化を要求するコードがあれば、その時はじめて `identity<MyClass>` が作られます。

もちろん、一度実体化されたテンプレートは再利用されます。つまり、`identity<int>` を要求するコードが、一つのソースコードに何度現れても、ただひとつの `identity<int>` が生成されます。

## Java との比較

Java にもジェネリクスという仕組みがあります。

概念的にはテンプレートに似たものなので、比較してみます (テンプレートの方がより強力ですが、型を汎用化したいという目的であれば、両者とも同様に使用できます。)

同じようなコードをコンパイルしてバイトコードを見てみます。

```
class Main {
  static <T> T identity(T x) {
    return x;
  }

  public static void main(String[] args) {
    Integer d = 1;
    Float f = 0.0f;
    Main.identity(f);
    Main.identity(d);
  }
}

```

`javac Main.java` してから、`javap -v Main` します。これでバイトコードが出力されます。

```
class Main
/* 中略 */
{
  Main();
    descriptor: ()V
    flags:
    Code:
      stack=1, locals=1, args\_size=1
         0: aload\_0
         1: invokespecial #1                  // Method java/lang/Object."<init>":()V
         4: return
      LineNumberTable:
        line 1: 0

  static <T extends java.lang.Object> T identity(T);
    descriptor: (Ljava/lang/Object;)Ljava/lang/Object;
    flags: ACC\_STATIC
    Code:
      stack=1, locals=1, args\_size=1
         0: aload\_0
         1: areturn
      LineNumberTable:
        line 3: 0
    Signature: #14                          // <T:Ljava/lang/Object;>(TT;)TT;

  public static void main(java.lang.String[]);
    descriptor: (Ljava/lang/String;)V
    flags: ACC\_PUBLIC, ACC\_STATIC
    Code:
      stack=1, locals=3, args\_size=1
         0: iconst\_1
         1: invokestatic  #2                  // Method java/lang/Integer.valueOf:(I)Ljava/lang/Integer;
         4: astore\_1
         5: fconst\_0
         6: invokestatic  #3                  // Method java/lang/Float.valueOf:(F)Ljava/lang/Float;
         9: astore\_2
        10: aload\_2
        11: invokestatic  #4                  // Method identity:(Ljava/lang/Object;)Ljava/lang/Object;
        14: pop
        15: aload\_1
        16: invokestatic  #4                  // Method identity:(Ljava/lang/Object;)Ljava/lang/Object;
        19: pop
        20: return
      LineNumberTable:
        line 7: 0
        line 8: 5
        line 9: 10
        line 10: 15
        line 11: 20
}
SourceFile: "Main.java"
```

注目すべきは `// Method identity:(Ljava/lang/Object;)Ljava/lang/Object;` というコメントのついた行です。

2 行ありますが、それぞれ `identity(d)` と `identity(f)` に相当します。

`Integer.valueOf` や `Float.valueOf` を含むコメントを見ていただければわかると思いますが、このコメント部分には呼び出しているメソッドの型が記されています。

つまり、`identity` は `Integer` で呼んでも `Float` で呼んでも `Object identity(Object)` を呼んでいるということです。

これは [Java のジェネリクスの大きな特徴で型消去などと呼ばれる性質です。ジェネリクスによる型はすべてコンパイル時にのみ利用され、実行時にはすべて `Object` として表現しつつ適切にキャストを挟むような構造になっています。

キャストはキャストでも、正しいことがコンパイラによって保証されたキャストになるので、`List` よりも `List<String>` のほうが安全というわけです。

## それぞれの利点と欠点

テンプレートやジェネリクスを実現する方法として、2 つの例を上げました。

一つは C++ の採用している方式で、テンプレート引数ごとに新しく実体を作ってしまう方式です。

もう一つは Java の採用している方式で、`Object` のようにすべての型を受け取れる基底クラスのようなものを用いて、実行時には型情報を残さない方式です。

今回はたまたま C++ と Java を例にあげましたが、他の言語でもこのような方式を使っている言語は多いです。（みんなだいすき D 言語は C++ の方式を採用しています）

さてそれぞれの利点と欠点についてです。

### Java 方式

- 利点
  - 分割コンパイルが容易（ジェネリックな関数を提供する側は `Object` を入れたバイナリやバイトコードと、型情報だけを生成すればよい。使用する側は、型情報から型検査を行った後、`Object` へのキャストなどを含めたバイナリやバイトコードをはけば良い）
- 欠点
  - 実行時にやることが増えるのでオーバヘッドがある

### C++ 方式

- 利点
  - 実行時オーバヘッドなし（全てコンパイル時に解決される）
- 欠点
  - バイナリサイズの増加
  - 分割コンパイルが困難 (テンプレートを実体化しようと思うと、型情報だけでは足りない。使用者側が定義そのものをまるまる知っている必要がある。)

こんな感じでしょうか。

この比較はあくまで型を汎用化したいという目的に関しての比較です。C++ のテンプレートにできて Java のジェネリクスに出来ないことはたくさんあります。

## まとめ

C++er はみんな実行時のオーバヘッドが嫌いです。テンプレートは、今までに紹介してきた使用法からは想像も出来ないほど豊富な計算を、コンパイル時にすべて行うことが出来ます。実行時のオーバヘッドなしで。

コンパイル時にテンプレートの解決が終わるということは、強力な最適化が望めるということでもあります。つまり、実行時のキャストといったわかりやすいオーバヘッド以上に、実行速度には差が生まれるでしょう。

というわけで今回はテンプレートの実現方法について、Java と比較しながら説明してみました。

---

---
