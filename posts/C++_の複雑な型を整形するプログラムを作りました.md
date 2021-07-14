---
title: "C++ の複雑な型を整形するプログラムを作りました"
date: 2016-03-08T11:55:10.000Z
tags: ["C++"]
---

テンプレートをバリバリ使っている C++ プログラムのコンパイルエラーが，死ぬほど辛かったので作りました．
型を綺麗に出力するだけです．
C++の型版 `jq` みたいなやつありそうだけど無いのかな？

[agatan/tf](https://github.com/agatan/tf)

たとえば，

```
boost::spirit::x3::raw\_directive<boost::spirit::x3::lexeme\_directive<boost::spirit::x3::sequence<boost::spirit::x3::alternative<boost::spirit::x3::char\_class<boost::spirit::char\_encoding::standard, boost::spirit::x3::alpha\_tag>, boost::spirit::x3::literal\_char<boost::spirit::char\_encoding::standard, boost::spirit::x3::unused\_type> >, boost::spirit::x3::kleene<boost::spirit::x3::alternative<boost::spirit::x3::char\_class<boost::spirit::char\_encoding::standard, boost::spirit::x3::alnum\_tag>, boost::spirit::x3::literal\_char<boost::spirit::char\_encoding::standard, boost::spirit::x3::unused\_type> > > > > >::parse<\_\_gnu\_cxx::\_\_normal\_iterator<const char *, std::\_\_cxx11::basic\_string<char> >, boost::spirit::x3::context<boost::spirit::x3::error\_handler\_tag, const std::reference\_wrapper<boost::spirit::x3::error\_handler<\_\_gnu\_cxx::\_\_normal\_iterator<const char *, std::\_\_cxx11::basic\_string<char> > > >, boost::spirit::x3::context<boost::spirit::x3::skipper\_tag, const boost::spirit::x3::char\_class<boost::spirit::char\_encoding::ascii, boost::spirit::x3::space\_tag>, boost::spirit::x3::unused\_type> >, std::\_\_cxx11::basic\_string<char>, char>

```

こんなエラーがよく有りますよね．

これを `tf` の標準入力に流しこむと，

```
boost::spirit::x3::raw\_directive<
    boost::spirit::x3::lexeme\_directive<
        boost::spirit::x3::sequence<
            boost::spirit::x3::alternative<
                boost::spirit::x3::char\_class<
                    boost::spirit::char\_encoding::standard,
                    boost::spirit::x3::alpha\_tag
                >,
                boost::spirit::x3::literal\_char<
                    boost::spirit::char\_encoding::standard,
                    boost::spirit::x3::unused\_type
                >
            >,
            boost::spirit::x3::kleene<
                boost::spirit::x3::alternative<
                    boost::spirit::x3::char\_class<
                        boost::spirit::char\_encoding::standard,
                        boost::spirit::x3::alnum\_tag
                    >,
                    boost::spirit::x3::literal\_char<
                        boost::spirit::char\_encoding::standard,
                        boost::spirit::x3::unused\_type
                    >
                >
            >
        >
    >
>::parse<
    \_\_gnu\_cxx::\_\_normal\_iterator<
        constchar*,
        std::\_\_cxx11::basic\_string<
            char
        >
    >,
    boost::spirit::x3::context<
        boost::spirit::x3::error\_handler\_tag,
        conststd::reference\_wrapper<
            boost::spirit::x3::error\_handler<
                \_\_gnu\_cxx::\_\_normal\_iterator<
                    constchar*,
                    std::\_\_cxx11::basic\_string<
                        char
                    >
                >
            >
        >,
        boost::spirit::x3::context<
            boost::spirit::x3::skipper\_tag,
            constboost::spirit::x3::char\_class<
                boost::spirit::char\_encoding::ascii,
                boost::spirit::x3::space\_tag
            >,
            boost::spirit::x3::unused\_type
        >
    >,
    std::\_\_cxx11::basic\_string<
        char
    >,
    char
>

```

こうなります．

単純に `<`, `>`, `,` を見てインデントを調整しながら出力しているだけです．
空白はスキップします．

構文解析とかは全くしていないので，コンパイルエラーをそのまま流し込んでも悲惨な事になります．
~~あと今気がついたのですが，`const hoge` が `consthoge` になっていますね．~~
修正しました

```
boost::spirit::x3::raw\_directive<
    boost::spirit::x3::lexeme\_directive<
        boost::spirit::x3::sequence<
            boost::spirit::x3::alternative<
                boost::spirit::x3::char\_class<
                    boost::spirit::char\_encoding::standard,
                    boost::spirit::x3::alpha\_tag
                >,
                boost::spirit::x3::literal\_char<
                    boost::spirit::char\_encoding::standard,
                    boost::spirit::x3::unused\_type
                >
            >,
            boost::spirit::x3::kleene<
                boost::spirit::x3::alternative<
                    boost::spirit::x3::char\_class<
                        boost::spirit::char\_encoding::standard,
                        boost::spirit::x3::alnum\_tag
                    >,
                    boost::spirit::x3::literal\_char<
                        boost::spirit::char\_encoding::standard,
                        boost::spirit::x3::unused\_type
                    >
                >
            >
        >
    >
>::parse<
    \_\_gnu\_cxx::\_\_normal\_iterator<
        const char *,
        std::\_\_cxx11::basic\_string<
            char
        >
    >,
    boost::spirit::x3::context<
        boost::spirit::x3::error\_handler\_tag,
        const std::reference\_wrapper<
            boost::spirit::x3::error\_handler<
                \_\_gnu\_cxx::\_\_normal\_iterator<
                    const char *,
                    std::\_\_cxx11::basic\_string<
                        char
                    >
                >
            >
        >,
        boost::spirit::x3::context<
            boost::spirit::x3::skipper\_tag,
            const boost::spirit::x3::char\_class<
                boost::spirit::char\_encoding::ascii,
                boost::spirit::x3::space\_tag
            >,
            boost::spirit::x3::unused\_type
        >
    >,
    std::\_\_cxx11::basic\_string<
        char
    >,
    char
>

```

---

---
