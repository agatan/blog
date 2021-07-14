---
title: "C++ の複雑な型を整形するプログラムを作りました"
date: 2016-03-08T11:55:10.000Z
tags: ["C++"]
---

<p>テンプレートをバリバリ使っている <a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> プログラムの<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>エラーが，死ぬほど辛かったので作りました．
型を綺麗に出力するだけです．
<a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a>の型版 <code>jq</code> みたいなやつありそうだけど無いのかな？</p>

<p><a href="https://github.com/agatan/tf">agatan/tf</a></p>

<p>たとえば，</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink>boost::spirit::x3::raw_directive&lt;boost::spirit::x3::lexeme_directive&lt;boost::spirit::x3::sequence&lt;boost::spirit::x3::alternative&lt;boost::spirit::x3::char_class&lt;boost::spirit::char_encoding::standard, boost::spirit::x3::alpha_tag&gt;, boost::spirit::x3::literal_char&lt;boost::spirit::char_encoding::standard, boost::spirit::x3::unused_type&gt; &gt;, boost::spirit::x3::kleene&lt;boost::spirit::x3::alternative&lt;boost::spirit::x3::char_class&lt;boost::spirit::char_encoding::standard, boost::spirit::x3::alnum_tag&gt;, boost::spirit::x3::literal_char&lt;boost::spirit::char_encoding::standard, boost::spirit::x3::unused_type&gt; &gt; &gt; &gt; &gt; &gt;::parse&lt;__gnu_cxx::__normal_iterator&lt;<span class="synType">const</span> <span class="synType">char</span> *, std::__cxx11::basic_string&lt;<span class="synType">char</span>&gt; &gt;, boost::spirit::x3::context&lt;boost::spirit::x3::error_handler_tag, <span class="synType">const</span> std::reference_wrapper&lt;boost::spirit::x3::error_handler&lt;__gnu_cxx::__normal_iterator&lt;<span class="synType">const</span> <span class="synType">char</span> *, std::__cxx11::basic_string&lt;<span class="synType">char</span>&gt; &gt; &gt; &gt;, boost::spirit::x3::context&lt;boost::spirit::x3::skipper_tag, <span class="synType">const</span> boost::spirit::x3::char_class&lt;boost::spirit::char_encoding::ascii, boost::spirit::x3::space_tag&gt;, boost::spirit::x3::unused_type&gt; &gt;, std::__cxx11::basic_string&lt;<span class="synType">char</span>&gt;, <span class="synType">char</span>&gt;
</pre>

<p>こんなエラーがよく有りますよね．</p>

<p>これを <code>tf</code> の標準入力に流しこむと，</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink>boost::spirit::x3::raw_directive&lt;
    boost::spirit::x3::lexeme_directive&lt;
        boost::spirit::x3::sequence&lt;
            boost::spirit::x3::alternative&lt;
                boost::spirit::x3::char_class&lt;
                    boost::spirit::char_encoding::standard,
                    boost::spirit::x3::alpha_tag
                &gt;,
                boost::spirit::x3::literal_char&lt;
                    boost::spirit::char_encoding::standard,
                    boost::spirit::x3::unused_type
                &gt;
            &gt;,
            boost::spirit::x3::kleene&lt;
                boost::spirit::x3::alternative&lt;
                    boost::spirit::x3::char_class&lt;
                        boost::spirit::char_encoding::standard,
                        boost::spirit::x3::alnum_tag
                    &gt;,
                    boost::spirit::x3::literal_char&lt;
                        boost::spirit::char_encoding::standard,
                        boost::spirit::x3::unused_type
                    &gt;
                &gt;
            &gt;
        &gt;
    &gt;
&gt;::parse&lt;
    __gnu_cxx::__normal_iterator&lt;
        constchar*,
        std::__cxx11::basic_string&lt;
            <span class="synType">char</span>
        &gt;
    &gt;,
    boost::spirit::x3::context&lt;
        boost::spirit::x3::error_handler_tag,
        conststd::reference_wrapper&lt;
            boost::spirit::x3::error_handler&lt;
                __gnu_cxx::__normal_iterator&lt;
                    constchar*,
                    std::__cxx11::basic_string&lt;
                        <span class="synType">char</span>
                    &gt;
                &gt;
            &gt;
        &gt;,
        boost::spirit::x3::context&lt;
            boost::spirit::x3::skipper_tag,
            constboost::spirit::x3::char_class&lt;
                boost::spirit::char_encoding::ascii,
                boost::spirit::x3::space_tag
            &gt;,
            boost::spirit::x3::unused_type
        &gt;
    &gt;,
    std::__cxx11::basic_string&lt;
        <span class="synType">char</span>
    &gt;,
    <span class="synType">char</span>
&gt;
</pre>

<p>こうなります．</p>

<p>単純に <code>&lt;</code>, <code>&gt;</code>, <code>,</code> を見てインデントを調整しながら出力しているだけです．
空白はスキップします．</p>

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/%B9%BD%CA%B8%B2%F2%C0%CF">構文解析</a>とかは全くしていないので，<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>エラーをそのまま流し込んでも悲惨な事になります．
<del>あと今気がついたのですが，<code>const hoge</code> が <code>consthoge</code> になっていますね．</del>
修正しました</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink>boost::spirit::x3::raw_directive&lt;
    boost::spirit::x3::lexeme_directive&lt;
        boost::spirit::x3::sequence&lt;
            boost::spirit::x3::alternative&lt;
                boost::spirit::x3::char_class&lt;
                    boost::spirit::char_encoding::standard,
                    boost::spirit::x3::alpha_tag
                &gt;,
                boost::spirit::x3::literal_char&lt;
                    boost::spirit::char_encoding::standard,
                    boost::spirit::x3::unused_type
                &gt;
            &gt;,
            boost::spirit::x3::kleene&lt;
                boost::spirit::x3::alternative&lt;
                    boost::spirit::x3::char_class&lt;
                        boost::spirit::char_encoding::standard,
                        boost::spirit::x3::alnum_tag
                    &gt;,
                    boost::spirit::x3::literal_char&lt;
                        boost::spirit::char_encoding::standard,
                        boost::spirit::x3::unused_type
                    &gt;
                &gt;
            &gt;
        &gt;
    &gt;
&gt;::parse&lt;
    __gnu_cxx::__normal_iterator&lt;
        <span class="synType">const</span> <span class="synType">char</span> *,
        std::__cxx11::basic_string&lt;
            <span class="synType">char</span>
        &gt;
    &gt;,
    boost::spirit::x3::context&lt;
        boost::spirit::x3::error_handler_tag,
        <span class="synType">const</span> std::reference_wrapper&lt;
            boost::spirit::x3::error_handler&lt;
                __gnu_cxx::__normal_iterator&lt;
                    <span class="synType">const</span> <span class="synType">char</span> *,
                    std::__cxx11::basic_string&lt;
                        <span class="synType">char</span>
                    &gt;
                &gt;
            &gt;
        &gt;,
        boost::spirit::x3::context&lt;
            boost::spirit::x3::skipper_tag,
            <span class="synType">const</span> boost::spirit::x3::char_class&lt;
                boost::spirit::char_encoding::ascii,
                boost::spirit::x3::space_tag
            &gt;,
            boost::spirit::x3::unused_type
        &gt;
    &gt;,
    std::__cxx11::basic_string&lt;
        <span class="synType">char</span>
    &gt;,
    <span class="synType">char</span>
&gt;
</pre>

---

---
