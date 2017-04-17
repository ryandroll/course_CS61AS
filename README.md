# CS61AS Note and homework

## Why CS61AS
受到[如何掌握程序语言](http://yinwang0.lofter.com/post/183ec2_47bea8)這篇文章的影響，身為自學程式的愛好者，是該了解scheme來知道程式設計的精髓，而不是只是知道怎麼用各種語言寫迴圈。

說到學 scheme，相信大家都聽過 SICP 大名，但是這本書實在太不平易近人了，我覺得最大問題是在於，作者舉的例子太包羅萬象，一下數學、一下電路，對於要搞懂程式如何作用的我們，還要再分心搞懂這些東西，實在是很難吞下去。

於是我找到了[Berkley CS61AS](http://berkeley-cs61as.github.io/)，Berkley 版的SICP，雖然 MIT 已經放棄教 scheme 但是 Berkley 還沒有放棄，不過不是必修，而是可選的。但是對於想要了解 SICP 的人來說，是一個不錯入手的教材，有經過適當簡化，之後再回去翻 SICP 也比較容易理解。

## 環境設定
剛開始學習語言，最容易被打擊的就是環境設定了，這邊分享一下我的設定。我使用的是 macOS。

安裝 racket：使用 "brew cask install racket"

安裝 racket 的 berkley 套件：會用到 berkley 提供的函數，下載這個 repo 的 berkley.zip 後，在終端輸入 "sudo raco pkg install berkeley"。

編輯器：受到[ Scheme 编程环境的设置](http://www.yinwang.org/blog-cn/2013/04/11/scheme-setup)啟發，這邊推薦[ spacemacs ](https://github.com/syl20bnr/spacemacs) 的 evil mode，比 Rakcet 自帶的編輯器好用多了，不過要稍微調教以及學習一下。
首先安裝[ raket layers ](https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Blang/racket)，[並安裝 lispy 套件](https://github.com/abo-abo/lispy/pull/174)，把文檔的 clojure 改成 racket 。

emacs 生存法則：上古神器是很難上手，不過 spacemacs 已經大幅簡化難度。只要知道：
- 開啟跟關閉檔案可以直接按滑鼠，當然，這是有快捷鍵的，之後可以慢慢查。
- [简明 VIM 练级攻略](https://github.com/abo-abo/lispy)的第一級：生存
- [lispy github](https://github.com/abo-abo/lispy)，在insert模式中，左括弧前右括號後按，">","<","m","r"的作用就很好用了。
- 依序按下 SPC m s B 就可以把你寫好的檔案呼叫 repl 執行。
- 要再深入再慢慢讀各種文檔，但是知道上面三個就可以生存下來了。

## 課程相關

網頁中提供的測試程式與作業框架有些問題，可能會跳出執行時錯誤，如果有問題可以使用[這個人](https://github.com/nickyfoto/61as)的 grader 與解答當框架。

了解以上這些，可以少走許多冤枉路，我們就可以來專心學程式設計的精髓吧。
