[![Build Status](https://travis-ci.org/great-h/auto_participant.png)](https://travis-ci.org/great-h/auto_participant)

# これは何

[すごい広島](http://great-h.github.io/) の参加者の一覧を Doorkeeper から取得して `_posts` ディレクトリに存在するイベントぺージに追加するためのプログラムです。

# インストール

```
$ git clone https://github.com/great-h/auto_participant
$ cd auto_participant
$ cabal install
```

実行例

```
$ auto_participant
```

# TODO

* 規則から外れる名前やURLを設定ファイルで記述しておくことで対応できるようにする
