# csv-telescope

csvのデータを散布図で眺めるツール．

[GitHub Pages](https://yuyaaizawa.github.io/csv-telescope/)


## 使い方

- csv(ヘッダー付き)をテキストエリアに貼り付けてparseボタン
- 散布図上で
  - ドラッグ：平行移動
  - ホイール：拡大縮小
  - 点をクリック：パラメータ表示


## ビルド

```bash
elm make src/Main.elm --output=public/js/elm.js
```


## 予定

- 条件を指定して強調（着色）
- auto scale
- 範囲を指定（数字）して表示
- 拡大縮小の中心をマウスポインタの位置に
