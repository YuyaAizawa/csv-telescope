# csv-telescope

csvのデータを散布図で眺めるツール．

[GitHub Pages](https://yuyaaizawa.github.io/csv-telescope/)


## 使い方

- csv(ヘッダー付き)をテキストエリアに貼り付けてparseボタン
- 散布図上でドラッグ，ホイールで平行移動，拡大縮小


## ビルド

```bash
elm make src/Main.elm --output=public/js/elm.js
```


## 予定

- 点をクリックしてパラメータ表示
- 条件を指定して強調（着色）
