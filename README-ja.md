# purescript-eta-conversion

イータ変換化をサポートするライブラリ

関数を元にイータ変換可能な新たな関数を生成します

## instration
Install with [Spago](https://github.com/purescript/spago):
```
spago install eta-conversion
```

## 動機
### 基本的な使い方
次のようなデータ構造`Result`および関数`fun`と、これらを使う`exampleA1`という関数があったとします。
```haskell
newtype Result a = Result a

fun :: String -> Int -> Boolean -> String
fun _ _ _ = ""

exampleA1 :: String -> Int -> Boolean -> Result String
exampleA1 s i b = Result $ fun s i b
```
`<<|`を使うことで、この`exampleA1`を次のようにイータ変換できるようになります。
```haskell
exampleA2 :: String -> Int -> Boolean -> Result String
exampleA2 = Result <<| fun
```