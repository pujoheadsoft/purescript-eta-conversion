# purescript-eta-conversion

イータ変換化をサポートするライブラリ

関数を元にイータ変換可能な新たな関数を生成します

## instration
Install with [Spago](https://github.com/purescript/spago):
```
spago install eta-conversion
```

## 使い方
### 基本的な使い方
次のようなデータ構造`Result`および関数`fun`と、これらを使う`exampleA1`という関数があったとします。
```haskell
newtype Result a = Result a

fun :: String -> Int -> Boolean -> String
fun _ _ _ = ""

exampleA1 :: String -> Int -> Boolean -> Result String
exampleA1 s i b = Result $ fun s i b
```
この`exampleA1`関数に対し中置演算子`<<|`を使うことで次のようにイータ変換できるようになります。
```haskell
import Data.EtaConversionTransformer ((<<|))

exampleA2 :: String -> Int -> Boolean -> Result String
exampleA2 = Result <<| fun
```
これは次のように書いた場合と同じ動きをします。
```haskell
exampleA2 = \s i b -> Result $ fun s i b
```
すなわち`a <<| f`は、関数`f`と同じ引数を持ち、それらの引数を`f`に適用した結果を関数`a`に渡した結果を返す新たな関数を生成します。

`<<|`は引数が一つの場合、次のような定義になります。
```haskell
(o -> ret) -> (a1 -> o) -> (a1 -> ret)
```
引数が二つの場合は、次のような定義になります。
```haskell
(o -> ret) -> (a1 -> a2 -> o) -> (a1 -> a2 -> ret)
```
引数が三つ以上になった場合も上記と同様のパターンで定義の引数が増えていきます。

`<<|`は最大9つまでの引数に対応しています。

### 「関数を持つデータ構造を生成する関数」を生成する
次のようにデータ構造`Result`,`Functions`と、`Functions`の内容を取り出す関数`runFunctions`と、これらを利用する関数`exampleB1`があるとします。
```haskell
newtype Result a = Result a

newtype Functions = Functions {
  fun :: String -> Int -> Boolean -> String
}

runFunctions :: Functions -> { fun :: String -> Int -> Boolean -> String }
runFunctions (Functions r) = r

exampleB1 :: String -> Int -> Boolean -> Result (Functions -> String)
exampleB1 s i b = Result $ (\f -> f.fun s i b) <<< runFunctions
```
中置演算子`<<:`を使うと、この`exampleB1`を次のように定義することができます。
```haskell
import Data.EtaConversionTransformer ((<<:))

exampleB2 :: String -> Int -> Boolean -> Result (Functions -> String)
exampleB2 = Result <<: _.fun <<< runFunctions
```
### 「環境から関数を取り出して使うようなReaderTを生成する関数」を生成する
次のように関数を持つレコードを環境とし、その関数を内部で使うようなReaderTを生成したいとします。
```haskell
exampleC1 :: forall r m. TypeEquals r { fun :: String -> m Unit } => String -> ReaderT r m Unit
exampleC1 s = ReaderT $ \r -> (to r).fun s
```
このような関数は`readerT`関数を使うことで次のように書くことができます。
```haskell
import Data.ReaderTEtaConversionTransformer (readerT)

exampleC2 :: forall r m. TypeEquals r { fun :: String -> m Unit } => String -> ReaderT r m Unit
exampleC2 = readerT _.fun
```