#### 使い方

```
credentials += Credentials("GitHub Package Registry", "maven.pkg.github.com", "《GitHubのユーザ名》", sys.env("GITHUB_TOKEN"))

resolvers += "GitHub Package Registry (waman/multiverse)" at "https://maven.pkg.github.com/waman/multiverse"
libraryDependencies += "org.waman" % "multiverse" % "《バージョン》"
```

#### 基本的な使い方
multiverse ライブラリでは、数値に「(物理)単位」を付けたものを「量」とします。
「量」と「単位」はそれぞれ multiverse 固有の型`Quantity`,`PhysicalUnit`で表され、
数値は [spire](https://github.com/typelevel/spire) の`Fractional`オブジェクトで表されます。

`multiverse.implicits`パッケージ下のメンバをインポートすることにより、
数値に対して`apply(PhysicalUnit)`メソッドを呼び出して
`Quantity`オブジェクトを生成することができます：
```
import multiverse.implicits._

// 長さの PhysicalUnit オブジェクト（LengthUnit オブジェクト）をインポート
import multiverse.unit.basic.LengthUnits._

// 2メートルの「量」（ Quantity オブジェクト、Length オブジェクト）を生成
val twoMetre = 2.0(m)
println(twoMetre)  // 「2.0(m)」と表示
```
`Quantity`オブジェクトはまた`apply(PhysicalUnit)`メソッドを持ち （ただし、引数の型は制限あり）、
「量」を指定された「単位」に換算した数値を返します：
```
import multiverse.implicits._
import multiverse.unit.basic.LengthUnits._

// 2マイル（国際マイル）の Length オブジェクトを生成
val twoMile = 2.0(mi)

// メートルに換算した値を取得
val v: Double = twoMile(m)
println(v)  // 「3218.688」と表示

// 1行で書くと
println(2.0(mi)(m))  // 「3218.688」
```
2つの`apply`メソッドの使い方に注意。

##### 注1
`Quantity`のサブタイプは`Length`や`Mass`、
`PhysicalUnit`のサブタイプは`LengthUnit`や`MassUnit`という名前になっています。

##### 注
整数は精度を保つために実数 (`spire.math.Real`) として扱われます：
```
import multiverse.implicits._
import multiverse.unit.basic.LengthUnits._

val twoMile = 2(mi)
val v: spire.math.Real = twoMile(km)
println(v)  // 「50292/15625」と表示
```

##### 属性 Attribute

#### 単位オブジェクト

#### 物理量 Quantity
##### 演算

#### 単位系オブジェクト UnitSystem

