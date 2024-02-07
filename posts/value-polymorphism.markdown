---
date: 2011-10-16
title: “Value polymorphism”, simple explanation with examples
author: Chris Done
tags: haskell, types
---

A concept in Haskell which is particularly novel is that polymorphism
works at the value level rather than function-parameter or
object-dereference level.

Function-parameter polymorphism comes in some different forms, for
example, C++:

``` cpp
void draw(Circle c){ … }
void draw(Triangle t){ … }
draw(circle); // draws a circle
```

Function overloading is a type of function-parameter
polymorphism. Generic functions in Common Lisp are another way to have
function-parameter polymorphism:

``` commonlisp
(defgeneric draw (shape))
(defmethod draw ((shape circle)) …)
(defmethod draw ((shape triangle)) …)
(draw circle) ;; draws a circle
```

Object-dereference (or message passing) polymorphism is common to most
object oriented languages. Depending on the object, the function/message will
do something different:

``` cpp
class Circle { void draw(){ … } }
class Triangle { void draw(){ … } }
circle.draw(); // draws a circle
```

To avoid confusion, Haskell also has function parameter
polymorphism, like C++ and Common Lisp above:

``` haskell
class Drawable a where draw :: a -> Bitmap
instance Drawable Circle where draw = …
instance Drawable Triangle where draw = …
draw circle -- draws a circle
```

But more generally, Haskell has *value* polymorphism, which is that
any value can be polymorphic and will be instantiated to a class
instance depending on type signature or annotation:

``` haskell
class Default a where def :: a
instance Default Int where def = 0
instance Default Char where def = 'a'
```

The type of an expression `def` therefore is `Default a => a`, or,
“any instance of `Default`”. I can instantiate an instance myself by
specifying a type signature:

``` haskell
λ> def :: Int
→ 0
λ> def :: Char
→ 'a'
```

Or by type inference, meaning that the combination of this expression
with other expressions allows the compiler to infer the single correct
type instance:

``` haskell
λ> def : "bc"
→ "abc"
λ> def - 2
→ -2
λ> def == 0
→ True
```

But with no information it will be a static compile error:

    λ> def
    Ambiguous type variable `a' in the constraint:
      `Default a' arising from a use of `def' at
        <interactive>:1:0-2
    Probable fix: add a type signature that fixes these type
                  variable(s)

Why is value polymorphism beneficial? Some trivial examples follow
(and you are trusted to extrapolate to the more sophisticated things
that might otherwise obscure the essence of this feature).

The `Read` class contains a method `read` which is polymorphic on the
return value:

``` haskell
class Read a where
  read :: String -> a
```

It parses a data type from a string. Combined with the `Show` class,
together `Read` and `Show` make a naive serialization
library. In the same way, it would be ambiguous to read without
specifying the instance:

    λ> read "2"
    Ambiguous type variable `a' in the constraint:
      `Read a' arising from a use of `read' at
        <interactive>:1:0-7
    Probable fix: add a type signature that fixes these type
                  variable(s)

But specifying with a type signature or using type inference are fine:

``` haskell
λ> read "2" :: Int
→ 2
λ> read "2" * 3
→ 6
```


Another example is JSON parsing (the real class is different to this,
but introduces questions that are irrelevant to the point of this
post).

``` haskell
class JSON a where
  decode :: String -> Result a
```

The `decode` function is return-value polymorphic, it can be read like this:

``` haskell
decode :: (JSON a) => String -> Result a
```

That is, it returns a result (success or fail) with a value which is
an instance of the JSON class.

So both specifying an instance or using inference works:

``` haskell
λ> decode "1" :: Result Int
→ Ok 1
λ> do x <- decode "1"; return (x*3)
→ Ok 3
```

And it works however complex you want to go with your types:

``` haskell
λ> decode "[[1,\"a\",{\"x\":3}],[1,\"a\",{\"x\":2}]]"
   :: Result [(Int,String,JSObject Int)]
→ Ok [(1,"a",JSONObject {fromJSObject = [("x",3)]})
     ,(1,"a",JSONObject {fromJSObject = [("x",2)]})]
```

Thus by merely specifying the return type we have effectively
generated a parser. An invalid string will produce an error:

``` haskell
λ> decode "[[1,\"a\",{\"x\":3}],[1,\"a\"]]"
  :: Result [(Int,String,JSObject Int)]
→ Error "Unable to read Triple"
```

In fact, the literal `1` is also polymorphic with type `Num a => a`,
meaning that the number could be an `Integer`, a `Double`, a
`Rational`, or a user-defined type like `Scientific`. It will be
determined by inference or annotation.

Such static value polymorphism is difficult to do in popular languages
such as C#, Java, C++, without some kind of proxy objects to
explicitly instantiate an object to dereference using generics or
templates, and hard to do in Lisp, Python, Ruby and JavaScript without
static type systems (although can also be approximated with proxy aka
"witness" objects). This is, for example, why implementing the Monad
class is rather awkward in other languages.

The list goes on. More examples include database query results, string
literals, monoids, monads, …

Lastly, the `Default` class is a real class and in common use today.
