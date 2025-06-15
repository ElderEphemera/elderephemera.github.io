---
title: How Many Distinct Characters Do You Need In Haskell?
date: 2025-06-15
---

<style>
.box {
  border: solid 1px;
  padding: 2px;
  margin: 2px;
}
</style>

Recently, I've been messing around with code golfing in Haskell again, and I came up with an interesting variant of the usual challenge: Instead of minimizing the number of characters in a solution, minimize the number of *distinct* characters. This challenge is especially interesting to me because you don't even need a particular problem to solve, you can instead show that a set of characters is capable of solving *any* possible problem in a certain language. Haskell code is (in)famous for being terse, so it seems like a natural fit for this challenge. I ended up going down a bit of a rabbit hole on this. There were a lot more complications than I thought there would be.

Of course, this has been done before. [JSFuck](https://jsfuck.com/) shows that any JavaScript program can be replicated using only 6 distinct characters, and [this Code Golf Stack Exchange question](https://codegolf.stackexchange.com/q/110648) *(spoilers! kind of)* asks pretty much the exact question I'm interested in. But nonetheless, I think this is a very interesting problem and one that's worth going into depth on, so I will. In the process of working on this, I learned several things about computability theory and wrote a lot of cursed code (some of it unnecessarily so, just for fun). So I hope you'll learn something from reading this and also furrow your brow incredulously at some of the code.

For this post, I'll assume basic knowledge of Haskell and computability theory. Anything else you need to know, I'll try to explain or at least link to an explanation. A fair warning: this post is dense. It's essentially my notes and experiments that panned out, just cleaned up a little bit and with some added exposition. Feel free to skim through a bit and dig into the parts that interest you. Also. I've tried to make the least readable code I've ever written somewhat bearable to look at, but there's only so much I can do. It'll be helpful to know that you can click on the circle on the top right of large code blocks to collapse them. Apologies to mobile readers in advance.

## Setting the Stage

Before we begin in earnest, we need to define what exactly the goal is. As mentioned above, JSFuck is able to mimic arbitrary JavaScript programs. But Haskell lacks several of the things that make that easy, most notably an eval function that runs strings as code. Instead, we'll focus on just achieving Turing completeness. This lets us completely ignore IO and anything else that's not strictly necessary for *computation*. That being said, it can be interesting to figure out how expressive these sets of characters are as well as how much performance suffers from restricting things so much. So I'll touch on that a little (or a lot) for each character set here.

Another consideration is what we should consider a program. Since we won't be dealing with IO, we can't have an actual executable. Instead, we'll focus on definitions; if we can construct the definition of a function that emulates an arbitrary Turing machine (or something equivalent) using only a set of characters, then we'll call that set Turing complete. Another option would be to deal with expressions, which would allow us to get rid of at least `=`, but at the cost of recursive definitions—which is likely to be too great a cost to be worth it.

Now that the problem is a bit more specified, I'd encourage you to think about yourself a bit before reading on. I really did have fun with this challenge, and I hope you do too.

### Semantic Necessities

To achieve Turing completeness, we'll need a few things that our set of characters is capable of expressing.

#### Looping

First, we'll need some form of looping. This one is easy, even in regular Haskell we just use recursion. Although we often use indirect recursion through functions like `foldr`, that most likely won't be the case here. Even `fix` requires three letters and if we don't use any built-in functions, we need at most one. So we'll stick to direct recursion.

#### Branching

We'll also need some form of branching. The obvious choice here for Haskell is pattern matching. While `case` and `of` would require way too many characters, pattern matching directly in a function definition needs essentially no extra characters, especially because pattern syntax mimics expression syntax. Guards could also be tempting because they only need one extra character, `|`.

#### Data

The last thing we need is a way of storing and modifying arbitrarily large data. The obvious choice in a functional language like Haskell is of course functions. The untyped lambda calculus manages to be Turing complete with *only* functions by using constructions like the Y combinator and Church numerals. Haskell's function definition syntax is incredibly terse, so if we can replicate this in Haskell, it will give us a very small set of Turing complete characters.

Another good option is integers. Like essentially all other programming languages, Haskell inherits very succinct notation for manipulating numbers from math. Additionally, we can pattern match on number literals, so we get branching too.

More unique to Haskell is its built-in list syntax, `:` and `[]`, giving us a way to construct and deconstruct (with pattern matching) with just 3 characters. This is a particularly attractive option if we're aiming for performance because, under the definitions and sugar, *all* list manipulation in Haskell is done using just these two constructors. Meaning we won't even have to sacrifice performance at all.

### Syntactic Necessities

In addition to what is required by Turing completeness, there are a couple of things that the syntax of Haskell itself will require.

#### Grouping

Because Haskell is an applicative language, we need some way of grouping subexpressions. The obvious built-in way to do this is parentheses: `f (x + 2) (g y)`. Not much to say there.

However, Haskell doesn't always *need* parentheses. We can usually use operators to replace them because operators have lower precedence than function application. It's often even considered good style to write something like `f . g $ h x` over `f (g (h x))`.

Another, less obvious (and certainly not good style), way to group subexpressions in Haskell is with brackets `[]`. We can use brackets to both create and pattern match singleton lists so, as long as we're completely consistent with bracketing parameters both in the definition of a function and all its uses, we can use brackets like parentheses. For example:

```hs
ghci> :{
ghci| zipWith[f][x:xs][y:ys]=f[x][y]:zipWith[f][xs][ys]
ghci| zipWith[_][_][_]=[]
ghci| add[x][y]=x+y
ghci| :}
ghci> zipWith[add][[1..5]][[6..]]
[7,9,11,13,15]
```

There are other ways to avoid parentheses in Haskell, like using `let` expressions or `where` clauses to bind subexpressions to names, or (ab)using `-XBlockArguments`, but those won't be useful here. They require too many extra characters.

#### Names

We'll need names to give to our functions and their parameters. You might object with a point about pointfree code, but that would require even more characters. Really all we need to have as many names as we could want is one lowercase letter: `x` gives us `x`, `xx`, `xxx`, etc.

But Haskell also lets us name things as operators, so if we have parentheses and at least one symbol character (`=` is a given), then we don't even need letters at all:

```hs
ghci> (===)(====)(===)(==)=(====)((===)(==))(==)
ghci> (===) (+) succ 5
11
```

Without parentheses, operators are much more limited. Then, all operators must be applied to exactly 2 arguments. This means we'd still need a letter for the argument names. Additionally, we can't pattern match on `:`, because its precedence is 5 which is less than the default precedence, 9. So `x===y:ys=y` parses as `(x===y):ys=y` which is invalid.

## Trying Lists

With all of that established, we can start with the challenge itself. We'll use lists for our first attempt, just because they seem the most easy to work with. We'll need `:`, `[`, and `]` to construct and destruct lists, `=` and `;` for recursive definitions, and `x` for names (using operator names would require parentheses which we don't need otherwise, so it's better to not). All together that's six characters, `:;=[]x`. Not a bad start if we can make it work.

To prove Turing completeness of this set of characters, we'll write an interpreter for a known Turing complete language. Due to its simplicity, [brainfuck](https://en.wikipedia.org/wiki/Brainfuck) is a fine option (if you're unfamiliar, I'd recommend skimming the Wikipedia article real quick). We'll start with a simple but readable-ish implementation, then quickly make it completely unreadable while reducing characters.

```hs
bf :: String -> String -> String
bf prog input = decode $ prog =: encode input

encode :: String -> [[[a]]]
encode = map (flip replicate [] . fromEnum)

decode :: [[[a]]] -> String
decode = map (toEnum . length)

------------------------------------------------------------------------------

prog =: input = run [] [] [] zeros prog input

zeros = [] : zeros

run stack left here right (instr:prog) input =
  run' stack left here right instr prog input
run stack left here right [] input = []

run' stack left here (next:right) '>' prog input =
  run stack (here:left) next right prog input
run' stack (prev:left) here right '<' prog input =
  run stack left prev (here:right) prog input
run' stack left here right '+' prog input =
  run stack left ([]:here) right prog input
run' stack left ([]:here) right '-' prog input =
  run stack left here right prog input
run' stack left here right '.' prog input =
  here : run stack left here right prog input
run' stack left here right ',' prog (char:input) =
  run stack left char right prog input
run' stack left [] right '[' prog input =
  run stack left [] right (skip prog) input
run' stack left here right '[' prog input =
  run (prog:stack) left here right prog input
run' (jump:stack) left [] right ']' prog input =
  run stack left [] right prog input
run' (jump:stack) left here right ']' prog input =
  run (jump:stack) left here right jump input
run' stack left here right instr prog input =
  run stack left here right prog input

skip (instr:prog) = skip' instr prog
skip [] = []

skip' '[' prog = skip (skip prog)
skip' ']' prog = prog
skip' instr prog = skip prog
```

I'm not going to go into great detail here, but there are a few things that need to be mentioned about this. First, the code above the line is simply translating to and from lists and is not part of the interpreter proper. Specifically, it encodes a character `c` as a list by `replicate (fromEnum c) []`. We also don't encode the program itself quite yet just to save some readability in this initial version.

However, we do start with both `run` and `skip` separated into two functions each. This will be necessary later because we cannot pattern match on a list and its element at the same time without parentheses: `x:y:z` is `x:(y:z)`, not `(x:y):z`.

One last thing to note is that this interpreter behaves differently from most others when decrementing zero. Usually, this results in a negative number or a wrap-around to some maximum value. Here we just do nothing, because it's easier. Fortunately, this does not affect Turing completeness.

Our first step to reducing this interpreter will be to replace the character pattern matching with list pattern matching.

```hs {.wrap-code}
bf :: String -> String -> String
bf prog input = decode $ encode prog =: encode input

encode :: String -> [[[a]]]
encode = map (flip replicate [] . fromEnum)

decode :: [[[a]]] -> String
decode = map (toEnum . length)

------------------------------------------------------------------------------

prog =: input = run [] [] [] zeros prog input

zeros = [] : zeros

run stack left here right (instr:prog) input = run' stack left here right instr prog input
run stack left here right [] input = []

run' stack left here (next:right) ([]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]) prog input = run stack (here:left) next right prog input
run' stack (prev:left) here right ([]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]) prog input = run stack left prev (here:right) prog input
run' stack left here right ([]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]) prog input = run stack left ([]:here) right prog input
run' stack left ([]:here) right ([]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]) prog input = run stack left here right prog input
run' stack left here right ([]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]) prog input = here : run stack left here right prog input
run' stack left here right ([]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]) prog (char:input) = run stack left char right prog input
run' stack left [] right ([]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]) prog input = run stack left [] right (skip prog) input
run' stack left here right ([]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]) prog input = run (prog:stack) left here right prog input
run' (jump:stack) left [] right ([]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]) prog input = run stack left [] right prog input
run' (jump:stack) left here right ([]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]) prog input = run (jump:stack) left here right jump input
run' stack left here right instr prog input = run stack left here right prog input

skip (instr:prog) = skip' instr prog
skip [] = []

skip' ([]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]) prog = skip (skip prog)
skip' ([]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]) prog = prog
skip' instr prog = skip prog
```

Next, we replace spaces and parentheses with brackets, as discussed earlier, and remove the spaces because it's already unreadable anyway. (Also the wrapping code won't change from here on out, so I'll omit it.)

```hs {.wrap-code}
prog=:input=run[[]][[]][[]][zeros][prog][input]

zeros=[]:zeros

run[stack][left][here][right][instr:prog][input]=run'[stack][left][here][right][instr][prog][input]
run[stack][left][here][right][[]][input]=[]

run'[stack][left][here][next:right][[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][prog][input]=run[stack][here:left][next][right][prog][input]
run'[stack][prev:left][here][right][[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][prog][input]=run[stack][left][prev][here:right][prog][input]
run'[stack][left][here][right][[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][prog][input]=run[stack][left][[]:here][right][prog][input]
run'[stack][left][[]:here][right][[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][prog][input]=run[stack][left][here][right][prog][input]
run'[stack][left][here][right][[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][prog][input]=here:run[stack][left][here][right][prog][input]
run'[stack][left][here][right][[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][prog][char:input]=run[stack][left][char][right][prog][input]
run'[stack][left][[]][right][[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][prog][input]=run[stack][left][[]][right][skip[prog]][input]
run'[stack][left][here][right][[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][prog][input]=run[prog:stack][left][here][right][prog][input]
run'[jump:stack][left][[]][right][[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][prog][input]=run[stack][left][[]][right][prog][input]
run'[jump:stack][left][here][right][[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][prog][input]=run[jump:stack][left][here][right][jump][input]
run'[stack][left][here][right][instr][prog][input]=run[stack][left][here][right][prog][input]

skip[instr:prog]=skip'[instr][prog]
skip[[]]=[]

skip'[[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][prog]=skip[skip[prog]]
skip'[[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][prog]=prog
skip'[instr][prog]=skip[prog]
```

Now we replace all names with `x`s. This can mostly just be done with find and replace.

```hs {.wrap-code}
xx=:xxx=x[[]][[]][[]][xxxxx][xx][xxx]

xxxxx=[]:xxxxx

x[xxxx][xxxxx][xxxxxx][xxxxxxx][xxxxxxxxxx:xxxxxxxx][xxxxxxxxx]=xx[xxxx][xxxxx][xxxxxx][xxxxxxx][xxxxxxxxxx][xxxxxxxx][xxxxxxxxx]
x[xxxx][xxxxx][xxxxxx][xxxxxxx][[]][xxxxxxxxx]=[]

xx[xxxx][xxxxx][xxxxxx][xxxxxxxxxx:xxxxxxx][[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][xxxxxxxx][xxxxxxxxx]=x[xxxx][xxxxxx:xxxxx][xxxxxxxxxx][xxxxxxx][xxxxxxxx][xxxxxxxxx]
xx[xxxx][xxxxxxxxxx:xxxxx][xxxxxx][xxxxxxx][[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][xxxxxxxx][xxxxxxxxx]=x[xxxx][xxxxx][xxxxxxxxxx][xxxxxx:xxxxxxx][xxxxxxxx][xxxxxxxxx]
xx[xxxx][xxxxx][xxxxxx][xxxxxxx][[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][xxxxxxxx][xxxxxxxxx]=x[xxxx][xxxxx][[]:xxxxxx][xxxxxxx][xxxxxxxx][xxxxxxxxx]
xx[xxxx][xxxxx][[]:xxxxxx][xxxxxxx][[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][xxxxxxxx][xxxxxxxxx]=x[xxxx][xxxxx][xxxxxx][xxxxxxx][xxxxxxxx][xxxxxxxxx]
xx[xxxx][xxxxx][xxxxxx][xxxxxxx][[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][xxxxxxxx][xxxxxxxxx]=xxxxxx:x[xxxx][xxxxx][xxxxxx][xxxxxxx][xxxxxxxx][xxxxxxxxx]
xx[xxxx][xxxxx][xxxxxx][xxxxxxx][[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][xxxxxxxx][xxxxxxxxxx:xxxxxxxxx]=x[xxxx][xxxxx][xxxxxxxxxx][xxxxxxx][xxxxxxxx][xxxxxxxxx]
xx[xxxx][xxxxx][[]][xxxxxxx][[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][xxxxxxxx][xxxxxxxxx]=x[xxxx][xxxxx][[]][xxxxxxx][xxx[xxxxxxxx]][xxxxxxxxx]
xx[xxxx][xxxxx][xxxxxx][xxxxxxx][[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][xxxxxxxx][xxxxxxxxx]=x[xxxxxxxx:xxxx][xxxxx][xxxxxx][xxxxxxx][xxxxxxxx][xxxxxxxxx]
xx[xxxxxxxxxx:xxxx][xxxxx][[]][xxxxxxx][[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][xxxxxxxx][xxxxxxxxx]=x[xxxx][xxxxx][[]][xxxxxxx][xxxxxxxx][xxxxxxxxx]
xx[xxxxxxxxxx:xxxx][xxxxx][xxxxxx][xxxxxxx][[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][xxxxxxxx][xxxxxxxxx]=x[xxxxxxxxxx:xxxx][xxxxx][xxxxxx][xxxxxxx][xxxxxxxxxx][xxxxxxxxx]
xx[xxxx][xxxxx][xxxxxx][xxxxxxx][xxxxxxxxxx][xxxxxxxx][xxxxxxxxx]=x[xxxx][xxxxx][xxxxxx][xxxxxxx][xxxxxxxx][xxxxxxxxx]

xxx[xxxxxxxxxx:xxxxxxxx]=xxxx[xxxxxxxxxx][xxxxxxxx]
xxx[[]]=[]

xxxx[[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][xxxxxxxx]=xxx[xxx[xxxxxxxx]]
xxxx[[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][xxxxxxxx]=xxxxxxxx
xxxx[xxxxxxxxxx][xxxxxxxx]=xxx[xxxxxxxx]
```

Finally, we replace the newlines with semicolons. This doesn't actually reduce the number of unique characters, but it's more aesthetically pleasing to me to have no whitespace in our character set.

```hs {.wrap-code}
xx=:xxx=x[[]][[]][[]][xxxxx][xx][xxx];xxxxx=[]:xxxxx;x[xxxx][xxxxx][xxxxxx][xxxxxxx][xxxxxxxxxx:xxxxxxxx][xxxxxxxxx]=xx[xxxx][xxxxx][xxxxxx][xxxxxxx][xxxxxxxxxx][xxxxxxxx][xxxxxxxxx];x[xxxx][xxxxx][xxxxxx][xxxxxxx][[]][xxxxxxxxx]=[];xx[xxxx][xxxxx][xxxxxx][xxxxxxxxxx:xxxxxxx][[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][xxxxxxxx][xxxxxxxxx]=x[xxxx][xxxxxx:xxxxx][xxxxxxxxxx][xxxxxxx][xxxxxxxx][xxxxxxxxx];xx[xxxx][xxxxxxxxxx:xxxxx][xxxxxx][xxxxxxx][[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][xxxxxxxx][xxxxxxxxx]=x[xxxx][xxxxx][xxxxxxxxxx][xxxxxx:xxxxxxx][xxxxxxxx][xxxxxxxxx];xx[xxxx][xxxxx][xxxxxx][xxxxxxx][[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][xxxxxxxx][xxxxxxxxx]=x[xxxx][xxxxx][[]:xxxxxx][xxxxxxx][xxxxxxxx][xxxxxxxxx];xx[xxxx][xxxxx][[]:xxxxxx][xxxxxxx][[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][xxxxxxxx][xxxxxxxxx]=x[xxxx][xxxxx][xxxxxx][xxxxxxx][xxxxxxxx][xxxxxxxxx];xx[xxxx][xxxxx][xxxxxx][xxxxxxx][[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][xxxxxxxx][xxxxxxxxx]=xxxxxx:x[xxxx][xxxxx][xxxxxx][xxxxxxx][xxxxxxxx][xxxxxxxxx];xx[xxxx][xxxxx][xxxxxx][xxxxxxx][[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][xxxxxxxx][xxxxxxxxxx:xxxxxxxxx]=x[xxxx][xxxxx][xxxxxxxxxx][xxxxxxx][xxxxxxxx][xxxxxxxxx];xx[xxxx][xxxxx][[]][xxxxxxx][[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][xxxxxxxx][xxxxxxxxx]=x[xxxx][xxxxx][[]][xxxxxxx][xxx[xxxxxxxx]][xxxxxxxxx];xx[xxxx][xxxxx][xxxxxx][xxxxxxx][[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][xxxxxxxx][xxxxxxxxx]=x[xxxxxxxx:xxxx][xxxxx][xxxxxx][xxxxxxx][xxxxxxxx][xxxxxxxxx];xx[xxxxxxxxxx:xxxx][xxxxx][[]][xxxxxxx][[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][xxxxxxxx][xxxxxxxxx]=x[xxxx][xxxxx][[]][xxxxxxx][xxxxxxxx][xxxxxxxxx];xx[xxxxxxxxxx:xxxx][xxxxx][xxxxxx][xxxxxxx][[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][xxxxxxxx][xxxxxxxxx]=x[xxxxxxxxxx:xxxx][xxxxx][xxxxxx][xxxxxxx][xxxxxxxxxx][xxxxxxxxx];xx[xxxx][xxxxx][xxxxxx][xxxxxxx][xxxxxxxxxx][xxxxxxxx][xxxxxxxxx]=x[xxxx][xxxxx][xxxxxx][xxxxxxx][xxxxxxxx][xxxxxxxxx];xxx[xxxxxxxxxx:xxxxxxxx]=xxxx[xxxxxxxxxx][xxxxxxxx];xxx[[]]=[];xxxx[[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][xxxxxxxx]=xxx[xxx[xxxxxxxx]];xxxx[[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[]:[[]]][xxxxxxxx]=xxxxxxxx;xxxx[xxxxxxxxxx][xxxxxxxx]=xxx[xxxxxxxx]
```

Now, because this interpreter functions a bit differently than most, as mentioned earlier, we can't use just any existing brainfuck code for testing. A lot of optimized brainfuck relies on either integer overflow or negative numbers. I ended up using the Hello World program from the Wikipedia page and adding to it to account for bugs I found while writing a few different brainfuck interpreters for this challenge.

```console {.wrap-code}
$ ghci bf6.hs -e 'putStrLn $ bf "_[[\\]+]++++++++[>++++[>++>+9+J+>+++>+<<<<-]>+>+>->>+[<]<#-]>>.>A---.++++7H+++..+++.>>.<-.5<.+++.---f---.-3-------.>>_+.>++.>+++++++a++++>,.<[>+.<-]<.>>>+++++++++++>,.<[>+.<-]" "aA"'
Hello World!
abcdefghijkl
ABCDEFGHIJKL
```

And there it is, Haskell with just 6 characters is Turing complete. 

### Expressivity

Beyond just being Turing complete, we can actually express quite a lot of functions with these characters. Combining our bracketed parameter trick with partial application and empty lists gives us the ability to define functions without the brackets like so:

```hs
f=f1[];
f1[]x=f2[x];
f2[x]y=f3[x][y];
f3[x][y]z={implementation here}
```

And since we have both constructors for lists as well as recursion, this gets us all (non-higher-order) functions on lists! For higher-order functions, instead of spaces, parentheses, or `$`, we can use a function like `the[x]=x`, so that `f x` is `the[f]x`. With that, we have *all* functions on lists. As an example, here's `map`:

```hs {.wrap-code}
ghci> xxxx=xxx[];xxx[]x=xx[x];xx[xx]xxx=x[xxx]xx;x[[]]x=[];x[xxxx:xxx]xx=xxxxx[xx]xxxx:x[xxx]xx;xxxxx[x]=x
ghci> :t xxxx
xxxx :: (t -> a) -> [t] -> [a]
ghci> xxxx (+5) [0..9]
[5,6,7,8,9,10,11,12,13,14]
```

### Efficiency

The only performance issue here is the packing and unpacking of the singleton lists. With optimizations, I believe this will likely be compiled away for most non-recursive functions through inlining and the case-of-known-constructor optimization. On the other hand, we do miss out on list fusion and the other rewrite rules that built-in functions have. But none of this should affect time complexity, only constant factors. So it's not that bad.


## What About Integers?

Now can we do the same thing with integers? With `1`, `-`, and parentheses, we can subtract by one, add one (by subtracting negative one), and pattern match on one. With parentheses we can also use operator names, and by throwing in `=` and `;` as well, we get recursive definitions. So it seems like `()-1;=` should be enough for Turing completeness.

However, it'll be difficult to write a brainfuck interpreter again. We'd need to encode the unbounded tape in a finite number of integers because the number of parameters is static. This is definitely doable—for example by interpreting a number in base 256—but it will likely lead to performance that's bad enough it makes it hard to even test.

Counter machines will be a much better fit here. Specifically, we'll use Minsky Machines as described [on the Esolang wiki](https://esolangs.org/wiki/Minsky_machine) or in *Computation: Finite and Infinite Machines* (1967) By Marvin L. Minsky (where they're called program machines). These machines model programs as a sequence of instructions operating on some registers, each of which stores a non-negative integer. To do this there are two kinds of instructions:

- `inc(a)` ("Increment" or "Successor") which increments the value in register `a` by one and then proceeds to the next instruction
- `dec(a,n)` ("Decrement or jump") which, if the value in register `a` is non-zero, decrements it by one and then proceeds to the next instruction, but otherwise if the value is zero, jumps to the nth instruction.

One small change we'll be making is that registers will store positive integers instead of non-negative integers so that we can pattern match on `1` for decrements. Just replace "zero" with "one" in the description of `dec` above.

Because a program has a finite number of instructions and each instruction operates on a fixed register, a specific Minsky machine can only use a finite number of registers; perfect for our purposes. Even so, Minsky machines with at least 2 unbounded registers (i.e. capable of storing arbitrarily large numbers) are Turing complete.

To make an interpreter though, we'd still have the problem of encoding the arbitrarily long programs as a finite number of integers. We'll sidestep this by making a compiler instead.

We'll use the following translation table. Here `n: inc(a)` means that the nth instruction is `inc(a)`, and likewise for `dec`. Additionally, $\equiv_k$ and $\equiv'_k$ stand for some operator made out of `-` and `=`. It doesn't matter specifically what operator, as long as shadowing doesn't interfere.

$$
\begin{array}{lcl}
\texttt{n: inc(a)} & \longrightarrow & (\equiv_n)(\equiv'_0)\cdots(\equiv'_k)=(\equiv_{n+1})(\equiv'_0)\cdots(\equiv'_{a-1})((\equiv'_a)-(-1))(\equiv'_{a+1})\cdots(\equiv'_k);\\
\texttt{n: dec(a,m)} & \longrightarrow & (\equiv_n)(\equiv'_0)\cdots(\equiv'_{a-1})1(\equiv'_{a+1})\cdots(\equiv'_k)=(\equiv_{m})(\equiv'_0)\cdots(\equiv'_{a-1})1(\equiv'_{a+1})\cdots(\equiv'_k);\\
&&(\equiv_n)(\equiv'_0)\cdots(\equiv'_k)=(\equiv_{n+1})(\equiv'_0)\cdots(\equiv'_{a-1})((\equiv'_a)-1)(\equiv'_{a+1})\cdots(\equiv'_k);\\
\end{array}
$$

For the input to our compiler, we'll use [Szewczyk notation](https://esolangs.org/wiki/Szewczyk_notation_for_Minsky_machine) which is a concise, and very machine-readable, notation for Minsky machines. Very quickly, `inc(a)` is represented by `a -1` and `dec(a,n)` by `a n`. A program is a sequence of these commands separated by whitespace. (It's supposed to be a newline, but we won't be that particular. It's easier to type out a program on the command line with just spaces anyway.)

The code we want to generate is Haskell, so we may as well (ab)use Template Haskell to do the code generation. That means the input program has to be known at compile time so we'll also (ab)use the C preprocessor to pass in the input as a macro. Of course, this is all very cursed, but what we're doing is inherently a bit strange, so we might as well go whole hog. As such, any idiosyncracies or errors in any of the following code are totally intentional and not at all a product of laziness or incopetance. TODO spellcheck

```hs
{-# language CPP #-}
{-# language TemplateHaskell #-}
{-# options -Wno-x-partial #-}

import Control.Monad (replicateM)
import Data.List ((\\))
import Language.Haskell.TH

$(pure $ let
  ops = map mkName . filter (any (/='-')) $ flip replicateM "-=" =<< enumFrom 3
  opsWithNext = zip ops $ tail ops

  chunk (r:y:rest) = (r,y) : chunk rest
  chunk _ = []

  prog = chunk $ read <$> words PROGRAM

  oneL = IntegerL 1
  minusN = mkName "-"
  subtractE x y = InfixE (Just y) (VarE minusN) (Just x)
  appsE f xs = foldl AppE (VarE f) xs

  mkArgs reserved = take registers $ ops \\ reserved
  registers = maximum (fst <$> prog) + 1

  modify 0 f (x:xs) = f x : xs
  modify n f (x:xs) = x : modify (n-1) f xs

  clause reg patf expf j reserved = Clause
    (modify reg patf $ VarP <$> args)
    (NormalB . appsE j . modify reg expf $ VarE <$> args)
    []
    where args = mkArgs (j:reserved)

  instr (op, next) (r, -1) = FunD op
    [ clause r id (subtractE . LitE $ IntegerL (-1)) next [minusN]
    ]
  instr (op, next) (r, y) = FunD op
    [ clause r (const $ LitP oneL) (const $ LitE oneL) (ops !! y) []
    , clause r id (subtractE $ LitE oneL) next [minusN]
    ]

  begin = ValD (VarP $ mkName "-=") (NormalB $ appsE (head ops) ones) []
    where ones = replicate registers . ParensE $ LitE oneL

  end op = FunD op [Clause (VarP <$> args) (NormalB $ VarE arg1) []]
    where args@(arg1:_) = mkArgs []

  in begin : end (ops !! length prog) : zipWith instr opsWithNext prog
 )

main :: IO ()
main = putStr "Generated: " <> print (-=)
```

I don't really want to explain this evil code in detail, so I'm not going to. If you understand the translation table, you understand what this is doing. Anyways, keeping with the theme of cursed code, we can test it using this dark incantation amalgamated from several Stack Overflow answers and the sed manual:

```console {.wrap-code}
$ runghc -DPROGRAM='"1 -1 1 -1 1 -1 2 -1 2 -1 2 -1 2 -1 2 16 1 12 0 -1 3 -1 4 8 3 15 1 -1 4 12 4 7"' -dth-dec-file minsky6.hs && sed -z 's/[^\n]*\n//;s/\n\? //g;s/\n/;/g' minsky6.dyn.th.hs | tee >(sed 'a\\nputStr"Processed: "\n(-=)' | ghci -v0 1>&2) | sed 's/./\0\n/g' | LC_COLLATE=C sort -u | sed -z 's/\n//g;s/.*/Characters: \0\n/'
Generated: 13
Processed: 13
Characters: ()-1;=
```

I *will* walk through this a bit, just so you trust me that it works. First, we call `runghc` with our input test program. Then, because GHC likes to make TH-generated output code "pretty" (and we want it decidedly ugly), we post-process the TH dump with `sed`. Next, we feed the processed code to `ghci` to make sure we haven't messed it up. Finally, we get a list of the distinct characters in the processed code to make sure nothing extraneous slipped in. You can also cut off everything after the first `sed` command to see the processed code for yourself, not that you'd *want* to see that... Here it is:

```hs {.wrap-code}
(-=)=(--=)(1)(1)(1)(1)(1);(=-=-)(--=)(-=-)(-==)(=--)(=-=)=(--=);(--=)(--=)(-==)(=--)(=-=)(==-)=(-=-)(--=)((-==)-(-1))(=--)(=-=)(==-);(-=-)(--=)(-=-)(=--)(=-=)(==-)=(-==)(--=)((-=-)-(-1))(=--)(=-=)(==-);(-==)(--=)(-=-)(-==)(=-=)(==-)=(=--)(--=)((-=-)-(-1))(-==)(=-=)(==-);(=--)(--=)(-=-)(-==)(=--)(==-)=(=-=)(--=)(-=-)((-==)-(-1))(=--)(==-);(=-=)(--=)(-=-)(-==)(=--)(=-=)=(==-)(--=)(-=-)((-==)-(-1))(=--)(=-=);(==-)(--=)(-=-)(-==)(=--)(=-=)=(===)(--=)(-=-)((-==)-(-1))(=--)(=-=);(===)(--=)(-=-)(-==)(=--)(=-=)=(---=)(--=)(-=-)((-==)-(-1))(=--)(=-=);(---=)(--=)(-=-)1(=--)(=-=)=(=-=-)(--=)(-=-)1(=--)(=-=);(---=)(--=)(-=-)(-==)(=--)(=-=)=(--=-)(--=)(-=-)((-==)-1)(=--)(=-=);(--=-)(--=)1(-==)(=--)(=-=)=(-==-)(--=)1(-==)(=--)(=-=);(--=-)(--=)(-=-)(-==)(=--)(=-=)=(--==)(--=)((-=-)-1)(-==)(=--)(=-=);(--==)(--=)(-=-)(-==)(=--)(=-=)=(-=--)((--=)-(-1))(-=-)(-==)(=--)(=-=);(-=--)(--=)(-=-)(-==)(=--)(=-=)=(-=-=)(--=)(-=-)(-==)((=--)-(-1))(=-=);(-=-=)(--=)(-=-)(-==)(=--)1=(--=-)(--=)(-=-)(-==)(=--)1;(-=-=)(--=)(-=-)(-==)(=--)(=-=)=(-==-)(--=)(-=-)(-==)(=--)((=-=)-1);(-==-)(--=)(-=-)(-==)1(=-=)=(=--=)(--=)(-=-)(-==)1(=-=);(-==-)(--=)(-=-)(-==)(=--)(=-=)=(-===)(--=)(-=-)(-==)((=--)-1)(=-=);(-===)(--=)(-=-)(-==)(=--)(=-=)=(=---)(--=)((-=-)-(-1))(-==)(=--)(=-=);(=---)(--=)(-=-)(-==)(=--)1=(-==-)(--=)(-=-)(-==)(=--)1;(=---)(--=)(-=-)(-==)(=--)(=-=)=(=--=)(--=)(-=-)(-==)(=--)((=-=)-1);(=--=)(--=)(-=-)(-==)(=--)1=(---=)(--=)(-=-)(-==)(=--)1;(=--=)(--=)(-=-)(-==)(=--)(=-=)=(=-=-)(--=)(-=-)(-==)(=--)((=-=)-1);
```

### Expressivity

Because we don't have constructors this time around, how expressive this character set is isn't as obvious. Luckily, Minsky already did the hard work here for us by proving that Minsky machines are capable of computing any general-recursive function i.e. any computable function on natural numbers. Combining this with the fact that we have negation and parentheses we can see that this set is capable of expressing any function on integers, including higher-order functions. Of course, "integer" is a little vague: Haskell has several integer types, but any of them will do. We can even convert between them as well. This function converts from an unbounded signed integer type (i.e. `Integer`) to any other numeric type.

```hs
convert x = go 1 x x;
go m n 1 = m;
go m 1 p = -(m-1-1);
go m n p = go (m-(-1)) (n-(-1)) (p-1)
```

(This uses more characters obviously, but only for readability.) Note that the recursion with possibly negative integers is a bit tricky but completely doable. We can also use non-integer number types as well, but we have to keep to integer values and linear combinations of inputs.

We also have access to `==` which gives us the ability to return `Bool`s. Just write a function that returns `1` for true and `0` for false, then use `==1`.

### Efficiency

The performance story here is much more bleak though. We can do better than a naive Minsky machine because we have subtraction and not just increment/decrement. But even something as simple as multiplication will be linear time, and other things will likely be much worse.

## And Functions, Of Course

Since we have to use functions anyway for taking input and producing output, why don't we also use them for our data? If we do this, then we only need 4 characters, `();=`. Using just these characters, we can represent lambda calculus in Haskell by naming each lambda and passing the variables it closes over as arguments. However, Haskell is a typed language so we are limited to typed lambda calculi, which are not necessarily Turing complete. Luckily, we can use recursive definitions as well which, by popular knowledge is enough for Turing completeness.

But it would be much better to have definitive proof like an interpreter or compiler. We could implement the transformation from lambda calculus described above, but it seems easier to just compile from Minsky machines again, even if only to avoid parsing and handling names and such. We can pretty easily adapt our Minsky machine compiler from earlier by including a few definitions to manipulate [Church-encoded](https://en.wikipedia.org/wiki/Church_encoding) numerals.

For the initial version of the compiler, I've left some helpers outside of the TH splice and not yet converted to the character set for readability.

```hs
{-# language CPP #-}
{-# language TemplateHaskell #-}

import Control.Monad (replicateM)
import Data.List ((\\))
import Language.Haskell.TH

$(pure $ let
  eqs = "====" : map ('=':) eqs
  ops = mkName <$> eqs
  opsWithNext = zip ops $ tail ops

  chunk2 (r:y:rest) = (r,y) : chunk2 rest
  chunk2 _ = []

  prog = chunk2 $ read <$> words PROGRAM

  mkArgs reserved = take registers $ (map mkName ["==", "==="] ++ ops) \\ reserved
  registers = maximum (fst <$> prog) + 1

  modify 0 f (x:xs) = f x : xs
  modify n f (x:xs) = x : modify (n-1) f xs

  instr (op, next) (r, -1) = FunD op
    [ Clause
      (VarP <$> args)
      (NormalB . foldl AppE (VarE next) . modify r (AppE . VarE $ mkName "inc") $ VarE <$> args)
      []
    ]
    where args = mkArgs [next]
  instr (op, next) (r, y) = FunD op
    [ Clause
      (VarP <$> args)
      (NormalB $ foldl AppE (VarE $ mkName "dec") [goNext, jump, VarE reg])
      []
    ]
    where
      args = mkArgs [next]
      (argsl, reg:argsr) = splitAt r args
      goNext = foldl (\x y -> InfixE (Just x) (VarE $ mkName "flp") (Just y)) (foldl AppE (VarE next) $ VarE <$> argsl) $ VarE <$> argsr
      jump = foldl AppE (VarE $ ops !! y) $ VarE <$> args

  begin = ValD (VarP $ mkName "===") (NormalB $ foldl AppE (VarE $ head ops) zeros) []
    where zeros = replicate registers . VarE $ mkName "zero"

  end op = FunD op [Clause (VarP <$> args) (NormalB $ VarE arg1) []]
    where args@(arg1:_) = mkArgs []

  in begin : end (ops !! length prog) : zipWith instr opsWithNext prog
 )

zero f x = x
inc n f x = f (n f x)
pre n f x = n (\g h -> h (g f)) (\u -> x) (\u -> u)
dec s z n = n (\_ -> s (pre n)) z
flp f y x = f x y

main :: IO ()
main = putStr "Generated: " *> print $ (===) (+1) (0::Int)
```

And running this on the test machine from earlier, we get... a massive wall of type errors...

```console
$ runghc -DPROGRAM='"1 -1 1 -1 1 -1 2 -1 2 -1 2 -1 2 -1 2 16 1 12 0 -1 3 -1 4 8 3 15 1 -1 4 12 4 7"' minsky4.hs
minsky4.hs:8:2: error: [GHC-25897]
    • Couldn't match type ‘t1’ with ‘(t1 -> t1) -> t1’
      Expected: (((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                -> (t1 -> t1) -> (t1 -> t1) -> t1
        Actual: (((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                -> (t1 -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1
      ‘t1’ is a rigid type variable bound by
        the inferred types of
          =================== :: ((((t1 -> t1) -> t1)
                                   -> (t1 -> t1) -> (t1 -> t1) -> t1)
                                  -> (t1 -> t1) -> (t1 -> t1) -> t1)
                                 -> ((((((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                      -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                      -> (t1 -> t1)
                                      -> t1)
                                     -> ((((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                                         -> (t1 -> t1) -> (t1 -> t1) -> t1)
                                     -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                     -> (t1 -> t1)
                                     -> t1)
                                 -> (((t2 -> (t1 -> t1) -> t1)
                                      -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                      -> (t1 -> t1)
                                      -> t1)
                                     -> ((((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                                         -> (t1 -> t1) -> (t1 -> t1) -> t1)
                                     -> (t1 -> t1)
                                     -> t1)
                                 -> ((((((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                      -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                      -> (t1 -> t1)
                                      -> t1)
                                     -> ((t1 -> t1) -> t1)
                                     -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                     -> (t1 -> t1)
                                     -> t1)
                                 -> (((t3 -> (t1 -> t1) -> t1)
                                      -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                                      -> (t1 -> t1)
                                      -> (t1 -> t1)
                                      -> t1)
                                     -> ((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                 -> (t1 -> t1)
                                 -> t1
          =========== :: ((((t1 -> t1) -> t1)
                           -> (t1 -> t1) -> (t1 -> t1) -> t1)
                          -> (t1 -> t1) -> (t1 -> t1) -> t1)
                         -> ((((((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                              -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                             -> ((((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                                 -> (t1 -> t1) -> (t1 -> t1) -> t1)
                             -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                             -> (t1 -> t1)
                             -> t1)
                         -> (((t2 -> (t1 -> t1) -> t1)
                              -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                             -> ((((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                                 -> (t1 -> t1) -> (t1 -> t1) -> t1)
                             -> (t1 -> t1)
                             -> t1)
                         -> ((((((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                              -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                             -> ((t1 -> t1) -> t1)
                             -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                             -> (t1 -> t1)
                             -> t1)
                         -> (((t3 -> (t1 -> t1) -> t1)
                              -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                              -> (t1 -> t1)
                              -> (t1 -> t1)
                              -> t1)
                             -> ((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                         -> (t1 -> t1)
                         -> t1
          ============ :: ((((t1 -> t1) -> t1)
                            -> (t1 -> t1) -> (t1 -> t1) -> t1)
                           -> (t1 -> t1) -> (t1 -> t1) -> t1)
                          -> ((((((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                               -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                              -> ((((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                                  -> (t1 -> t1) -> (t1 -> t1) -> t1)
                              -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                              -> (t1 -> t1)
                              -> t1)
                          -> (t2 -> ((t1 -> t1) -> (t1 -> t1) -> t1) -> t1)
                          -> ((((((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                               -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                              -> ((t1 -> t1) -> t1)
                              -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                              -> (t1 -> t1)
                              -> t1)
                          -> (((t3 -> (t1 -> t1) -> t1)
                               -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                               -> (t1 -> t1)
                               -> (t1 -> t1)
                               -> t1)
                              -> ((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                          -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                          -> (t1 -> t1)
                          -> t1
          ============= :: ((((t1 -> t1) -> t1)
                             -> (t1 -> t1) -> (t1 -> t1) -> t1)
                            -> (t1 -> t1) -> (t1 -> t1) -> t1)
                           -> ((((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                               -> ((t1 -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                           -> (t2 -> ((t1 -> t1) -> (t1 -> t1) -> t1) -> t1)
                           -> ((((((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                               -> ((t1 -> t1) -> t1)
                               -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                               -> (t1 -> t1)
                               -> t1)
                           -> (((t3 -> (t1 -> t1) -> t1)
                                -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                                -> (t1 -> t1)
                                -> (t1 -> t1)
                                -> t1)
                               -> ((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                           -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                           -> (t1 -> t1)
                           -> t1
          ============== :: ((((t1 -> t1) -> t1)
                              -> (t1 -> t1) -> (t1 -> t1) -> t1)
                             -> (t1 -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                            -> ((((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                -> ((t1 -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                            -> (t2 -> ((t1 -> t1) -> (t1 -> t1) -> t1) -> t1)
                            -> ((((((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                 -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                -> ((t1 -> t1) -> t1)
                                -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                -> (t1 -> t1)
                                -> t1)
                            -> (((t3 -> (t1 -> t1) -> t1)
                                 -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                                 -> (t1 -> t1)
                                 -> (t1 -> t1)
                                 -> t1)
                                -> ((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                            -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                            -> (t1 -> t1)
                            -> t1
          =============== :: ((((t1 -> t1) -> t1)
                               -> (t1 -> t1) -> (t1 -> t1) -> t1)
                              -> (t1 -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                             -> ((((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                 -> ((t1 -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                             -> (t2 -> ((t1 -> t1) -> (t1 -> t1) -> t1) -> t1)
                             -> ((((((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                  -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                 -> ((t1 -> t1) -> t1)
                                 -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                 -> (t1 -> t1)
                                 -> t1)
                             -> (((t3 -> (t1 -> t1) -> t1)
                                  -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                                  -> (t1 -> t1)
                                  -> (t1 -> t1)
                                  -> t1)
                                 -> ((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                             -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                             -> (t1 -> t1)
                             -> t1
          ================ :: ((((t1 -> t1) -> t1)
                                -> (t1 -> t1) -> (t1 -> t1) -> t1)
                               -> (t1 -> t1) -> (t1 -> t1) -> t1)
                              -> ((((((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                   -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                  -> ((((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                                      -> (t1 -> t1) -> (t1 -> t1) -> t1)
                                  -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                  -> (t1 -> t1)
                                  -> t1)
                              -> (t2 -> ((t1 -> t1) -> (t1 -> t1) -> t1) -> t1)
                              -> ((((((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                   -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                  -> ((t1 -> t1) -> t1)
                                  -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                  -> (t1 -> t1)
                                  -> t1)
                              -> (((t3 -> (t1 -> t1) -> t1)
                                   -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                                   -> (t1 -> t1)
                                   -> (t1 -> t1)
                                   -> t1)
                                  -> ((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                              -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                              -> (t1 -> t1)
                              -> (t1 -> t1)
                              -> t1
          ================= :: ((((t1 -> t1) -> t1)
                                 -> (t1 -> t1) -> (t1 -> t1) -> t1)
                                -> (t1 -> t1) -> (t1 -> t1) -> t1)
                               -> ((((((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                    -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                   -> ((((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                                       -> (t1 -> t1) -> (t1 -> t1) -> t1)
                                   -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                   -> (t1 -> t1)
                                   -> t1)
                               -> (t2 -> ((t1 -> t1) -> (t1 -> t1) -> t1) -> t1)
                               -> ((((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                   -> t1 -> (t1 -> t1) -> (t1 -> t1) -> t1)
                               -> (((t3 -> (t1 -> t1) -> t1)
                                    -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                                    -> (t1 -> t1)
                                    -> (t1 -> t1)
                                    -> t1)
                                   -> ((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                               -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                               -> (t1 -> t1)
                               -> t1
          ================== :: ((((t1 -> t1) -> t1)
                                  -> (t1 -> t1) -> (t1 -> t1) -> t1)
                                 -> (t1 -> t1) -> (t1 -> t1) -> t1)
                                -> ((((((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                     -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                     -> (t1 -> t1)
                                     -> t1)
                                    -> ((((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                                        -> (t1 -> t1) -> (t1 -> t1) -> t1)
                                    -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                    -> (t1 -> t1)
                                    -> t1)
                                -> (t2 -> ((t1 -> t1) -> (t1 -> t1) -> t1) -> t1)
                                -> ((((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                    -> t1 -> (t1 -> t1) -> (t1 -> t1) -> t1)
                                -> (((t3 -> (t1 -> t1) -> t1)
                                     -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                                     -> (t1 -> t1)
                                     -> (t1 -> t1)
                                     -> t1)
                                    -> ((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                -> (t1 -> t1)
                                -> t1
        at minsky4.hs:(8,2)-(50,2)
    • In the first argument of ‘(================)’, namely ‘(==)’
      In the first argument of ‘dec’, namely
        ‘((================) (==) (===) (====) (=====))’
      In the expression:
        dec
          ((================) (==) (===) (====) (=====))
          ((============) (==) (===) (====) (=====) (======)) (======)
    • Relevant bindings include
        (======) :: ((t3 -> (t1 -> t1) -> t1)
                     -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                     -> (t1 -> t1)
                     -> (t1 -> t1)
                     -> t1)
                    -> ((t1 -> t1) -> t1) -> (t1 -> t1) -> t1
          (bound at minsky4.hs:8:2)
        (=====) :: (((((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                     -> (t1 -> t1) -> t1)
                    -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                   -> ((t1 -> t1) -> t1)
                   -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                   -> (t1 -> t1)
                   -> t1
          (bound at minsky4.hs:8:2)
        (====) :: t2 -> ((t1 -> t1) -> (t1 -> t1) -> t1) -> t1
          (bound at minsky4.hs:8:2)
        (===) :: (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                 -> ((t1 -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1
          (bound at minsky4.hs:8:2)
        (==) :: (((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                -> (t1 -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1
          (bound at minsky4.hs:8:2)
        (===============) :: ((((t1 -> t1) -> t1)
                               -> (t1 -> t1) -> (t1 -> t1) -> t1)
                              -> (t1 -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                             -> ((((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                 -> ((t1 -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                             -> (t2 -> ((t1 -> t1) -> (t1 -> t1) -> t1) -> t1)
                             -> ((((((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                  -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                 -> ((t1 -> t1) -> t1)
                                 -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                                 -> (t1 -> t1)
                                 -> t1)
                             -> (((t3 -> (t1 -> t1) -> t1)
                                  -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                                  -> (t1 -> t1)
                                  -> (t1 -> t1)
                                  -> t1)
                                 -> ((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                             -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                             -> (t1 -> t1)
                             -> t1
          (bound at minsky4.hs:8:2)
  |
8 | $(pure $ let
  |  ^^^^^^^^^^^...

minsky4.hs:8:2: error: [GHC-25897]
    • Couldn't match type ‘t1’ with ‘(t1 -> t1) -> t1’
      Expected: ((t2 -> (t1 -> t1) -> t1)
                 -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                -> ((((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                    -> (t1 -> t1) -> (t1 -> t1) -> t1)
                -> (t1 -> t1)
                -> t1
        Actual: ((t2 -> (t1 -> t1) -> t1)
                 -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                -> ((((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                    -> (t1 -> t1) -> (t1 -> t1) -> t1)
                -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                -> (t1 -> t1)
                -> t1
      ‘t1’ is a rigid type variable bound by
        the inferred type of
          ========== :: ((((t1 -> t1) -> t1)
                          -> (t1 -> t1) -> (t1 -> t1) -> t1)
                         -> (t1 -> t1) -> (t1 -> t1) -> t1)
                        -> ((((((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                             -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                            -> ((((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                                -> (t1 -> t1) -> (t1 -> t1) -> t1)
                            -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                            -> (t1 -> t1)
                            -> t1)
                        -> (((t2 -> (t1 -> t1) -> t1)
                             -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                            -> ((((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                                -> (t1 -> t1) -> (t1 -> t1) -> t1)
                            -> t2
                            -> (t1 -> t1)
                            -> t1)
                        -> ((((((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                             -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                            -> ((t1 -> t1) -> t1)
                            -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                            -> (t1 -> t1)
                            -> t1)
                        -> (((t3 -> (t1 -> t1) -> t1)
                             -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                             -> (t1 -> t1)
                             -> (t1 -> t1)
                             -> t1)
                            -> ((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                        -> (t1 -> t1)
                        -> t1
        at minsky4.hs:(8,2)-(50,2)
    • In the third argument of ‘(===========)’, namely ‘(inc (====))’
      In the expression:
        (===========) (==) (===) (inc (====)) (=====) (======)
      In an equation for ‘==========’:
          (==========) (==) (===) (====) (=====) (======)
            = (===========) (==) (===) (inc (====)) (=====) (======)
    • Relevant bindings include
        (======) :: ((t3 -> (t1 -> t1) -> t1)
                     -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                     -> (t1 -> t1)
                     -> (t1 -> t1)
                     -> t1)
                    -> ((t1 -> t1) -> t1) -> (t1 -> t1) -> t1
          (bound at minsky4.hs:8:2)
        (=====) :: (((((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                     -> (t1 -> t1) -> t1)
                    -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                   -> ((t1 -> t1) -> t1)
                   -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                   -> (t1 -> t1)
                   -> t1
          (bound at minsky4.hs:8:2)
        (====) :: ((t2 -> (t1 -> t1) -> t1)
                   -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                  -> ((((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                      -> (t1 -> t1) -> (t1 -> t1) -> t1)
                  -> t2
                  -> (t1 -> t1)
                  -> t1
          (bound at minsky4.hs:8:2)
        (===) :: (((((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                   -> (t1 -> t1) -> t1)
                  -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                 -> ((((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                     -> (t1 -> t1) -> (t1 -> t1) -> t1)
                 -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                 -> (t1 -> t1)
                 -> t1
          (bound at minsky4.hs:8:2)
        (==) :: (((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                -> (t1 -> t1) -> (t1 -> t1) -> t1
          (bound at minsky4.hs:8:2)
        (==========) :: ((((t1 -> t1) -> t1)
                          -> (t1 -> t1) -> (t1 -> t1) -> t1)
                         -> (t1 -> t1) -> (t1 -> t1) -> t1)
                        -> ((((((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                             -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                            -> ((((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                                -> (t1 -> t1) -> (t1 -> t1) -> t1)
                            -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                            -> (t1 -> t1)
                            -> t1)
                        -> (((t2 -> (t1 -> t1) -> t1)
                             -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                            -> ((((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                                -> (t1 -> t1) -> (t1 -> t1) -> t1)
                            -> t2
                            -> (t1 -> t1)
                            -> t1)
                        -> ((((((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                             -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                            -> ((t1 -> t1) -> t1)
                            -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                            -> (t1 -> t1)
                            -> t1)
                        -> (((t3 -> (t1 -> t1) -> t1)
                             -> (((t1 -> t1) -> t1) -> (t1 -> t1) -> (t1 -> t1) -> t1)
                             -> (t1 -> t1)
                             -> (t1 -> t1)
                             -> t1)
                            -> ((t1 -> t1) -> t1) -> (t1 -> t1) -> t1)
                        -> (t1 -> t1)
                        -> t1
          (bound at minsky4.hs:8:2)
  |
8 | $(pure $ let
  |  ^^^^^^^^^^^...

minsky4.hs:8:2: error: [GHC-25897]
    • Couldn't match type ‘t’ with ‘t -> t’
      Expected: (((t -> t) -> t) -> (t -> t) -> (t -> t) -> t)
                -> (t -> t) -> (t -> t) -> t
        Actual: (((t -> t) -> t) -> (t -> t) -> (t -> t) -> t)
                -> (t -> t) -> t -> t
      ‘t’ is a rigid type variable bound by
        the inferred type of === :: (t -> t) -> t
        at minsky4.hs:(8,2)-(50,2)
    • In the first argument of ‘(====)’, namely ‘zero’
      In the expression: (====) zero zero zero zero zero
      In an equation for ‘===’: (===) = (====) zero zero zero zero zero
    • Relevant bindings include
        (===) :: (t -> t) -> t (bound at minsky4.hs:8:2)
  |
8 | $(pure $ let
  |  ^^^^^^^^^^^...
```

Oof. No matter what I did I couldn't make this work. The root of the problem is that the type of Church numerals is `forall a. (a -> a) -> a -> a`, so any function on them ends up having a [higher rank](https://en.wikipedia.org/wiki/Parametric_polymorphism#Higher-rank_polymorphism) type, which GHC won't infer. I looked at some other encodings and even thought about making up a new one, but it seems that any useful encoding for numbers ends up requiring higher rank types. So I went looking for a formal proof that simply typed lambda calculus with fixed points is Turing complete, in hopes that I could copy the proof method. Instead, I found [this MathOverflow question](https://mathoverflow.net/q/261934), the answer to which is that it isn't Turing complete! The answer cites the paper [On the λY calculus](https://www.sciencedirect.com/science/article/pii/S0168007204000661), which studies STLC with added Y combinators, dubbed λY. In particular, it shows that it's decidable whether or not terms in that calculus have a normal form. Hence its halting problem is decidable, hence it is not Turing Complete.

This was very surprising to me. Often it's stated that STLC isn't Turing complete because you can't type the Y combinator or do any other kind of recursion. But evidently, it's more than that: the inability to properly type data-y things like Church numerals is also a problem.

So λY isn't Turing complete; does that mean Haskell with only `();=` isn't either? Well, Haskell is pretty far from simply typed, even ignoring GHC extensions. But as far as I can tell, we can't actually use anything that makes it different without extra characters. Defining new datatypes is out. The only built-in function we can use is `(==)` which isn't helpful. And from what I can tell, polymorphic recursion always requires type signatures (inference in the general case is undecidable), not that I'm sure it would help anyway.  I'm not aware of any combinations of language extensions that can make this work either. `RankNTypes` would certainly work—because with it we can type Church numerals—but that also requires explicit type signatures. Rank-2-types would also be enough and, unlike polymorphic recursion and rank-n-types, inference would still be decidable. Unfortunately, GHC does not implement rank-2-types; the `Rank2Types` extension is just a deprecated, now-alias for `RankNTypes`. Given all that, it seems that any Haskell program using only `();=` can be monomorphised into an equivalent λY program, making it not Turing complete.

If you peeked at [the Code Golf Stack Exchange question](https://codegolf.stackexchange.com/q/110648) linked earlier, you'll now notice that both of the Haskell answers there are incorrect, or at the very least incomplete. They both rely on combinators S, K, I, and Y being enough for Turing completeness. But λY can define all of those and still isn't Turing complete. (If you didn't peek earlier, you should check it out after reading this. There's some really neat stuff in there for the other languages.)

## Doing Better

So far both of our successful attempts use 6 characters. Can we do better? Well, if you recall from earlier...

> Without parentheses, operators are much more limited. Then, all operators must be applied to exactly 2 arguments.

> Even so, Minsky machines with at least 2 unbounded registers (i.e. capable of storing arbitrarily large numbers) are Turing complete.

That's rather serendipitous! Our Minsky machine compiler from before used functions with one parameter per register, so if we just limit it to two registers, we can eliminate parentheses. There are still a few kinks to work out though. The first is that we still have to name the parameters, so we'll need a letter. Still, we got rid of two characters and only added one, so it's a net win giving us the 5-character set `-1;=x`.

More subtly, we need to figure out how to remove the parentheses from expressions. The default operator fixity is `infixl 9` and `-` is `infixl 6`, so we can't use it within arguments. However, if we just define a new operator that uses `-`, `x-=xx=x-xx`, we can now use subtraction in the *left* argument of an operator. To affect the second register (i.e. the right argument) as well, we can swap the parameters and then use an extra function to swap them back. So instead of `x-=-xx=x-==(xx-=1)`, we'd use `x-=-xx=xx-=1===x;xx===x=x-==xx`.

Using `-1` (which we need to add one because we only have subtraction) is also an issue because if it occurs after an operator without a space, the `-` will be interpreted as a continuation of the operator. To work around this we can define `x1=1-1-1` and use `x1` in place of `-1`.

Using the notation from earlier and with these workarounds in mind, we get the translation:

$$
\begin{array}{lcl}
\texttt{n: inc(0)} & \longrightarrow & x\equiv_{2n}xx=x-=x1\equiv_{2n+2}xx;\\
\texttt{n: inc(1)} & \longrightarrow & x\equiv_{2n}xx=xx-=x1\equiv_{2n+1}x;\\
& & xx\equiv_{2n+1}x=x\equiv_{2n+2}xx;\\
\texttt{n: dec(0,m)} & \longrightarrow & 1\equiv_{2n}x=1\equiv_{2m}x;\\
& & x\equiv_{2n}xx=x-=1\equiv_{2n+2}xx;\\
\texttt{n: dec(1,m)} & \longrightarrow & x\equiv_{2n}1=x\equiv_{2m}1;\\
& & x\equiv_{2n}xx=xx-=1\equiv_{2n+1}x;\\
& & xx\equiv_{2n+1}x=x\equiv_{2n+2}xx;\\
\end{array}
$$

### Are These Machines *Really* Turing Complete?

We already learned that claims of Turing completeness without proof can be dubious. And the fact that these machines can only jump if one of the only two registers is zero seems *incredibly* limited. But this claim originates from a proof in the same book where these machines were defined, Minsky's 1967 book *Computation: Finite and Infinite Machines*, which states:

> *For any Turing machine $T$ there exists a program machine $M_T$ with just two registers that behaves the same as $T$ (in the sense described in sections 10.1 and 11.2) when started with zero in one register and $2^a3^m5^n$ in the other. This machine uses only the operations <span class="box">$'$</span> and <span class="box">$-$</span>.*

Thus proving Turing completness. ($a'$ and $a^-(n)$ are Minsky's notation for `inc(a)` and `dec(a,n)` respectively.)

However, the proof itself makes use of a third instruction: `go`, an unconditional jump. It even says at the beginning that this instruction will be assumed instead of having an additional register.

> *We recall (from 11.4) that the operation <span class="box">$a^0$</span>, i.e., put zero in register **a**, can be simulated if we have a register **w** already containing zero; then we can also use $w^-(n)$ as a <span class="box">$go(n)$</span> instruction. For our purposes here it is more convenient to assume that we have <span class="box">$a'$</span>, <span class="box">$a^-(n)$</span>, and <span class="box">$go(n)$</span> at the start.*

It took me a while trying to reconcile this inconsistency between the proof body and conclusion, but I believe that it's a genuine mistake. Minsky's previous work used machines that were not sequential by default. Every instruction specified the number of the instruction to go to next, making `go` unnecessary because every instruction is essentially followed by an unconditional jump already. Perhaps a prior draft of the book used these non-sequential machines and in changing that, some things slipped through the cracks. As more evidence of this, this sentence appears when the machines are first defined:

> *An instruction is a statement naming (1) an operation, (2) a register, and (3) the numbers of one or two other instructions.*

But this doesn't match up with anything else in the book; `inc` and other non-jumping instructions never name the number of another instruction and `dec` and other jumping instructions only ever name one.

Regardless of how this happened, I'm fairly certain that the statement as written is wrong. As a sketch of a proof, consider the fact that during the execution of a Minsky machine using only `inc` and `dec`, if the values of all of the registers are all greater than the number of instructions in the program, then execution will continue straight through to the end without anymore jumps because, even if all of the remaining instructions decrement the same register, no registers will contain zero again.

This means for a two-register machine, we can assume that at most one register is unbounded at any given time, otherwise we can end execution immediately, applying some precomputable offset to the registers based on the current instruction number. We can use this to construct a pushdown automaton corresponding to a given machine by storing one register in the stack and the other in the finite state of the automata, switching between which is which as needed. Thus two register two instruction Minsky machines are at most as powerful as pushdown automata, i.e. not Turing complete.

While it is interesting that we found a *second* thing that is often claimed to be Turing complete but isn't, in the end, this doesn't really affect us much. We can simply add in a translation for `go` to regain Turing completeness:

$$
\begin{array}{lcl}
\texttt{n: go(m)} & \longrightarrow & x\equiv_{2n}xx=x\equiv_{2m}xx;\\
\end{array}
$$

And also extend Szewczyk notation by representing `go(n)` as `-1 n`. (You could think of this as `-1` being a special register that can't be incremented and so is always zero. Or don't, your choice.)

If we wanted to keep to only two operators, we could also replace the "Decrement or jump" instruction with "Decrement and jump", also described in Minsky's book, which jumps when the register value is nonzero rather than when it is zero and, together with `inc`, can replicate the functions of both unconditional jump and "Decrement or jump" without additional registers. There isn't really a reason to do this though, and extending Szewczyk notation seems better than changing its existing semantics.

### Back To It

With that all settled, here's the Minsky machine compiler. There's not much to it, just like the last one, but adapted to the new rules.

```hs
{-# language CPP #-}
{-# language TemplateHaskell #-}
{-# options -Wno-x-partial #-}

import Control.Monad
import Data.List
import Prelude
import Language.Haskell.TH

$(pure $
  let
    ops = chunk $ fmap mkName
      $ "=-" : filter (any (=='=')) (flip replicateM "-=" =<< enumFrom 3)
    opsWithNext = zip ops $ tail ops

    prog = chunk $ read <$> words PROGRAM
    chunk (r:y:rest) = (r,y) : chunk rest
    chunk _ = []

    x = mkName "x"
    xx = mkName "xx"
    xxx = mkName "xxx"
    one = LitE $ IntegerL 1
    infix_ l o r = InfixE (Just l) (VarE o) (Just r)
    succ v = infix_ (VarE v) (mkName "-=") (VarE xxx)
    pred v = infix_ (VarE v) (mkName "-=") one
    args --> body = Clause args (NormalB body) []

    instr ((op1, op2), (next, _)) i@(1, _) =
      [ instr' op1 op2 next i
      , FunD op2
        [ [VarP x, VarP xx] --> infix_ (VarE xx) next (VarE x)
        ]
      ]
    instr ((op1, op2), (next, _)) i = [instr' op1 op2 next i]

    instr' _ _ _ (_, y) | y > length prog = error $ "Invalid jump: " ++ show y
    instr' op1 op2 next (0, -1) = FunD op1
      [ [VarP x, VarP xx] --> infix_ (succ x) next (VarE xx)
      ]
    instr' op1 op2 next (1, -1) = FunD op1
      [ [VarP x, VarP xx] --> infix_ (succ xx) op2 (VarE x)
      ]
    instr' op1 op2 next (0, y) = FunD op1
      [ [LitP $ IntegerL 1, VarP x] --> infix_ one (fst $ ops !! y) (VarE x)
      , [VarP x, VarP xx] --> infix_ (pred x) next (VarE xx)
      ]
    instr' op1 op2 next (1, y) = FunD op1
      [ [VarP x, LitP $ IntegerL 1] --> infix_ (VarE x) (fst $ ops !! y) one
      , [VarP x, VarP xx] --> infix_ (pred xx) op2 (VarE x)
      ]
    instr' op1 op2 next (-1, y) | y >= 0 = FunD op1
      [ [VarP x, VarP xx] --> infix_ (VarE x) (fst $ ops !! y) (VarE xx)
      ]
    instr' _ _ _ (r, _) = error $ "Invalid register: " ++ show r

    sub = FunD (mkName "-=")
      [ [VarP x, VarP xx] --> infix_ (VarE x) (mkName "-") (VarE xx)
      ]
    negOne = FunD xxx
      [ [] --> infix_ (infix_ one (mkName "-=") one) (mkName "-=") one
      ]
    begin = FunD x
      [ [] --> infix_ one (fst $ head ops) one
      ]
    end (op, _) = FunD op
      [ [VarP x, VarP xx] --> VarE x
      ]

  in
    [sub, negOne, begin, end (ops !! length prog)]
    ++ concat (zipWith instr opsWithNext prog)
 )

main :: IO ()
main = putStr "Generated: " *> print x
```

We can test this just like the last one. However, we need a new test program and different post-processing. But still, the result is cursed.

```console {.wrap-code}
$ runghc -DPROGRAM='"1 -1 1 -1 1 -1 0 -1 0 -1 0 -1 0 -1 0 16 1 12 0 -1 1 -1 0 8 1 15 1 -1 0 12 0 7"' -dth-dec-file minskynb.hs && sed '1d;s/^(\([-=]*\)) \([^ ]*\)/\2\1/' minskynb.dyn.th.hs | tr -d ' ()' | tr '\n' ';' | head -c-1 | tee >(sed 'a\\nputStr"Processed: "\nx' | ghci -v0 1>&2) | sed 's/./\0\n/g' | LC_COLLATE=C sort -u | sed -z 's/\n//g;s/.*/Characters: \0\n/'
Generated: 2
Processed: 2
Characters: -1;=x
```

It works!

### Expressivity

Unlike before, we can't just use the fact that Minsky machines can calculate any general recursive function because, with only two registers, they can't! This was proven by Rich Schroeppel in a note titled ["A Two Counter Machine Cannot Calculate 2^N" (pdf)](https://dspace.mit.edu/bitstream/handle/1721.1/6202/AIM-257.pdf). The problem is, that the Turing completeness proof for two register machines relies on the input and output being in a specific form. So for example, for any general recursive function $f$, we can construct a machine that calculates $2^{f(n)}$ when given the input $2^n$. But we cannot necessarily construct a machine that calculates just $f(n)$ from just $n$. (There's nothing special about the 2 here, any constant integer greater than 1 works just as well.)

Schroeppel defines the "Input Problem" and the "Output Problem". The former being to find a machine that calculates $2^n$ from $n$ and the latter being to find a machine that calculates $n$ from $2^n$. If these problems were solved then we could combine those machines with the existing proof of Turing completeness to be able to calculate any computable unary function. Unfortunately, as you may have guessed from the title of the note mentioned above, Schroeppel proved that the Input Problem is unsolvable. However, It seems likely that our Haskell character set is more powerful than a two-register Minsky machine. Indeed, these problems can be solved with plain recursion and only a little bit of finagling:

```hs
1 `exp` x = x
xx `exp` x = xx `minus` 1 `exp` x `mul` x

1 `mul` x = x
xx `mul` x = xx `minus` 1 `mul` x `add` x

1 `log` x = zero
xx `log` x = 1 `add` xx `div1` x `log` x `add` 1

1 `div1` x = zero
xx `div1` x = xx `minus` x `div1` x `add` 1

x `add` xx = xx - zero `minus` x
x `minus` xx = x - xx

zero = 1 - 1
two = 1 `add` 1
```

Or with just our five characters:

```hs {.wrap-code}
1=-x=x;xx=-x=xx-=1=-x-=-x;1-=-x=x;xx-=-x=xx-=1-=-x-==x;x-==xx=xx-xxx-=x;1=--x=xxx;xx=--x=1-==xx=-=x=--x-==1;1=-=x=xxx;xx=-=x=xx-=x=-=x-==1;x-=xx=x-xx;xxx=1-1;xxxx=1-==1
```

Here ``n `exp` two`` (or `n=-xxxx`) computes $2^n$ and ``n `log` two`` (or `n=--xxxx`) computes $m$ when $n = 2^m$.

This suffices to show we can compute arbitrary (computable) *unary* functions (with a second ignored parameter, because we must have two), but what about larger arity functions? Binary functions are actually quite easy if we use Gödel encoding, representing two natural numbers $x$ and $y$ by a single product of prime powers $2^x 3^y$. This is very similar to how Minsky's proof of Turing completeness for the two register machine works, but to avoid addressing the input and output problems again we can just work directly with the computable function which we already showed work in the unary case.

Let $f$ be a binary computable function, then there exists a unary computable function $g$, such that $f(x, y) = g(2^x 3^y)$. So if ``x `g` 1`` computes $g(x)$, then we can define an `f` such that ``x `f` y`` computes `f(x, y)` as follows:

```hs
x `f` xx = x `exp` two `f'` xx
x `f'` xx = xx `exp` three `mul` x `g` 1
```

Obviously, we can't have operators with higher arity than binary, but we *can* stretch our definition of a Haskell "function" a bit. We'll say a sequence of $n-1$ operators `f1`, ..., `f{n-1}` computes a n-ary function $f$ if ``x1 `f1` x2 `f2` ... `f{n-1}` xn`` evaluates to $f(x1, x2, ..., xn)$ for all $x1, x2, ..., xn$. Then we can use Gödel encodings again. If $f$ is an n-ary computable function then there is a corresponding computable function $g$ such that $f(x_1, ..., x_n) = g(p_1^{x_1}...p_n^{x_n})$ where $p_1, ..., p_n$ are the first $n$ prime numbers. Once more, like before, we'll say ``x `g` 1`` computes $g(x)$, and then define a sequence `f1`, ..., `f{n-1}` that computes $f$ like so:

```hs
x `f1` xx = x `exp` p1 `f1'` xx
x `f1'` xx = xx `exp` p2 `mul` x
x `f2` xx = xx `exp` p3 `mul` x
...
x `f{n-2}` xx = xx `exp` p{n-1} `mul` x
x `f{n-1}` xx = xx `exp` pn `mul` x `g` 1

p1 = 1 - p1'
p1' = 1 - 1 - 1
p2 = 1 - p2'
p2' = 1 - 1 - 1 - 1
p3 = 1 - p3'
p3' = 1 - 1 - 1 - 1 - 1 - 1
...
```

So with a little bit of stretching definitions, we get almost the same expressivity as the previous number character set. The one missing thing is higher-order functions. As far as I can tell, there's no way to have those without parentheses, spaces, or a predefined operator like `$`.

### Efficiency

The performance of the previous number character set was already pretty bad. Now it's worse. Someone dedicated enough should be able to do a lot of things better than the general recursive functions, Minsky machines, Haskell pipeline described above, maybe even matching the time complexity of the same functions with the other number character set. But as soon as Gödel encodings were actually needed, things would likely become completely unusable fairly quickly.

## More Lists

So if we can get rid of the parentheses from our number character set, can we get rid of the brackets from our list character set? We used the brackets for both grouping and nil, but not for operators like the parentheses (mostly because that doesn't work). So getting rid of them would leave us with `:;=x`, only four characters! Unfortunately, this doesn't quite work out. As mentioned earlier, we can't pattern match on `:` without brackets or parentheses because its precedence (`infixr 5`) is lower than the default operator precedence (`infixl 9`). However, if we manage to do without nil, we can replace the brackets with parentheses and then get rid of `x` by using operator names instead, giving us only five characters, `():;=`.

Let's look back at our brainfuck interpreter from before and try to adapt it. With parentheses, we can get rid of most of our uses of nil by changing the type of our encoded strings from `[[[a]]]` to `[[()]]`. The remaining uses of nil are matching on specific length lists and constructing nils for initial arguments and base cases.

The first use can be eliminated by ordering our list pattern matches from largest to smallest and inserting dummy clauses to ensure that when we want to match on a list of length n, we can match on a list of length n or greater because lists greater than length n were already caught by previous clauses. If that doesn't make sense, here's a small example: we can replace

```hs
foo (():():[]) = "two"
foo (():():():[]) = "three"
foo (():():():():():():[]) = "six"
foo _ = "other"
```

with

```hs
foo (():():():():():():():_) = "other"
foo (():():():():():():_) = "six"
foo (():():():():_) = "other"
foo (():():():_) = "three"
foo (():():_) = "two"
foo _ = "other"
```

To eliminate the second use, we can steal the nils we need from our inputs using a function like this:

```hs
nil :: [a] -> [a]
nil (_:xs) = nil xs
nil xs = xs
```

Using this we're restricted in the type of our nils, but still, this covers all of our uses except for the stack, which is a list of encoded "strings" so the list type is nested one layer deeper than our inputs. We could solve this in a variety of ways. We could pass one of our inputs in a singleton list. Or we could come up with a way of encoding `[[[()]]]` as `[[()]]`. But the way we'll do it is by just not using a finite stack. As long as the brackets in our input brainfuck program are balanced, we never actually inspect the initial stack. So we can use something like `repeat prog` instead of `[]`. This gives unmatched closing brackets some strange semantics, but that's fine. We'll just assume the input is well-formed.

With all of that, we should now be prepared to write our revised interpreter. Like before, we start out with readable-ish code and progressively mangle it.

```hs
bf :: String -> String -> String
bf prog input = decode $ prog =: encode input

encode :: String -> [[()]]
encode = map (flip replicate () . fromEnum)

decode :: [[()]] -> String
decode = map (toEnum . length)

------------------------------------------------------------------------------

prog =: input = run [] [] [] zeros prog input

zeros = [] : zeros

run (jump:stack) left (():here) right (']':prog) input = run (jump:stack) left (():here) right jump input
run (jump:stack) left here right (']':prog) input = run stack left here right prog input
run stack left (():here) right ('[':prog) input = run (prog:stack) left (():here) right prog input
run stack left here right ('[':prog) input = run stack left here right (skip prog) input
run stack left here (next:right) ('>':prog) input = run stack (here:left) next right prog input
run stack (prev:left) here right ('<':prog) input = run stack left prev (here:right) prog input
run stack left here right ('.':prog) input = here : run stack left here right prog input
run stack left (():here) right ('-':prog) input = run stack left here right prog input
run stack left here right (',':prog) (char:input) = run stack left char right prog input
run stack left here right ('+':prog) input = run stack left (():here) right prog input
run stack left here right (instr:prog) input = run stack left here right prog input
run stack left here right prog input = []

skip (']':prog) = prog
skip ('[':prog) = skip (skip prog)
skip (instr:prog) = skip prog
skip [] = []
```

This is very similar to our starting code from before, the main differences are that we no longer need to worry about nested patterns, so we don't split `run` and `skip` into two functions, and we order character pattern matches in reverse ASCII order.

The next step is to have the program be encoded. We'll have to add do-nothing clauses to the beginning of `run` and `skip` as well as between matches of characters that have a gap between their ASCII values, specifically `]` and `[`, `[` and `>`, `>` and `<`, and `<` and `.`.

```hs {.wrap-code}
bf :: String -> String -> String
bf prog input = decode $ encode prog =: encode input

encode :: String -> [[()]]
encode = map (flip replicate () . fromEnum)

decode :: [[()]] -> String
decode = map (toEnum . length)

------------------------------------------------------------------------------

prog =: input = run [] [] [] zeros prog input

zeros = [] : zeros

run stack left here right ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) input = run stack left here right prog input
run (jump:stack) left (():here) right ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) input = run (jump:stack) left (():here) right jump input
run (jump:stack) left here right ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) input = run stack left here right prog input
run stack left here right ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) input = run stack left here right prog input
run stack left (():here) right ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) input = run (prog:stack) left (():here) right prog input
run stack left here right ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) input = run stack left here right (skip prog) input
run stack left here right ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) input = run stack left here right prog input
run stack left here (next:right) ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) input = run stack (here:left) next right prog input
run stack left here right ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) input = run stack left here right prog input
run stack (prev:left) here right ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) input = run stack left prev (here:right) prog input
run stack left here right ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) input = run stack left here right prog input
run stack left here right ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) input = here : run stack left here right prog input
run stack left (():here) right ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) input = run stack left here right prog input
run stack left here right ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) (char:input) = run stack left char right prog input
run stack left here right ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) input = run stack left (():here) right prog input
run stack left here right (instr:prog) input = run stack left here right prog input
run stack left here right prog input = []

skip ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) = skip prog
skip ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) = prog
skip ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) = skip prog
skip ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) = skip (skip prog)
skip (instr:prog) = skip prog
skip [] = []
```

Now we remove the `[]`s as discussed earlier.

```hs {.wrap-code}
prog =: input = run (rep prog) (nil prog) (nil (hed prog)) (rep (nil (hed prog))) prog input

hed (x:xs) = x

nil (x:xs) = nil xs
nil xs = xs

rep x = x : rep x

run stack left here right ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) input = run stack left here right prog input
run (jump:stack) left (():here) right ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) input = run (jump:stack) left (():here) right jump input
run (jump:stack) left here right ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) input = run stack left here right prog input
run stack left here right ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) input = run stack left here right prog input
run stack left (():here) right ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) input = run (prog:stack) left (():here) right prog input
run stack left here right ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) input = run stack left here right (skip prog) input
run stack left here right ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) input = run stack left here right prog input
run stack left here (next:right) ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) input = run stack (here:left) next right prog input
run stack left here right ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) input = run stack left here right prog input
run stack (prev:left) here right ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) input = run stack left prev (here:right) prog input
run stack left here right ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) input = run stack left here right prog input
run stack left here right ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) input = here : run stack left here right prog input
run stack left (():here) right ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) input = run stack left here right prog input
run stack left here right ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) (char:input) = run stack left char right prog input
run stack left here right ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) input = run stack left (():here) right prog input
run stack left here right (instr:prog) input = run stack left here right prog input
run stack left here right prog input = nil prog

skip ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) = skip prog
skip ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) = prog
skip ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) = skip prog
skip ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():i):prog) = skip (skip prog)
skip (instr:prog) = skip prog
skip prog = prog
```

Now the renaming...

```hs {.wrap-code}
(==) =: (=:) = (===) ((====) (==)) ((=:=) (==)) ((=:=) ((=::) (==))) ((====) ((=:=) ((=::) (==)))) (==) (=:)

(=::) ((==):(=:)) = (==)

(=:=) ((==):(=:)) = (=:=) (=:)
(=:=) (=:) = (=:)

(====) (==) = (==) : (====) (==)

(===) (=::) (===:) (==:=) (==::) ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==)) (=:) = (===) (=::) (===:) (==:=) (==::) (==) (=:)
(===) ((=:==):(=::)) (===:) (():(==:=)) (==::) ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==)) (=:) = (===) ((=:==):(=::)) (===:) (():(==:=)) (==::) (=:==) (=:)
(===) ((=:==):(=::)) (===:) (==:=) (==::) ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==)) (=:) = (===) (=::) (===:) (==:=) (==::) (==) (=:)
(===) (=::) (===:) (==:=) (==::) ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==)) (=:) = (===) (=::) (===:) (==:=) (==::) (==) (=:)
(===) (=::) (===:) (():(==:=)) (==::) ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==)) (=:) = (===) ((==):(=::)) (===:) (():(==:=)) (==::) (==) (=:)
(===) (=::) (===:) (==:=) (==::) ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==)) (=:) = (===) (=::) (===:) (==:=) (==::) ((==:) (==)) (=:)
(===) (=::) (===:) (==:=) (==::) ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==)) (=:) = (===) (=::) (===:) (==:=) (==::) (==) (=:)
(===) (=::) (===:) (==:=) ((=:==):(==::)) ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==)) (=:) = (===) (=::) ((==:=):(===:)) (=:==) (==::) (==) (=:)
(===) (=::) (===:) (==:=) (==::) ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==)) (=:) = (===) (=::) (===:) (==:=) (==::) (==) (=:)
(===) (=::) ((=:==):(===:)) (==:=) (==::) ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==)) (=:) = (===) (=::) (===:) (=:==) ((==:=):(==::)) (==) (=:)
(===) (=::) (===:) (==:=) (==::) ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==)) (=:) = (===) (=::) (===:) (==:=) (==::) (==) (=:)
(===) (=::) (===:) (==:=) (==::) ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==)) (=:) = (==:=) : (===) (=::) (===:) (==:=) (==::) (==) (=:)
(===) (=::) (===:) (():(==:=)) (==::) ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==)) (=:) = (===) (=::) (===:) (==:=) (==::) (==) (=:)
(===) (=::) (===:) (==:=) (==::) ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==)) ((=:==):(=:)) = (===) (=::) (===:) (=:==) (==::) (==) (=:)
(===) (=::) (===:) (==:=) (==::) ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==)) (=:) = (===) (=::) (===:) (():(==:=)) (==::) (==) (=:)
(===) (=::) (===:) (==:=) (==::) ((=:==):(==)) (=:) = (===) (=::) (===:) (==:=) (==::) (==) (=:)
(===) (=::) (===:) (==:=) (==::) (==) (=:) = (=:=) (==)

(==:) ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==)) = (==:) (==)
(==:) ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==)) = (==)
(==:) ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==)) = (==:) (==)
(==:) ((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==)) = (==:) ((==:) (==))
(==:) ((=:):(==)) = (==:) (==)
(==:) (==) = (==)
```

And finally, we remove the spaces and replace newlines with semicolons.

```hs {.wrap-code}
(==)=:(=:)=(===)((====)(==))((=:=)(==))((=:=)((=::)(==)))((====)((=:=)((=::)(==))))(==)(=:);(=::)((==):(=:))=(==);(=:=)((==):(=:))=(=:=)(=:);(=:=)(=:)=(=:);(====)(==)=(==):(====)(==);(===)(=::)(===:)(==:=)(==::)((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==))(=:)=(===)(=::)(===:)(==:=)(==::)(==)(=:);(===)((=:==):(=::))(===:)(():(==:=))(==::)((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==))(=:)=(===)((=:==):(=::))(===:)(():(==:=))(==::)(=:==)(=:);(===)((=:==):(=::))(===:)(==:=)(==::)((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==))(=:)=(===)(=::)(===:)(==:=)(==::)(==)(=:);(===)(=::)(===:)(==:=)(==::)((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==))(=:)=(===)(=::)(===:)(==:=)(==::)(==)(=:);(===)(=::)(===:)(():(==:=))(==::)((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==))(=:)=(===)((==):(=::))(===:)(():(==:=))(==::)(==)(=:);(===)(=::)(===:)(==:=)(==::)((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==))(=:)=(===)(=::)(===:)(==:=)(==::)((==:)(==))(=:);(===)(=::)(===:)(==:=)(==::)((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==))(=:)=(===)(=::)(===:)(==:=)(==::)(==)(=:);(===)(=::)(===:)(==:=)((=:==):(==::))((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==))(=:)=(===)(=::)((==:=):(===:))(=:==)(==::)(==)(=:);(===)(=::)(===:)(==:=)(==::)((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==))(=:)=(===)(=::)(===:)(==:=)(==::)(==)(=:);(===)(=::)((=:==):(===:))(==:=)(==::)((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==))(=:)=(===)(=::)(===:)(=:==)((==:=):(==::))(==)(=:);(===)(=::)(===:)(==:=)(==::)((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==))(=:)=(===)(=::)(===:)(==:=)(==::)(==)(=:);(===)(=::)(===:)(==:=)(==::)((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==))(=:)=(==:=):(===)(=::)(===:)(==:=)(==::)(==)(=:);(===)(=::)(===:)(():(==:=))(==::)((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==))(=:)=(===)(=::)(===:)(==:=)(==::)(==)(=:);(===)(=::)(===:)(==:=)(==::)((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==))((=:==):(=:))=(===)(=::)(===:)(=:==)(==::)(==)(=:);(===)(=::)(===:)(==:=)(==::)((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==))(=:)=(===)(=::)(===:)(():(==:=))(==::)(==)(=:);(===)(=::)(===:)(==:=)(==::)((=:==):(==))(=:)=(===)(=::)(===:)(==:=)(==::)(==)(=:);(===)(=::)(===:)(==:=)(==::)(==)(=:)=(=:=)(==);(==:)((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==))=(==:)(==);(==:)((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==))=(==);(==:)((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==))=(==:)(==);(==:)((():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():():(=:=:)):(==))=(==:)((==:)(==));(==:)((=:):(==))=(==:)(==);(==:)(==)=(==)
```

Done! Let's test it.

```console {.wrap-code}
$ ghci bfffr.hs -e 'putStrLn $ bf "_[[\\]+]++++++++[>++++[>++>+9+J+>+++>+<<<<-]>+>+>->>+[<]<#-]>>.>A---.++++7H+++..+++.>>.<-.5<.+++.---f---.-3-------.>>_+.>++.>+++++++a++++>,.<[>+.<-]<.>>>+++++++++++>,.<[>+.<-]" "aA"'
Hello World!
abcdefghijkl
ABCDEFGHIJKL
```

Perfect!

### Expressivity

Because of the lack of nils, functions defined with this character set can only return a finite list when they receive one of the same type as input. This means it can't express everything that our previous list set can, but there's still an okay chunk of things it can do such as type-restricted versions of `foldr` and `zipWith`.

### Efficiency

For the most part, the efficiency here is pretty good. The only real issue is the extra linear factor whenever we have to extract the nil from an input.

# Close Calls and More

Those two 5-character sets were the best I was able to do. However, there are a few similar or failed attempts I'd like to share too, just because they're interesting.

## Lists With Less

My first attempt at a 5-character set was `;=[]x`. I knew going in this wouldn't work, but I played around with it a bit anyway because it was interesting. The idea here is that we could use arbitrarily nested lists like natural numbers (`[]` is 0, `[x]` is x+1). Unfortunately for this to work in a typed language, we need a type for arbitrarily nested lists, which requires equirecursive types, which Haskell does not have. GHC does, however, give us enough tools to fake it. The most important being `-XOverloadedLists` which allows us to take control of the built-in list syntax. Unfortunately, this isn't quite enough because GHC's type inference really doesn't like doing this with no signatures. Specifically, the `IsList` class and its associated type family `Item` cause issues. The easiest way to fix this is to just circumvent the problem entirely using `-XRebindableSyntax`. This allows us to not use the `IsList` class at all; we just need any old functions named `fromListN` and `toList`. With these, we can make yet another Minsky machine compiler:

```hs
{-# language CPP #-}
{-# language OverloadedLists #-}
{-# language RebindableSyntax #-}
{-# language TemplateHaskell #-}
{-# options -Wno-x-partial #-}

import Data.List
import Prelude
import Language.Haskell.TH

fromListN :: Int -> [Integer] -> Integer
fromListN 0 = const 0
fromListN _ = succ . sum

toList :: Integer -> [Integer]
toList 0 = mempty
toList n = pred n : mempty

#ifdef PROGRAM

$(pure $
  let xs = iterate ('x':) "xx"

      prog = chunk $ read <$> words PROGRAM
      chunk (r:y:rest) = (r,y) : chunk rest
      chunk _ = mempty

      args x = take registers $ mkName <$> ("x":xs) \\ (x:mempty)
      registers = maximum (fst <$> prog) + 1

      modify 0 f (x:xs) = f x : xs
      modify n f (x:xs) = x : modify (n-1) f xs

      clause r pf ef j = Clause
        (modify r pf $ ListP . pure . VarP <$> args j)
        (NormalB . foldl AppE (VarE $ mkName j) . modify r ef $ ListE . pure . VarE <$> args j)
        mempty

      instr x (r, -1) = FunD (mkName x) (clause r id (ListE . pure) ('x':x) : mempty)
      instr x (r, y) = FunD (mkName x)
        ( clause r (ListP . pure) id ('x':x)
        : clause r (const . ListP . pure $ ListP mempty) (const . ListE . pure $ ListE mempty) (xs !! y)
        : mempty)

      begin = FunD (mkName "x") (Clause mempty (NormalB $ foldl AppE (VarE $ mkName "xx") (replicate registers . ListE . pure $ ListE mempty)) mempty : mempty)
      end x = FunD (mkName x) (Clause (ListP . pure . VarP <$> args x) (NormalB . VarE . head $ args x) mempty : mempty)

  in begin : end (xs !! length prog) : zipWith instr xs prog
 )

main :: IO ()
main = putStr "Generated: " *> print x

#endif
```

And the test:

```console {.wrap-code}
$ runghc -DPROGRAM='"1 -1 1 -1 1 -1 2 -1 2 -1 2 -1 2 -1 2 16 1 12 0 -1 3 -1 4 8 3 15 1 -1 4 12 4 7"' -dth-dec-file minsky5.hs && sed -z 's/[^\n]*\n//;s/ //g;s/\n=/=/g;s/\n\(.\)/;\1/g' minsky5.dyn.th.hs | tee >(sed 'a\putStr"Processed: "\nx' | ghci minsky5.hs -XOverloadedLists -XRebindableSyntax -v0 1>&2) | sed 's/./\0\n/g' | LC_COLLATE=C sort -u | sed -z 's/\n//g;s/.*/Characters: \0\n/'
Generated: 12
Processed: 12
Characters: ;=[]x
```

There are a few things to notice here. First, Instead of an actual infinitely nested list, I've just used `Integer`s. For an actual nested list type, we could do

```hs
newtype WList = WList { toList :: [WList] }

fromListN :: a -> [WList] -> WList
fromListN _ = WList

depth :: WList -> Integer
depth = maximum' . map depth . toList where
  maximum' xs | null xs = 0
  maximum' xs = 1 + maximum xs
```

and call `depth` (or something similar) on the result, but using `Integer` directly is more efficient (probably), not that it matters much for our simple tests.

Another thing is that I've used `#ifdef` so that we can easily import the overloaded list functions into ghci for testing.

The final thing to notice, which probably should have been first, is that this is clearly cheating. The generated code may only use the characters `;=[]x`, but that code calls other (not predefined) code that isn't restricted. In the `WList` case, there's a tenuous argument to be made that all of the non-restricted code is erased before runtime (because it's just wrapping and unwrapping a newtype) and it's just there to make the type checker happy, but that point is rather weak. Part of programming in a statically typed language is making the type-checker happy. Ignoring that because it's inconvenient isn't really in the spirit of the challenge.

A more compelling argument could be made if we managed to ditch `-XRebindableSyntax`. In that case, we could hopefully have an entry point with a type something like `(IsList a, Item a ~ a) => a -> a -> a`. The code would not depend on anything being predefined, the instance would be passed in while calling. This would be somewhat analogous to Church numerals, where you need to do something like `mynumber (+1) 0` to actually print the "number". Unfortunately, this doesn't seem to be possible. Overloaded lists are just too polymorphic. There's nothing to guide type inference and GHC won't infer ambiguous types by putting unsolved variables into the type signature because that would be a wild thing for it to do.

## Minus Subtraction, Plus Addition

In the number character sets, I always use `-`, which seems like the obvious choice because it can be used for subtraction, negation, and (combining them) addition. But in Haskell 98, `+` also can be used for subtraction! Specifically n+k patterns can be used to subtract constants, which is enough. So `()+1;=` is also Turing complete. We can even make it zero-based instead of one-based much easier because `n+k` only matches against a number `x` when `x >= k`. Specifically, this means `n+1` doesn't match zero. This ends up simplifying the translation, and `inc` and the first clause of `dec` turn out very symmetric in a pleasing way.

$$
\begin{array}{lcl}
\texttt{n: inc(a)} & \longrightarrow & (\equiv_n)(\equiv'_0)\cdots(\equiv'_k)=(\equiv_{n+1})(\equiv'_0)\cdots(\equiv'_{a-1})((\equiv'_a)+1)(\equiv'_{a+1})\cdots(\equiv'_k);\\
\texttt{n: dec(a,m)} & \longrightarrow & (\equiv_n)(\equiv'_0)\cdots(\equiv'_{a-1})((\equiv'_{a})+1)(\equiv'_{a+1})\cdots(\equiv'_k)=(\equiv_{n+1})(\equiv'_0)\cdots(\equiv'_k);\\
&&(\equiv_n)(\equiv'_0)\cdots(\equiv'_k)=(\equiv_{m})(\equiv'_0)\cdots(\equiv'_k);\\
\end{array}
$$

Unfortunately, we can't immediately adapt the TH compiler from earlier because Template Haskell does not support `NPlusKPatterns`. Instead, we can fake it with a pattern synonym and do some post-processing to check that the actual thing works too.

```hs
{-# language CPP #-}
{-# language NPlusKPatterns #-}
{-# language PatternSynonyms #-}
{-# language TemplateHaskell #-}
{-# language ViewPatterns #-}
{-# options -Wno-x-partial #-}

import Control.Monad
import Data.List
import Prelude
import Language.Haskell.TH

pattern P x <- (\y -> if y >= 1 then Just (y-1) else Nothing -> Just x)

$(pure $
  let ops = flip replicateM "+=" =<< enumFrom 3
      opsWithNext = zip ops $ tail ops

      prog = chunk $ read <$> words PROGRAM
      chunk (r:y:rest) = (r,y) : chunk rest
      chunk _ = []

      args reserved = take registers $ mkName <$> ("+":"++":"+=":"=+":"==":ops) \\ reserved
      registers = maximum (fst <$> prog) + 1

      modify 0 f (x:xs) = f x : xs
      modify n f (x:xs) = x : modify (n-1) f xs

      clause r pf ef j reserved = Clause
        (modify r pf $ VarP <$> args (j:reserved))
        (NormalB . foldl AppE (VarE $ mkName j) . modify r ef $ VarE <$> args (j:reserved))
        []

      instr (op, next) (r, -1) = FunD (mkName op)
        [ clause r id (InfixE (Just . LitE $ IntegerL 1) (VarE $ mkName "+") . Just) next ["+"]
        ]
      instr (op, next) (r, y) = FunD (mkName op)
        [ clause r (ConP (mkName "P") [] . pure) id next []
        , clause r id id (ops !! y) []
        ]

      zero = ValD
        (ConP (mkName "P") [] (pure . VarP $ mkName "=+"))
        (NormalB . LitE $ IntegerL 1)
        []
      begin = FunD (mkName "+=") [
        Clause [] (NormalB $
          foldl AppE (VarE $ mkName "+++") (replicate registers . VarE $ mkName "=+")) []
        ]
      end op = FunD (mkName op) [Clause (VarP <$> args []) (NormalB . VarE . head $ args []) []]

  in zero : begin : end (ops !! length prog) : zipWith instr opsWithNext prog
 )

main :: IO ()
main = print (+=)
```

(We can't just use `pattern P x <- x + 1` because pattern synonyms *also* don't support `NPlusKPatterns`!)

```console {.wrap-code}
$ runghc -DPROGRAM='"1 -1 1 -1 1 -1 2 -1 2 -1 2 -1 2 -1 2 16 1 12 0 -1 3 -1 4 8 3 15 1 -1 4 12 4 7"' -dth-dec-file minskyna.hs && sed -z 's/[^\n]*\n//;s/ //g;s/\n=/=/g;s/\n\(.\)/;\1/g;s/P(\([^)]*\))/((\1)+1)/g' minskyna.dyn.th.hs | tee >(sed 'a\\nputStr"Processed: "\n(+=)' | ghci -XHaskell98 -v0 1>&2) | sed 's/./\0\n/g' | LC_COLLATE=C sort -u | sed -z 's/\n//g;s/.*/Characters: \0\n/'
Generated: 12
Processed: 12
Characters: ()+1;=
```

We get 12 now instead of the 13 from the `()-1;=` compiler because this one is zero-based, but accounting for that everything looks peachy!

Unfortunately, we can't also do `+1;=x` though, for the same reason as `:;=x`: `xx===x+1=x` parses as `(xx===x)+1=x`, which is invalid.

## No Letters

Throughout this post, I've used `x` whenever a non-operator name was needed. I chose this because it's one of the easier letters to distinguish long sequences of `x`, which was helpful for debugging (though it still made me go cross-eyed occasionally trying to see the difference between `xxxxxxxx` and `xxxxxxxxx`). But if we wanted a character set without letters all we need to do is swap out `x` with `_`. Sequences of two or more `_`s are perfectly valid identifiers in Haskell. This could also make some programs shorter because we can use wildcard patterns instead of patterns like `[]` and `()` and also instead of having to come up with a unique name for each unused variable match in a clause. In particular, I like the character set `-1;=_` because so many of the characters are similar. For fun, here's a program that tests if a number is prime:

```hs {.wrap-code}
_===1=1==_1;_===__=__-=1-=-__==1;_1=1-1-1;__-=___=__-___;1-=-_=1;__-=-___=___-=_1--=__-==___;1-==_=_1;__-==___=__-=1-=-___;1--=_=1;__--=___=__=-___-=_1=--___;1=--__=__;__=--___=__-=1--=___;__=-1=__-1;1=-_=1-1;__=-___=___-=1=-=__;___=-=__=__-=1=-___
```

```hs
ghci> filter (1===) [1..100]
[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
```

## More Expressive Character Sets

I also had fun coming up with character sets that aren't just Turing complete, but able to express more functions.

My favorite of these is `:[].,x=;`. It's the same as `:[]x=;` from before, which was able to express any list function, but with `.` and `,` to give us arithmetic sequences. This allows us to manipulate anything with an `Enum` instance, and even better if it's also `Bounded`. The comma helps because `[x..]` and `[x..y]` only allow us to count up, so we can't return anything lower than provided in the input. With the comma, we can get both `succ x` by pattern matching on `[x..]` (by passing it to a function that pattern matches on its argument) and then `pred x` by pattern matching on `[succ x,x,..]`:

```hs
-- xx[x] = succ x
xx[x]=xxx[[x..]];
xxx[x:xx:xxx]=xx;

-- xxxx[x] = pred x
xxxx[x]=xxxxx[[xx[x],x..]];
xxxxx[x:xx:xxx:xxxx]=xxx;
```

The one limitation here is that we can't do `pred maxBound` because our `succ maxBound` will error, just like the real one.

We can also do comparisons because `[x..y]` is non-empty if and only if `x <= y` and a singleton if and only if `x == y`.

```hs
-- xx[x][y][lt][eq][gt] = case compare x y of { LT -> lt; EQ -> eq; GT -> gt }
xx[x][xx]=xxx[[x..xx]];
xxx[[]][x][xx][xxx]=xxx;
xxx[[xxxx]][x][xx][xxx]=xx;
xxx[xxxx][x][xx][xxx]=x;
```

As an example of using this set, here's the derivation of functions analogous to the `encode` and `decode` functions from the first brainfuck interpreter above:

```hs
encode :: [String] -> [[[a]]]
decode :: [Char] -> [[[a]]] -> String

encode[[]]=[]
encode[x:xs]=rep[[x..]]:encode[xs]

rep[x:y:_]=empty[[y,x..]]

empty[_:_:[]]=[]
empty[_:xs]=[]:empty[xs]

decode[_][[]]=[]
decode[c][x:xs]=after[[zero[[c..]]..]][x]:decode[c][xs]

after[x:_][[]]=x
after[_:xs][_:ys]=after[xs][ys]

zero[x:y:_]=last'[[y,x..]]

last'[[x]]=x
last'[x:xs]=last'[xs]
```

```hs
x[[]]=[]
x[xx:xxx]=xxxxxxx[[xx..]]:x[xxx]

xxxxxxx[x:xx:xxxx]=xxx[[xx,x..]]


xxx[x:xx:[]]=[]
xxx[xx:x]=[]:xxx[x]

xx[x][[]]=[]
xx[x][xxx:xxxx]=xxxxx[[xxxxxx[[x..]]..]][xxx]:xx[x][xxxx]

xxxxx[x:xx][[]]=x
xxxxx[xxx:x][xxxx:xx]=xxxxx[x][xx]

xxxxxx[x:xx:xxx]=xxxx[[xx,x..]]

xxxx[[x]]=x
xxxx[xx:x]=xxxx[x]
```

```hs {.wrap-code}
x[[]]=[];x[xx:xxx]=xxxxxxx[[xx..]]:x[xxx];xxxxxxx[x:xx:xxxx]=xxx[[xx,x..]];xxx[x:xx:[]]=[];xxx[xx:x]=[]:xxx[x];xx[x][[]]=[];xx[x][xxx:xxxx]=xxxxx[[xxxxxx[[x..]]..]][xxx]:xx[x][xxxx];xxxxx[x:xx][[]]=x;xxxxx[xxx:x][xxxx:xx]=xxxxx[x][xx];xxxxxx[x:xx:xxx]=xxxx[[xx,x..]];xxxx[[x]]=x;xxxx[xx:x]=xxxx[x]
```

The extra list wrapping can be gotten rid of as explained in the expressivity section of the first set, `:;=[]x`, but `decode` needs a character as an input as it has no way to summon one from nothing. Though adding `'` to the set would remedy this and adding a digit character would do the same for integers.

# Wrapping up

So all-in-all, I came up with two 5-character sets—`-1;=x` and `():;=`—that, when restricted to them, leave Haskell still Turing complete and also found out that the obvious 4-character set, `();=`, likely doesn't work out. I learned a lot doing this. I'd never done anything with Minsky machines or any other register machines and the fact that simply typed lambda calculus is still not Turing complete if you add in fixpoints really surprised me. I'd love to see what other people come up with. Maybe there is a 4-character Turing complete set or more 5-character ones.
