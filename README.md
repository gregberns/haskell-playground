# haskell-poc-apply

## Purpose

Better understand applicatives, profunctors, and any of the other crazy shit category theory or Haskell has to offer.

## To Run

```bash
stack build && stack exec haskell-poc-apply-exe
```

```
ghci -isrc 
:l ComonadStore
ComonadStore.main
```

## Language and Expression Evaluation and Simplification

Migrate OCaml language program

"Normalization by evaluation" - turning a program `const id` into the 'simplified' version - `id`
* https://www.youtube.com/watch?v=25RmUl88jSw


## Applicatives

* Applicatives -> Alternative

Attempting to understand [this article](http://comonad.com/reader/2012/abstracting-with-applicatives/) and what it is trying to do.

## Contravariant

Contravariant -> Divisible (Analog of Applicable)-> Decidable (analog of Alternative)

* Divisible and Decidable - talk at YOW 2016? http://lamdajam.yowconference.com.au

Why: Sorting - If we use Discrimination we can do sorting in linear time

### Profunctors

Profunctors are contravariant functors.

Other kinds: Strong -> Arrow, Choice, Closed
            Category -> Arrow

Uses: Lens

```hs
class Profuctor p where
  dimap :: (a-> b) -> (c -> d) -> p b c -> p a d

lmap :: (a -> b) -> p b c -> p a c
rmap :: (c -> d) -> p b c -> p b d

class Profunctor (->) where
  dimap :: (a -> b) -> (c -> d) -> (b -> c) -> (a -> d)
  dimap ab cd bc = cd . bc . ab
```

Control.Arrow.Kleisli

Replicate the work of [Hardy Jones'](https://github.com/joneshf) [Elm Profunctor](https://github.com/joneshf/elm-profunctors) library, with an objective to understand component composition.

In `ProgramThree.hs`, the `Model a` has an `a` that is `a -> b`, which from my current understanding is a profuctor.

### Another thing to learn

"Use CoYoneda to delay a functor instance"
- Brian Lonsdorf, Magic Read Along, Episode: "I am not full of Beans!", 17:25

### Sorting

http://www.magicreadalong.com/episode/19
https://www.youtube.com/watch?v=JogetnhuUlY
http://possiblywrong.com/presentations/ylj14/#39

## Comonads

https://bartoszmilewski.com/2017/01/02/comonads/
