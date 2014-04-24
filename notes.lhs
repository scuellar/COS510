%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%              Ignor this preamble               %
%                                                %                    
\documentclass{article}
\newtheorem{example}{Example}
\newcommand{\fold}{\text{fold}}
\newcommand{\unfold}{\text{unfold}}
\usepackage{semantic, multicol}
%include polycode.fmt
%format Mu = "\mu"
%Command for the title
\newcommand{\BigBox}[5]{\noindent \begin{center} \hspace*{-0.25in}\framebox[5.5in]{ \vbox{ \hbox to 5.25in { \textbf{#1} \hfill } \vspace{1mm} \hbox to 5.25in { #2 \hfill } \vspace{4mm} \hbox to 5.25in { {\Large \hfill #3  \hfill} } \vspace{2mm}  \hbox to 5.25in { {\it #4 \hfill #5} } } } \end{center}\vspace*{4mm}}
\newcommand{\lecture}[5]{\BigBox{#1}{Lecturer: #2}{Lecture: #3}{#4}{Scribes: #5}}

%if False

>{-# LANGUAGE FlexibleInstances, GADTs, ExistentialQuantification, EmptyDataDecls #-}

%endif
\begin{document}
%                                                %
%              End of preamble                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\lecture{COS 510: Programming Languages}{Michael Greenberg}{20}{21 of April, 2014}{Santiago Cuellar, Jonathan Balkind}

\section{Compiling this document}

This document has been typeset directly from a literate haskell file. The $.lhs$ file can be found online %Yes? 
 and can be compiled to follow along.

\section{Defining a Language with Recursive Types}

So far we have only seen the simply typed lambda calculus $\lambda^{->}$, sometimes with some extra constructors. Such type systems are useful, but not expressive enough to have recursion (and thus are not Turing complete). Today we will define a stronger type system by including recursive types. We begin by defining the types supported in the language.

$$\tau ::= B\ ||\ \tau_1\ ->\ \tau_2\ ||\ \alpha\ ||\ \mu \alpha. \tau\ ||\ \tau_1 * \tau_2\ ||\ \tau_1 + \tau_2$$

We include some basic type $B$ such as unit, int, bool, etc. We also include function types, sum and product types. Further, we include new types $\alpha$, which is a type variable, and $\mu \alpha.\tau$, which is a recursive type. $\alpha$ is a type variable because we may substitute in a type, like with the $x$ variables in lambdas, where we substitute in a value for $x$.

Defining a recursive type looks something like this
$$
\begin{array}{lcr}
T &=& \mu \alpha . T' \\
  &=& T' \lbrack T/\alpha \rbrack \\
\end{array}
$$
where the $\alpha$ in $T'$ is replaced with $T$ in the same way that free variables are bound in lambda expressions. If $ \mu \alpha . T'$ is seen as a function, that equation is exactly what means to be a fixed point, the function returns $T$ when applied to $T$. Note that $T$ here defines a family of types of different lengths, namely all fixed points of the function $ \mu \alpha . T'$. We will focus on the smallest one.

There is no polymorphism here (but there will be in the next lecture!), as there is no quantifier on the type variable $\alpha$. However, we can define infinite types (even though memory is finite) using this method. For example, we might define a list of ints as follows:

$$
\begin{array}{lcl}
IntList &=& \mu \alpha. () + (Int * \alpha) \\
 &=& () \\
 &=& () + Int * () \\
 &=& () + (Int * (Int * ())) \\
\end{array}
$$

\subsection{An Inductive Definition of a Recursive Type for Lists}

While the above can work, we would prefer to define things in an inductive way. We introduce the fold and unfold expressions to handle this. We add those constructors to the toolkit of expressions we have been handling and we introduce typing rules and operational semantics for these.

$$e ::= ...\ ||\ \fold_{\mu}\ e\ ||\ \unfold_{\mu}\ e$$

\begin{multicols}{2}
$$
\inference{U = \mu \alpha . \tau  \hspace{0.5cm}  G \vdash e : U}
{G \vdash \unfold_\mu e : \tau[U/\alpha]}
$$

$$
\inference{U = \mu \alpha . \tau  \hspace{0.5cm}  G \vdash e : \tau[U/\alpha]}
{G \vdash \fold_\mu e : U}
$$
\end{multicols}

The definitions of fold and unfold mirror one another, with one to compose a list and the other to decompose.

\subsection{Defining an IntList}

$$
\begin{array}{lcl}
IntList &=& () + Int * IntList \\
\mu \alpha &=& () + Int * \alpha \\
\end{array}
$$

In the previous lecture, we saw the first of the above definitions when we were trying to define what an IntList should look like. However, it requires a name for the type and hence lacks generality. Notice that the new definition is anonymous in the same way that lambdas are anonymous.

\subsection{What do fold and unfold \emph{do}?}

Is there an operational semantics for fold an unfold?
The language we are defining here has type erasure, meaning that the types can be removed from the program at compile-time with the correct operation of the program still meeting the same guarantees. (The language from the homework (A5) does not have type erasure.) Could anything go wrong without an operational semantics? Let's continue and see.

We first define the nil and cons constructors for our list:

$$
\begin{array}{lcl}
nil &=& fold_{IntList}\ L () : () + Int * IntList\\
&&\\
cons &::& Int -> IntList -> IntList\\
cons &=& \lambda x : Int\\
     &&  \lambda l : IntList\\
     &&      fold_{IntList}\ R(x, l)\\
\end{array}
$$

The functions first build the thing in the branch (for cons: (x, l), for nil: ()), and then fold it up into an IntList. Now, let's try a function performing a calculation over a list, which will have to decompose the list into each of its elements using unfold:

$$
\begin{array}{lcl}
length &::& IntList -> Int \\
length &=& \lambda x : Int \\
       &&    case\ unfold_{IntList}\ of \\
       &&        L () -> 0 \\
       &&        R (\_, l') -> 1 + length\ l' \\
\end{array}
$$

As you can see, we fold for constructors and we unfold for pattern matching. This is very mechanical! They can be made implicit in the language, leaving the compiler to do the work.

A Haskell list may look like:
< l = [1, 2]
But under the hood, it has a different representation which looks more like our definition for the construction of that list:
< l = cons 1 (cons 2 nil)

Based on what we've seen, we can tell that the unfold of a fold should step to what's inside. In fact, it looks like: $\unfold_\mu (\fold_\mu\ e) --> e$

Unfortunately, Michael ran out of time here, so we now move on to the Haskell.

\section{Least Fixed Point in Haskell}
To introduce the least fixed point operator in Haskell, we use the following syntax

> newtype Mu f = Fold {unFold :: f (Mu f) }

This newtype alias actually gets removed at runtime (because there's only one constructor), leaving no overhead for maintaining the type. We are building a new type $\mu$ that takes a type $f$ and applies it to a $\mu$, so $\mu$ is a fixed point of $f$. The function $unFold$ gets the contents out of that new type. The types are given by:

< Fold :: f (Mu f) -> Mu f
< unFold :: Mu f -> f (Mu f)

\subsection{ Defining the Type of a \emph{List of Integers} in Haskell.}


When giving types to a list of integers, we might be tempted to write the following \emph{wrong} definition:

< data IntList = Mu (Either () (Int, IntList))

We could also try this other \emph{wrong} definition:

< data IL = INil | ICons Int IL

Both definitions are ill formed because they used the defined term inside the definition. Instead we can use the following definition:

> data IntListSpine a = IntNil | IntCons Int a
> type IntList = Mu IntListSpine -- Taking the fixed point over the spine

There are two interpretations of this idea: The natural one is to start with the smallest set (i.e. with $\{ IntNil \}$) and build it up (using $IntCons$ in all existing lists). The other way is to see $\mu$ as the least fixed point of $IntListSpine$. The least fixed point here is the smallest type that is closed under $IntCons$. Does that least fixed point exist? Is it unique? The answer is yes and there's an amazing thing called a $\mu$-calculus, which is a calculus of least and greatest fixed-point data structures. There are also languages which use the greatest fixed-point, in this case the largest type closed under $IntCons$. In such language, instead of lists, we would get streams of $int$s from the definition above. As an exercise, one may show that streams are closed under $IntCons$.

%if False
What's the difference between data, type and newtype? @@Add explanation here!
%endif

There is an abstraction of types which focuses on the number of type variable arguments to a type, known as a kind. Kinds can themselves be further abstracted to sorts, and it's turtles all the way down (or up) from there.
\begin{itemize}
\item The kind of most types is $*$.

\item The kind of IntListSpine is $*\ =>\ *$.

\item The kind of |Mu| is $(*\ =>\ *)\ =>\ *$.
\end{itemize}
We've now got the types (and even kinds!) we need - let's define some constructors for the lists:

> nil :: IntList
> nil = Fold IntNil

> cons :: Int -> IntList -> IntList
> cons x ls = Fold $ IntCons x ls

Next we can add some functionality to our lists. To use the integers inside the list, we need to unpack the list. The following handy functions will do the job of unfolding lists properly

> match :: IntList -> (() -> a) -> (Int -> IntList -> a) -> a
> match ls emptyCase consCase = 
>   case unFold ls of
>     IntNil -> emptyCase ()
>     IntCons hd tl -> consCase hd tl

With $match$ writing functions for lists is easy

> hd :: IntList -> Int
> hd ls = match ls (\() -> error "don't do that!") (\x _ -> x)

> tl :: IntList -> IntList
> tl ls = match ls (\() -> error "oh no!") (\_ ls' -> ls')

> len :: IntList -> Int
> len ls = match ls (\() -> 0) (\_ ls' -> 1 + len ls')

> append :: IntList -> IntList -> IntList
> append (Fold IntNil) ls2 = ls2
> append (Fold (IntCons i1 ls1)) ls2 = append ls1 $ cons i1 ls2

> instance Show IntList where    
>   show ls = match ls 
>               (\() -> "nil") 
>               (\x ls' -> "cons " ++ show x ++ " (" ++ show ls' ++ ")")

\subsection{ Defining the Type of \emph{Generic Lists} in Haskell.}

Now we would like to define the type of generic lists, which adds a type parameter that wasn't present before. Again, the naive answer doesn't work:
< data List a = Nil | Cons a (List a)

We want to define a list spine dependent on the type of the list:

> data GenericListSpine a x = GenericNil | GenericCons a x
> type GenericList a = Mu (GenericListSpine a)

Based on the approach we've used so far, we could build more complex data structures too.

\begin{example} For fun, let's define trees!
\end{example}
The naive answer looks like:
< data Tree a = Leaf a | Node a (Tree a) (Tree a)

As you can probably guess, we need a TreeSpine and then an application of Mu to that spine:

> data TreeSpine a x = Leaf a | Node a x x
> type Tree a = Mu (TreeSpine a)

The definitions get easier the more you write because it's very mechanical. The spine is the function that defines the structure of the type and the least fixed point is the type with that structure. This is what a compiler does automatically for fixed points, where you only provide the structure.

\section{Shallow Embedding of a Dynamic Language}

In the homework we talked about embedding dynamic types in static types and we wrote the interpreter in the typical way, with the syntax tree etc. This is called a deep embedding, because we built a whole model of the embedded language in the host programming language. There are also shallow embeddings, where the embedded types are the types from your language. Shallow embeddings are nice but it's much harder to reason about.

\noindent We're going to do a shallow embedding of dynamic types

> data DynSpine a = B Bool | I Int | F (a -> a)
> type Dyn = Mu DynSpine

What makes this a shallow embedding is Haskell's type arrow above. We are using Haskell's funcitonality and lifting it to our language (e.g. with the constructor $F$). If we consider $Dyn$ algebraically, we can say that
$$
Dyn = Bool + Int + (Dyn -> Dyn)
$$

In the homework there are \emph{tag} and \emph{as} terms and we need to write those in Haskell.
\begin{enumerate}
\item Tags:

> b :: Bool -> Dyn
> b = Fold . B -- Which means |b x = Fold $ B x|

> i :: Int -> Dyn
> i = Fold . I

> f :: (Dyn -> Dyn) -> Dyn
> f = Fold . F

\item Untags:

The naive approach would be to try
< asBool :: Dyn -> Bool
< asBool v = unFold v
But that's not going to be of the right type, so we need to unpack the |DynSpine|.

> asBool :: Dyn -> Bool
> asBool v = 
>    case unFold v of
>        B b -> b
>        _ -> error "not a bool"

> asInt :: Dyn -> Int
> asInt v = 
>    case unFold v of
>        I i -> i
>        _ -> error "not an int"
 
> asFun :: Dyn -> Dyn -> Dyn 
> asFun v = 
>    case unFold v of
>        F f -> f
>        _ -> error "not an function"

\end{enumerate}

\subsection{Conditionals, Application and Operations on the Embedded Language}

Conditionals can be written easilty using Haskell's conditional syntax

> cond :: Dyn -> Dyn -> Dyn -> Dyn
> cond i t e = if asBool i then t else e

To define aritmetic operations, we define a handy lift operator that allows us to use Haskell's operations

> liftBin :: (a -> a -> b) -> (Dyn -> a) -> (b -> Dyn) -> Dyn -> Dyn -> Dyn
> liftBin op untag tag v1 v2 = tag $ op (untag v1) (untag v2)

Now we can use |liftBin| to embed the arithmetic expressions of our dynamic lenguage into Haskell. 

> plus :: Dyn -> Dyn -> Dyn
> plus = liftBin (+) asInt i -- Here |i| is the tag function

> minus :: Dyn -> Dyn -> Dyn
> minus = liftBin (-) asInt i

> times :: Dyn -> Dyn -> Dyn
> times = liftBin (*) asInt i

> eq :: Dyn -> Dyn -> Dyn
> eq = liftBin (==) asInt b

We do the same for function application and lambda expressions

> app :: Dyn -> Dyn -> Dyn
> app v1 v2 = asFun v1 v2

> lam :: (Dyn -> Dyn) -> Dyn
> lam = f

This is the essence of shallow embeddings. We use all of Haskell's built in functionality, we just add some coercions.

\section{Writing Programs in the Dynamic Language}

We can now write some programs in our dynamic language. So far we haven't defined anything recursive, so let's begin with the trivially looping program, known as the $\Omega$ combinator

> omega = app (lam (\x -> app x x)) (lam (\x -> app x x))

Now let's define the useful $Y$ combinator (or fixed-point combinator).

> fix = lam (\f -> app  (lam (\x -> app f (app x x)))
>                       (lam (\x -> app f (app x x))))

Using the Y combinator we can define the program that computes the factorial of a number:

> factorial = app fix $ lam (\fact ->
>                             lam (\n ->
>                                   cond (n `eq` (i 0)) 
>                                   (i 1) 
>                                   (n `times` app fact (n `minus` (i 1)))))

We can also define the program that computes the fibonacci sequence of a number:

> fib = app fix $ lam (\fib -> 
>                       lam (\n -> 
>                               cond (n `eq` (i 0))
>                               (i 1) 
>                               (cond (n `eq` (i 1)) (i 1) 
>                               ((app fib (n `minus` (i 2))) 
>                               `plus` 
>                               (app fib (n `minus` (i 1)))))))

Want to try out these functions? Load the Literate Haskell script into GHCI and try running something like:

< asInt (app fib (i 4))
< asInt (app factorial (i 6))

\section{Generalized Algebraic Data Type}

The type systems we have been working with, can be thought of as algebraic structures. There's a unit ($0$), a plus operator ($+$), a times operator ($*$), an arrow ($->$) and a fixed point operator ($\mu$), which is what makes an algebra of types. Generalized Algebraic Data Type (GADTs) are type-indexed type systems, that means type systems where types depend on other types. GADTs are not as general as dependent types, where types can depend on values, but with an embedded language we can simulate values at the type-level and aproximate dependent types. GADTs allow us to define  dynamic types such as length-indexed lists or bounded natural numbers which we construct bellow.

We shall build lists that know their own length in their type. To do so, we first need a type-level representation of the length, so we start by defining the types for each natural number

%if False

%> module NList where

%endif

> data Z
> data S x

Using this type-level definition of the naturals, we can construct length-indexed lists.

> data IList n b where 
>   INil  :: IList Z a 
>   ICons :: a -> IList n a -> IList (S n) a

%if False

> instance Show b => Show (IList n b) where
>     show INil = "Nil"
>     show (ICons x xs) = "(Cons " ++ show x ++ " " ++ show xs ++ ")"

%endif

\noindent $Nil$ is a list of length zero for any type $a$ and $Cons$ takes an element of type $a$, a list of length $n$ and gives you a list of length greater than $n+1$.

Lets write functions head and tail for our lists. Until now, when writing such functions, we had to check for empty lists and throw an exception in such a case. We can avoid using exceptions with length-indexed lists


> sHead :: IList (S n) b -> b
> sHead (ICons x xs) = x

> sTail :: IList (S n) b -> IList n b
> sTail (ICons x xs) = xs

Another interesting data type is a bounded natural number, a number $m$ that is less than some other number $n$ determined by its type

> data LT n where
>    Z :: LT (S n)
>    S :: LT n -> LT (S n)

%if False

> instance Show (LT n) where
>   show n = show (toInt n)

%endif

If $m < n$ then it should be true that $m < n + 1$. The following function implements that relation. 

> coerce :: LT n -> LT (S n)
> coerce Z = Z
> coerce (S n) = S (coerce n)

Notice that in the case $Z$, the function returns the same $Z$ \emph{but changes the type}. We can also convert any $LT\ n$ to an int (namely it's value $m$)

> toInt :: LT n -> Int
> toInt Z = 0
> toInt (S n) = 1 + toInt n

Let's now define a lookup function for our length-indexed lists using our bounded naturals. We could use the standard look up, where we have to use the Maybe monad for safety

> cLookup :: LT n -> IList m a -> Maybe a
> cLookup Z (ICons x xs) = Just x
> cLookup (S n) (ICons x xs) = cLookup n xs
> cLookup _ INil = Nothing

But since we are using length-indexed lists and bounded naturals, there is a better way to implement safe lookup. We will only lookup at a position if its type indicates that it's less than the length of the list.

> sLookup :: LT n -> IList n a -> a
> sLookup Z     (ICons x xs) = x
> sLookup (S n) (ICons x xs) = sLookup n xs


% I don't know what this following code does, we could remove it.
%if False

> cLookup' :: LT n -> IList m a -> Maybe (LT m)
> cLookup' Z (ICons x xs)     = Just Z
> cLookup' (S n) (ICons x xs) = do n' <- (cLookup' n xs)
>                                  return (S n')
> cLookup' _ INil = Nothing
 
> x :: IList (S (S Z)) Int
> x = (ICons 1 (ICons 2 INil))
 
> z :: IList (S (S (S Z))) Int
> z = (ICons 1 (ICons 2 (ICons 3 INil))) 

> y :: LT (S (S (S Z)))
> y = (S (S Z))

%endif


\end{document}
