\documentclass{article}
\newtheorem{example}{Example}
\usepackage{semantic}
%include polycode.fmt
%format Mu = "\mu"
\begin{document}

%if False

>{-# LANGUAGE FlexibleInstances, GADTs, ExistentialQuantification, EmptyDataDecls #-}

%endif

\section{Defining a Language with Recursive Types}

We must first define the types supported in the language. B represents the basic types (unit, int, bool, etc.), we have function types, sum and product types. The new types here are $\alpha$, which is a type variable, and $\mu \alpha.\tau$, which is a recursive type. $\alpha$ is a type variable because we substitute in a type, like with the x in lambdas, where we substitute in a value for x.

$$\tau ::= B\ ||\ \tau_1\ ->\ \tau_2\ ||\ \alpha\ ||\ \mu \alpha. \tau\ ||\ \tau_1 * \tau_2\ ||\ \tau_1 + \tau_2$$

Defining a recursive type looks something like this, where the $T'$ is replaced with T in the same way that free variables are bound in lambda expressions. Note that T here defines a family of types of different lengths, not just a single type.:

\begin{math}
\begin{array}{lcr}
T &=& \mu \alpha. T' \\
  &=& T' \lbrack T/\alpha \rbrack \\
\end{array}
\end{math}

There is no polymorphism here (but we see that in the next lecture!), as there is no quantifier on the type variable $\alpha$. However, we can define infinite types (even though memory is finite) using this method. For example, we might define a list of ints as follows:

\begin{math}
\begin{array}{lcl}
IntList &=& \mu \alpha. () + (Int * \alpha) \\
 &=& () \\
 &=& () + Int * () \\
 &=& () + (Int * (Int * ())) \\
\end{array}
\end{math}

\subsection{An Inductive Definition of a Recursive Type for Lists}

While the above can work, we would prefer to define things in an inductive way. We introduce the fold and unfold expressions to handle this. We need both typing rules and operational semantics for these.

$$e ::= ...\ ||\ fold_{\mu}\ e\ ||\ unfold_{\mu}\ e$$

\begin{equation}
\inference{U = \mu \alpha . \tau  \hspace{0.5cm}  G \vdash e : U}
{G \vdash unfold_\mu e : \tau[U/\alpha]}
\end{equation}

\begin{equation}
\inference{U = \mu \alpha . \tau  \hspace{0.5cm}  G \vdash e : \tau[U/\alpha]}
{G \vdash fold_\mu e : U}
\end{equation}

The definitions of fold and unfold mirror one another, with one to compose a list and the other to decompose.

\subsection{Defining an IntList}

\begin{math}
\begin{array}{lcl}
IntList &=& () + Int * IntList \\
\mu \alpha &=& () + Int * \alpha \\
\end{array}
\end{math}

In the previous lecture, we saw the first of the above definitions when we were trying to define what an IntList should look like. However, it requires a name for the type and hence lacks generality. Notice that the new definition is anonymous in the same way that lambdas are anonymous.

\subsection{What do fold and unfold \emph{do}?}

Is there an operational semantics for fold an unfold?
The language we are defining here has type erasure, meaning that the types can be removed from the program at compile-time with the correct operation of the program still meeting the same guarantees. (The language from the homework (A5) does not have type erasure.) Could anything go wrong without an operational semantics? Let's continue and see.

We first define the nil and cons constructors for our list:

\begin{math}
\begin{array}{lcl}
nil &=& fold_{IntList}\ L () : () + Int * IntList\\
&&\\
cons &::& Int -> IntList -> IntList\\
cons &=& \lambda x : Int\\
     &&  \lambda l : IntList\\
     &&      fold_{IntList}\ R(x, l)\\
\end{array}
\end{math}

The functions first build the thing in the branch (for cons: (x, l), for nil: ()), and then fold it up into an IntList. Next to try is a function performing a calculation over a list, which will have to decompose the list into each of its elements using unfold:

\begin{math}
\begin{array}{lcl}
length &::& IntList -> Int \\
length &=& \lambda x : Int \\
       &&    case\ unfold_{IntList}\ of \\
       &&        L () -> 0 \\
       &&        R (\_, l') -> 1 + length\ l' \\
\end{array}
\end{math}

As you can see, we fold for constructors and we unfold for pattern matching. This is very mechanical! They can be made implicit in the language, leaving the compiler to do the work.

A Haskell list may look like:
< l = [1, 2]
But under the hood, it has a different representation which looks more like our definition for the construction of that list:
< l = cons 1 (cons 2 nil)

Based on what we've seen, we can tell that the unfold of a fold should step to what's inside. This looks like: $unfold_\mu (fold_\mu\ e) --> e$

Unfortunately, Michael ran out of time here, so we now move on to the Haskell.

\section{Least Fixed Point in Haskell}
To introduce the least fixed point operator in Haskell, we use the following syntax

> newtype Mu f = Fold {unFold :: f (Mu f) }

This newtype alias actually gets removed at runtime (because there's only one constructor), leaving no overhead for maintaining the type.
We are building a new type $\mu$ that takes a type $f$ and applies it to a $\mu$, so $\mu$ is the least fixed point of $f$. The function $unFold$ gets the contents out of that new type. The types are given by:

< Fold :: f (Mu f) -> Mu f
< unFold :: Mu f -> f (Mu f)

\subsection{ Defining the Type of a \emph{List of Integers} in Haskell.}


We might be tempted to write the following \emph{wrong} definition:

< data IntList = Mu (Either () (Int, IntList))

We could also try this \emph{wrong} definition:

< data IL = INil | ICons Int IL

Both definitions are ill formed because they used the term inside the definition. Instead we can use the following definition:

> data IntListSpine a = INil | ICons Int a
> type IntList = Mu IntListSpine -- Taking the fixed point over these spines

There are two interpretations of this idea: The natural one is to start with the smallest set (i.e. with $\{ INil \}$) and build it up (using $ICons$ in all existing lists). The other way is to see $\mu$ as the least fixed point of $IntListSpine$. The least fixed point here is the smallest type that is closed under $ICons$. Does that least fixed point exist? Is it unique? The answer is yes and there's an amazing thing called a $\mu$-calculus, which is a calculus of least and greatest fixed-point data structures. There are also languages which use the greatest fixed-point, the largest type closed under $ICons$. In such a case, instead of lists, we would get streams of $int$s. As an exercise, one may show that streams are closed under $ICons$.

%if False
What's the difference between data, type and newtype? @@Add explanation here!
%endif

There is an abstraction of types which focuses on the number of type variable arguments to a type, known as a kind. Kinds can themselves be further abstracted to sorts, and it's turtles all the way down (or up) from there.

The kind of most types is $*$.

The kind of IntListSpine is $*\ =>\ *$.

The kind of |Mu| is $(*\ =>\ *)\ =>\ *$.

We've now got the types (and even kinds!) we need - let's define some constructors:

> nil :: IntList
> nil = Fold INil

> cons :: Int -> IntList -> IntList
> cons x ls = Fold $ ICons x ls

> match :: IntList -> (() -> a) -> (Int -> IntList -> a) -> a
> match ls emptyCase consCase = 
>   case unFold ls of
>     INil -> emptyCase ()
>     ICons hd tl -> consCase hd tl
    
> instance Show IntList where    
>   show ls = match ls 
>               (\() -> "nil") 
>               (\x ls' -> "cons " ++ show x ++ " (" ++ show ls' ++ ")")
    
> hd :: IntList -> Int
> hd ls = match ls (\() -> error "don't do that!") (\x _ -> x)

> tl :: IntList -> IntList
> tl ls = match ls (\() -> error "oh no!") (\_ ls' -> ls')

> len :: IntList -> Int
> len ls = match ls (\() -> 0) (\_ ls' -> 1 + len ls')

> append :: IntList -> IntList -> IntList
> append (Fold INil) ls2 = ls2
> append (Fold (ICons i1 ls1)) ls2 = append ls1 $ cons i1 ls2


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

What makes this a shallow embedding is Haskell's type arrow above. If we consider $Dyn$ algebraically, we can say that
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

We do the same for function application and lambda expressions.

> app :: Dyn -> Dyn -> Dyn
> app v1 v2 = asFun v1 v2

> lam :: (Dyn -> Dyn) -> Dyn
> lam = f

This is the essence of shallow embeddings. We use all of Haskell's built in functionality, we just add some coercions.

\section{Writing programs}

We can now write some programs in our dynamic language. So far we haven't defined anything recursive, so let's begin with the trivially looping program, known as the $\Omega$ combinator.

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


\section{Generalized Algebraic Data Type}

The type systems we have been working with, can be though of as algebraic structures. There's a unit ($0$), a plus operator ($+$), a times operator ($*$), an arrow ($->$) and a fixed point operator ($\mu$), which is what makes an algebra of type. GADTs allow us to define  dynamic types such as lists that know their own length in their type.

> data Z
> data S x

$Nil$ is a list of length zero for any type a and $Cons$ takes whatever the list holds, a list of length $n$ and gives you a list of length greater than $n$
 
> data List n b where 
>   Nil  :: List Z a 
>   Cons :: a -> List n a -> List (S n) a

> instance Show b => Show (List n b) where
>     show Nil = "Nil"
>     show (Cons x xs) = "(Cons " ++ show x ++ " " ++ show xs ++ ")"

> sHead :: List (S n) b -> b
> sHead (Cons x xs) = x

< hd :: List (S n) a -> a$
< hd (Cons x _) = x$

> sTail :: List (S n) b -> List n b
> sTail (Cons x xs) = xs


Bounded natural numbers. I.e. a number m that is less than some other number n

> data LT n where
>    Z :: LT (S n)
>    S :: LT n -> LT (S n)

> coerce :: LT n -> LT (S n)
> coerce Z = Z
> coerce (S n) = S (coerce n)

> toInt :: LT n -> Int
> toInt Z = 0
> toInt (S n) = 1 + toInt n

> instance Show (LT n) where
>   show n = show (toInt n)

only lookup at a position less than the length of the list

> cLookup :: LT n -> List m a -> Maybe a
> cLookup Z (Cons x xs) = Just x
> cLookup (S n) (Cons x xs) = cLookup n xs
> cLookup _ Nil = Nothing

> cLookup' :: LT n -> List m a -> Maybe (LT m)
> cLookup' Z (Cons x xs)     = Just Z
> cLookup' (S n) (Cons x xs) = do n' <- (cLookup' n xs)
>                                 return (S n')
> cLookup' _ Nil = Nothing
 
> x :: List (S (S Z)) Int
> x = (Cons 1 (Cons 2 Nil)) 

> y :: LT (S (S (S Z)))
> y = (S (S Z))

> sLookup :: LT n -> List n a -> a
> sLookup Z     (Cons x xs) = x
> sLookup (S n) (Cons x xs) = sLookup n xs



\end{document}
