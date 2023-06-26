module Uebung05_04 where


    {-
    ########################
    functional list applications
    ########################
    -}

    data Expr = Val Int
          | Expr :+: Expr
          | Expr :*: Expr

    infixl 6 :+:
    infixl 7 :*:

    instance Show Expr where
        showsPrec _ (Val n) = shows n
        showsPrec p (e1 :+: e2) = showParen (p > 6) (showsPrec 6 e1 . showString " + " . showsPrec 6 e2)
        showsPrec p (e1 :*: e2) = showParen (p > 7) (showsPrec 7 e1 . showString " * " . showsPrec 7 e2)
