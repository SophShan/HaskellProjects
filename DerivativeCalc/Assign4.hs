{-|
Module      : 1JC3-Assign4.Assign_4.hs
Copyright   :  (c) Curtis D'Alves 2021
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 4 - McMaster CS 1JC3 2021
-}

{- --------------------------------------------------------------------
 - Datatype: MathExpr
 - --------------------------------------------------------------------
 - Description: An Abstract Syntax Tree (AST) for encoding mathematical
 -              expressions
 - Example: The expression
 -                (abs (2*X + 1)) ^ 3
 -          can be encoded as
 -                Power 3 (Func1 Abs
 -                              (Func2 Add (Func2 Mult (Coef 2) X)
 -                                         (Coef 1)))
 - --------------------------------------------------------------------
 -}
data MathExpr a =
    X
  | Coef a
  | Add (MathExpr a) (MathExpr a)
  | Mult (MathExpr a) (MathExpr a)
  | Power (MathExpr a) Int
  | Cos (MathExpr a)
  | Sin (MathExpr a)
  | Abs (MathExpr a)
  deriving (Eq,Show,Read)

{- -----------------------------------------------------------------
 - eval
 - -----------------------------------------------------------------
 - Description:
 -    Takes in as input an expression of type MathExpr and a float or int type (if int type is imputed, it will be used as a float). Eval then evaluates 
 the MathExpr at the given float and outputs the value. If solely X is entered, the inputted float is outputted. If a Coef is inputted, the value of Coef 
 is outputted. These are the two base cases on which the remaining pattern matching cases (which utilize recursion), rely on. The output of this function 
 is a float.
 -}
eval :: (Floating a, Eq a) => MathExpr a -> a -> a
eval X v = v
eval (Coef x) _ = x
eval (Add x y) v = eval x v + eval y v
eval (Mult x y) v = eval x v * eval y v
eval (Power x y) v = (eval x v) ^^ y
eval (Cos x) v = cos (eval x v)
eval (Sin x) v = sin (eval x v)
eval (Abs x) v = abs (eval x v)

{- -----------------------------------------------------------------
 - instance Num a => Num (MathExpr a)
 - -----------------------------------------------------------------
 - Description:
 -    These implementations for the below methods allow one to use the methods on the custom data type MathExpr by defining each method in terms of 
 MathExpr. This allows inputs of Num (MathExpr a) to be used with the methods.  
 -}
instance Num a => Num (MathExpr a) where
  x + y         = Add x y
  x * y         = Mult x y
  negate x      = Mult (Coef (-1)) x
  abs x         = Abs x
  fromInteger i = Coef (fromInteger i)
  signum _      = error "signum is left un-implemented"

{- -----------------------------------------------------------------
 - instance Fractional a => Fractional (MathExpr a)
 - -----------------------------------------------------------------
 - Description: 
-      These implementations for the below methods allow one to use the methods on the custom data type MathExpr by redefining each method in terms of 
MathExpr. These instances allow inputs of Fractional (MathExpr a) to be used with the below methods.
 -    
 -}
instance Fractional a => Fractional (MathExpr a) where
  recip e        = Power e (-1)
  fromRational e = Coef (fromRational e)

{- -----------------------------------------------------------------
 - instance Floating a => Floating (MathExpr a)
 - -----------------------------------------------------------------
 - Description:
 -    This defines instances of the below methods in terms of the custom data type MathExpr a and allows input of Floating (MathExpr a) to be used with the
 methods.
 -}
instance Floating a => Floating (MathExpr a) where
  pi      = Coef pi
  sin x   = Sin x
  cos x   = Cos x
  log     = error "log is left un-implemented"
  asin _  = error "asin is left un-implemented"
  acos _  = error "acos is left un-implemented"
  atan _  = error "atan is left un-implemented"
  sinh _  = error "sinh is left un-implemented"
  cosh _  = error "cosh is left un-implemented"
  tanh _  = error "tanh is left un-implemented"
  asinh _ = error "asinh is left un-implemented"
  acosh _ = error "acosh is left un-implemented"
  atanh _ = error "atanh is left un-implemented"
  exp _   = error "exp is left un-implemented"
  sqrt _  = error "sqrt is left un-implemented"

{- -----------------------------------------------------------------
 - diff
 - -----------------------------------------------------------------
 - Description:
 -    This function takes in an input of MathExpr a and preforms pattern matching on the constructors to differentiate the MathExpr accodring to 
 differentiation rules. The differentiated MathExpr is then outputted as type MathExpr.
 -}
diff :: (Floating a, Eq a) => MathExpr a -> MathExpr a
diff X = Coef 1
diff (Coef x) = Coef 0
diff (Add x y) = Add (diff x) (diff y)
diff (Mult x y) = Add (Mult (diff x) y) (Mult x (diff y))
diff (Power x y) = Mult (Mult (diff x) (Coef (fromIntegral y))) (Power x (fromIntegral y-1))
diff (Cos x) = Mult (diff x)  (Mult (Coef (-1)) (Sin x))
diff (Sin x) = Mult (Cos x) (diff x)
diff (Abs x) = Mult (Mult x (recip (abs x))) (diff x)
  
{- -----------------------------------------------------------------
 - pretty
 - -----------------------------------------------------------------
 - Description: This function takes in a type MathExpr and pattern matches it according to its constructor to output the String representation of the 
 MathExpr.

 -}
-- NOTE: you'll have to test pretty yourself
pretty :: (Show a) => MathExpr a -> String
pretty X = "X"
pretty (Coef x) = "(" ++ show x ++ ")"
pretty (Add x y) = pretty x ++ " + " ++ pretty y
pretty (Mult x y) =  pretty x ++ " * " ++ pretty y
pretty (Power x y) = pretty x ++ " ^^ " ++ pretty (Coef y)
pretty (Cos x) = "cos(" ++ pretty x ++ ")"
pretty (Sin x) = "sin(" ++ pretty x ++ ")"
pretty (Abs x) = "abs(" ++ pretty x ++ ")"


{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -}
infix 4 =~
(=~) :: (Floating a,Ord a) => a -> a -> Bool
x =~ y = abs (x - y) <= 1e-4

{- EXAMPLE
 - Function: eval
 - Property: eval (Func2 Add (Coef x) X) y is correct for all x,y
 - Actual Test Result: Pass
 -}

evalProp0 :: (Float,Float) -> Bool
evalProp0 (x,y) = (x + y) =~ eval (Add (Coef x) X) y

runEvalProp0 :: IO ()
runEvalProp0 = quickCheck  evalProp0
