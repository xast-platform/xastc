module Xast.Utils.Generic where

-- | Inverse functor: applied monadic function to regular value 
-- (instead of regular function to monadic value)
(<--) :: Functor m => m (a -> b) -> a -> m b
f <-- a = fmap (\fm -> fm a) f

infixl 3 <--

(|>) :: a -> (a -> b) -> b
x |> f = f x

infixl 1 |>

unreachableWith :: String -> a
unreachableWith msg = error ("Entered unreachable state with: " ++ msg)

todo__ :: String -> a
todo__ msg = error ("TODO: " ++ msg)