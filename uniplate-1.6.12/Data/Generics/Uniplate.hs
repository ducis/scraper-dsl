{- |
/DEPRECATED/ Use "Data.Generics.Uniplate.Operations" instead.

This is the main Uniplate module, which defines all the essential operations
in a Haskell 98 compatible manner.

Most functions have an example of a possible use for the function.
To illustate, I have used the @Expr@ type as below:

> data Expr = Val Int
>           | Neg Expr
>           | Add Expr Expr
-}


module Data.Generics.Uniplate
    {- DEPRECATED "Use Data.Generics.Uniplate.Operations instead" -}
    where

import Control.Monad
import Data.Generics.Uniplate.Internal.Utils


-- * The Class

-- | The type of replacing all the children of a node
--
--   Taking a value, the function should return all the immediate children
--   of the same type, and a function to replace them.
type UniplateType on = on -> ([on], [on] -> on)

-- | The standard Uniplate class, all operations require this
class Uniplate on where
    -- | The underlying method in the class
    --
    -- > uniplate (Add (Val 1) (Neg (Val 2))) = ([Val 1, Neg (Val 2)], \[a,b] -> Add a b)
    -- > uniplate (Val 1)                     = ([]                  , \[]    -> Val 1  )
    uniplate :: UniplateType on
    
-- * The Operations

-- ** Queries

-- | Get all the children of a node, including itself and all children.
--
-- > universe (Add (Val 1) (Neg (Val 2))) =
-- >     [Add (Val 1) (Neg (Val 2)), Val 1, Neg (Val 2), Val 2]
--
-- This method is often combined with a list comprehension, for example:
--
-- > vals x = [i | Val i <- universe x]
universe :: Uniplate on => on -> [on]
universe x = builder (f x)
    where
        f :: Uniplate on => on -> (on -> res -> res) -> res -> res
        f x cons nil = x `cons` concatCont (map (\x -> f x cons) $ children x) nil


-- | Get the direct children of a node. Usually using 'universe' is more appropriate.
--
-- @children = fst . 'uniplate'@
children :: Uniplate on => on -> [on]
children = fst . uniplate



-- ** Transformations


-- | Transform every element in the tree, in a bottom-up manner.
--
-- For example, replacing negative literals with literals:
--
-- > negLits = transform f
-- >    where f (Neg (Lit i)) = Lit (negate i)
-- >          f x = x
transform :: Uniplate on => (on -> on) -> on -> on
transform f x = f $ generate $ map (transform f) current
    where (current, generate) = uniplate x


-- | Monadic variant of 'transform'
transformM :: (Monad m, Uniplate on) => (on -> m on) -> on -> m on
transformM f x = mapM (transformM f) current >>= f . generate
    where (current, generate) = uniplate x


-- | Rewrite by applying a rule everywhere you can. Ensures that the rule cannot
-- be applied anywhere in the result:
--
-- > propRewrite r x = all (isNothing . r) (universe (rewrite r x))
--
-- Usually 'transform' is more appropriate, but 'rewrite' can give better
-- compositionality. Given two single transformations @f@ and @g@, you can
-- construct @f `mplus` g@ which performs both rewrites until a fixed point.
rewrite :: Uniplate on => (on -> Maybe on) -> on -> on
rewrite f = transform g
    where g x = maybe x (rewrite f) (f x)


-- | Monadic variant of 'rewrite'
rewriteM :: (Monad m, Uniplate on) => (on -> m (Maybe on)) -> on -> m on
rewriteM f = transformM g
    where g x = f x >>= maybe (return x) (rewriteM f)


-- | Perform a transformation on all the immediate children, then combine them back.
-- This operation allows additional information to be passed downwards, and can be
-- used to provide a top-down transformation.
descend :: Uniplate on => (on -> on) -> on -> on
descend f x = generate $ map f current
    where (current, generate) = uniplate x


-- | Monadic variant of 'descend'    
descendM :: (Monad m, Uniplate on) => (on -> m on) -> on -> m on
descendM f x = liftM generate $ mapM f current
    where (current, generate) = uniplate x



-- ** Others

-- | Return all the contexts and holes.
--
-- > propUniverse x = universe x == map fst (contexts x)
-- > propId x = all (== x) [b a | (a,b) <- contexts x]
contexts :: Uniplate on => on -> [(on, on -> on)]
contexts x = (x,id) : f (holes x)
  where
    f xs = [ (y, ctx . context)
           | (child, ctx) <- xs
           , (y, context) <- contexts child]


-- | The one depth version of 'contexts'
--
-- > propChildren x = children x == map fst (holes x)
-- > propId x = all (== x) [b a | (a,b) <- holes x]
holes :: Uniplate on => on -> [(on, on -> on)]
holes x = uncurry f (uniplate x)
  where f [] _ = []
        f (x:xs) gen = (x, gen . (:xs)) :
                       f xs (gen . (x:))


-- | Perform a fold-like computation on each value,
--   technically a paramorphism
para :: Uniplate on => (on -> [r] -> r) -> on -> r
para op x = op x $ map (para op) $ children x

