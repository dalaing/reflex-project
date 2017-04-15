{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module Servant.Server.Reflex.Util (
    CollapseList(..)
  ) where

data Gather (xs :: [*]) where
  GNil :: Gather '[]
  GCons :: x -> Gather xs -> Gather (x ': xs)

class CollapseList (xs :: [*]) where
  type Collapsed xs

  collapse :: Gather xs -> Collapsed xs

instance CollapseList '[] where
  type Collapsed '[] = ()

  collapse GNil =
    ()

instance CollapseList '[a] where
  type Collapsed '[a] =
    a

  collapse (GCons a GNil) =
    a

instance CollapseList '[a, b] where
  type Collapsed '[a, b] =
    (a, b)

  collapse (GCons a  (GCons b GNil)) =
    (a, b)

instance CollapseList '[a, b, c] where
  type Collapsed '[a, b, c] =
    (a, b, c)

  collapse (GCons a  (GCons b (GCons c GNil))) =
    (a, b, c)

instance CollapseList '[a, b, c, d] where
  type Collapsed '[a, b, c, d] =
    (a, b, c, d)

  collapse (GCons a  (GCons b (GCons c (GCons d GNil)))) =
    (a, b, c, d)

instance CollapseList '[a, b, c, d, e] where
  type Collapsed '[a, b, c, d, e] =
    (a, b, c, d, e)

  collapse (GCons a  (GCons b (GCons c (GCons d (GCons e GNil))))) =
    (a, b, c, d, e)

instance CollapseList '[a, b, c, d, e, f] where
  type Collapsed '[a, b, c, d, e, f] =
    (a, b, c, d, e, f)

  collapse (GCons a  (GCons b (GCons c (GCons d (GCons e (GCons f GNil)))))) =
    (a, b, c, d, e, f)

instance CollapseList '[a, b, c, d, e, f, g] where
  type Collapsed '[a, b, c, d, e, f, g] =
    (a, b, c, d, e, f, g)

  collapse (GCons a  (GCons b (GCons c (GCons d (GCons e (GCons f (GCons g GNil))))))) =
    (a, b, c, d, e, f, g)

instance CollapseList '[a, b, c, d, e, f, g, h] where
  type Collapsed '[a, b, c, d, e, f, g, h] =
    (a, b, c, d, e, f, g, h)

  collapse (GCons a  (GCons b (GCons c (GCons d (GCons e (GCons f (GCons g (GCons h GNil)))))))) =
    (a, b, c, d, e, f, g, h)

instance CollapseList '[a, b, c, d, e, f, g, h, i] where
  type Collapsed '[a, b, c, d, e, f, g, h, i] =
    (a, b, c, d, e, f, g, h, i)

  collapse (GCons a  (GCons b (GCons c (GCons d (GCons e (GCons f (GCons g (GCons h (GCons i GNil))))))))) =
    (a, b, c, d, e, f, g, h, i)

instance CollapseList '[a, b, c, d, e, f, g, h, i, j] where
  type Collapsed '[a, b, c, d, e, f, g, h, i, j] =
    (a, b, c, d, e, f, g, h, i, j)

  collapse (GCons a  (GCons b (GCons c (GCons d (GCons e (GCons f (GCons g (GCons h (GCons i (GCons j GNil)))))))))) =
    (a, b, c, d, e, f, g, h, i, j)
