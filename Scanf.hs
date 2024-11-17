{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Data.Kind (Type)
import Distribution.Simple (KnownExtension(ScopedTypeVariables))
-- Support Types
data Proxy a = Proxy
data (a :: k1) <<< (b :: k2)
infixr 5 <<<

-- Scannable class to read tuples
class Scannable a where
   type Scanf a :: Type
   scan :: Proxy a -> IO (Scanf a)

instance Scannable () where
   type Scanf () = ()
   scan _ = return ()

instance (Read a, Scannable b) => Scannable ((a :: Type) <<< b) where
   type Scanf (a <<< b) = (a, Scanf b)
   scan _ = do
      value <- readLn
      rest <- scan (Proxy @b)
      return (value, rest)

example :: IO ()
example = do
    let proxy = Proxy @(Int <<< String <<< ())
    tuple <- scan proxy
    print tuple