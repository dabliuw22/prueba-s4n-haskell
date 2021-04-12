{-# LANGUAGE TemplateHaskell #-}

module Core.Fsl
  ( Cmd (..),
    Fsl,
    makeFsl,
    a,
    i,
    d,
  )
where

import Control.Lens (makePrisms)
import Control.Monad.Free (Free (..))

data Cmd a
  = A a
  | I a
  | D a

makePrisms ''Cmd

instance Show a => Show (Cmd a) where
  show (A cmd) = "A(" <> show cmd <> ")"
  show (I cmd) = "I(" <> show cmd <> ")"
  show (D cmd) = "D(" <> show cmd <> ")"

instance Functor Cmd where
  fmap f (A a) = A (f a)
  fmap f (I a) = I (f a)
  fmap f (D a) = D (f a)

type Fsl a = Free Cmd a

end :: Fsl ()
end = Pure ()

a :: Fsl () -> Fsl ()
a next = Free (A next)

i :: Fsl () -> Fsl ()
i next = Free (I next)

d :: Fsl () -> Fsl ()
d next = Free (D next)

makeFsl :: String -> Either String (Fsl ())
makeFsl [] = Right (Pure ())
makeFsl cmd =
  makeFslR (reverse cmd) end
  where
    reverse :: [Char] -> String
    reverse xs = foldl (flip (:)) [] xs
    makeFslR :: String -> Fsl () -> Either String (Fsl ())
    makeFslR [] next = Right next
    makeFslR (h : t) next =
      case h of
        'A' -> makeFslR t (a next)
        'I' -> makeFslR t (i next)
        'D' -> makeFslR t (d next)
        _ -> Left "Invalid Char Input"
