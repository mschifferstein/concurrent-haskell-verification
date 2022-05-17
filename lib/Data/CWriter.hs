module Data.CWriter where

import Data.Writer

import Data.C

import Data.MonadTrans

instance (Writer m) => Writer (C m) where
    write s = lift (write s)

loop :: Writer m => String -> m ()
loop s = write s >> loop s

example :: Writer m => C m ()
example = write "start!" >> (fork (loop "fish") >> loop "cat")

