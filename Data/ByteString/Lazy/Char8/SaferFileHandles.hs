{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Data.ByteString.SaferFileHandles
-- Copyright   :  (c) 2010 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- This module lifts the bytestring IO operations into the region monad.
--
-------------------------------------------------------------------------------

module Data.ByteString.Lazy.Char8.SaferFileHandles
    ( hGetContents
    , hGet
    , hGetNonBlocking

    , hPut
    ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Data.Int ( Int )

-- from transformers:
import Control.Monad.IO.Class ( MonadIO )

-- from bytestring:
import qualified Data.ByteString.Lazy.Char8 as B

-- from regions:
import Control.Monad.Trans.Region ( AncestorRegion )

-- from explicit-iomodes-bytestring:
import qualified Data.ByteString.Lazy.Char8.ExplicitIOModes as E
    ( hGetContents, hGet, hGetNonBlocking
    , hPut
    )

-- from safer-file-handles:
import System.IO.SaferFileHandles ( FileHandle, ReadModes, WriteModes )
import System.IO.SaferFileHandles.Unsafe ( wrap, wrap2 )


-------------------------------------------------------------------------------
-- ByteString I/O with regional file handles
-------------------------------------------------------------------------------

-- | Wraps: @Data.ByteString.'B.hGetContents'@.
hGetContents ∷ ( FileHandle handle, ReadModes ioMode
               , pr `AncestorRegion` cr, MonadIO cr
               )
             ⇒ handle ioMode pr → cr B.ByteString
hGetContents = wrap E.hGetContents

-- | Wraps: @Data.ByteString.'B.hGet'@.
hGet ∷ ( FileHandle handle, ReadModes ioMode
       , pr `AncestorRegion` cr, MonadIO cr
       )
     ⇒ handle ioMode pr → Int → cr B.ByteString
hGet = wrap2 E.hGet

-- | Wraps: @Data.ByteString.'B.hGetNonBlocking'@.
hGetNonBlocking ∷ ( FileHandle handle, ReadModes ioMode
                  , pr `AncestorRegion` cr, MonadIO cr
                  )
                ⇒ handle ioMode pr → Int → cr B.ByteString
hGetNonBlocking = wrap2 E.hGetNonBlocking


-- | Wraps: @Data.ByteString.'B.hPut'@.
hPut ∷ ( FileHandle handle, WriteModes ioMode
       , pr `AncestorRegion` cr, MonadIO cr
       )
     ⇒ handle ioMode pr → B.ByteString → cr ()
hPut = wrap2 E.hPut


-- The End ---------------------------------------------------------------------
