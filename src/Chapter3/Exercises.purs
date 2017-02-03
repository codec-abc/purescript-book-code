module ExercisesChapter3
  (
    main
  ) where

import Prelude
import Control.Monad.Eff.Console as Console
import Data.List (List(..), any, filter, fromFoldable, head, nubBy)
import Data.Maybe (Maybe)
import Control.Monad.Eff (Eff)

type Address =
  { 
    street :: String,
    city   :: String,
    state  :: String
  }

type Entry =
  { 
    firstName :: String,
    lastName  :: String,
    address   :: Address
  }

type Name =
  {
    firstName :: String,
    lastName :: String
  }

type AddressBook = List Entry

main :: forall t. Eff ( "console" :: Console.CONSOLE | t ) Unit
main = do
  Console.log "hello world"

showEntry :: Entry -> String
showEntry entry = 
  entry.lastName <> ", " <>
  entry.firstName <> ": " <>
  showAddress entry.address

showAddress :: Address -> String
showAddress addr = 
  addr.street <> ", " <>
  addr.city <> ", " <>
  addr.state

emptyBook :: AddressBook
emptyBook = fromFoldable []

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry entry book = Cons entry book

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName book = head $ filter filterEntry book
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

findEntryByAddress :: AddressBook -> Address -> Maybe Entry
findEntryByAddress book address = head $ filter filterEntry book
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = 
    entry.address.street == address.street && 
    entry.address.city == address.city &&  
    entry.address.state == address.state

isNameInBook :: AddressBook -> Name -> Boolean
isNameInBook book name = any filter book
  where filter entry = entry.firstName == name.firstName && entry.lastName == name.lastName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates book = nubBy filter book
  where filter entry entry2 = entry.firstName == entry2.firstName && entry.lastName == entry2.lastName