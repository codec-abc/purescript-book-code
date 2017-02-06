module ExercisesChapter2
    (
        main
    ) where

import Prelude (Unit, bind, show, ($), (*), (==))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.ReadLine (READLINE)
import Control.Monad.Eff.Console as Console
import Global as G
import Math as M
import Node.ReadLine as Rl


main :: forall t4.                  
  Eff                       
    ( "readline" :: READLINE
    , "console" :: Console.CONSOLE  
    , "err" :: EXCEPTION    
    | t4                    
    )                       
    Unit
main = do
  printPromp
  interface <- Rl.createConsoleInterface Rl.noCompletion
  let printer = \line ->
    if line == "exit" 
        then do
          Console.log "Exiting..."
          Rl.close interface
        else do
          let number = G.readFloat line
          let perimeter = circleArea number
          Console.log $ show perimeter
          printPromp
  Rl.setLineHandler interface printer
  Rl.prompt interface

circleArea :: Number -> Number
circleArea x = M.pi * x

printPromp :: forall t1. Eff ( "console" :: Console.CONSOLE | t1 )  Unit
printPromp = Console.log "Input a radius to get perimeter or type exit to quit"