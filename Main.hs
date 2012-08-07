module Main where


import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs )
import Options 
import qualified Normal

{-
import LexSyntax
import ParSyntax
import SkelSyntax
import PrintSyntax
-}

import RawSyntax
import AbsSynToTerm
import TypeCheckerNF
import Basics
import Terms
import Display
import Text.PrettyPrint.HughesPJ (render)
import qualified Data.Sequence as S


import Language.LBNF.Runtime (ParseMonad(..))

-- type ParseFun a = [Token] -> Err a

myLLexer = tokens -- myLexer

type Verbosity = Int

putStrV :: Verbosity -> Doc -> IO ()
putStrV v s = if verb options >= v then putStrLn (render s) else return ()

runFile f = do
  putStrV 1 $ "Processing file:" <+> text f
  contents <- readFile f
  run contents f

instance Pretty Token where
    pretty = text . show

run s fname = let ts = myLLexer s in case pExp ts of
   Bad err -> do 
     putStrLn "Parse Failed."
     putStrV 1 $ "Tokens:" <+> pretty ts
     putStrLn $ fname ++ ":" ++ err
     return False
   Ok tree -> do 
     process fname tree

process fname modul = do
  let resolved = runResolver $ resolveTerm modul
  -- putStrV 2 $ "\n[Abstract Syntax]\n\n" ++ show tree
  -- putStrV 2 $ "\n[Linearized tree]\n\n" ++ printTree tree
  putStrV 2 $ "[Resolved into]" $$ pretty resolved
  let (checked,info) = runChecker $ iType mempty resolved
  case info of
    [] -> return ()
    _ -> putStrV 0 $ vcat info -- display constraints, etc.
  case checked of
    Right (a,b,_) -> do 
       putStrV 0 $ "nf =" <+> pretty a
       putStrV 0 $ "ty =" <+> pretty b
{-
       case b of 
         (Pi i s t) -> do
           putStrV v $ "s ∈ ⟦S⟧ =" <+> prettyTerm (S.singleton i) (zerInRel s)
           putStrV v $ "T =" <+> prettyTerm (S.singleton i) t
         _ -> putStrV v "not a function!"
-}
       return True
    Left (e,err) -> do let Irr (line,col) = termPosition e 
                       putStrV 0 (text fname <> ":" <> pretty line <> ":" <> pretty (col - 1) <> ":" <+> err)
                       return False
      
{-
showTree tree
 = do
      putStrV 2 $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrV 2 $ "\n[Linearized tree]\n\n" ++ printTree tree
-}

main :: IO ()
main = do 
  results <- mapM runFile (files options)
  let oks = filter id results
  putStrV 0 $ pretty (length oks) <> "/" <> pretty (length results) <+> "files typecheck."





