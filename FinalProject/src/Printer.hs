{- | A Pretty Printer for MiniDafny |
   ==================================

The derived `Show` instances for the datatypes above are pretty hard to
read, especially when programs get long. Really, who wants to read this...

>>> wAbs
> Method "Abs" [("r",TInt)] [("absR",TInt)] [] (Block [If (Op2 (Var (Name "r")) Lt (Val (IntVal 0))) (Block [Assign (Name "absR") (Op1 Neg (Var (Name "r"))),Empty]) (Block [Assign (Name "absR") (Var (Name "r")),Empty])])

instead of this...

    ghci> putStrLn (pretty wAbs)
    method Abs (r : int) returns (absR : int)
    {
        if r < 0 {
            absR := -r;
        }
        else {
            absR := r; 
        }
    }

-}

module Printer where

import Syntax
import Data.List(intersperse)
import Text.PrettyPrint ( (<+>), Doc )
import qualified Text.PrettyPrint as PP

{- |

A *pretty printer* is a function that formats an abstract syntax tree into a
readable representation of the concrete syntax. 

The `pretty` library, imported above as `PP`, provides the following to assist
in the development of pretty printers:

   * An abstract type `Doc` of "pretty documents" that know how to lay
     themselves out prettily. We can use this type to define a class of of types
     that support pretty printing---those that define a function mapping any
     value of that type to a suitable `Doc`.
-} 

class PP a where
  pp :: a -> Doc

{- |

   * Operations for rendering, or converting a `Doc` to text at the
     top level.  The rendering functions are parameterized over display
     options, such as the maximum line length, so that they can figure out
     how to best display the text. 
-}

-- | Default operation for the pretty printer. Displays using standard formatting
-- rules, with generous use of indentation and newlines.
pretty :: PP a => a -> String
pretty = PP.render . pp

-- | Compact version. Displays its argument without newlines.
oneLine :: PP a => a -> String
oneLine = PP.renderStyle (PP.style {PP.mode=PP.OneLineMode}) . pp

{- |
   * Primitive documents and operations for constructing `Doc`s from primitive
     types, such as characters and string.

     For example, we can use the `text` function to define the `Uop` instance of the `PP`
     class.  This instance converts each unary operator into a document.
-}

instance PP Uop where
  pp Neg = PP.char '-'
  pp Not = PP.char '!'
  pp Len = PP.text ".Length"

{- |
   * Combinators for combining `Doc`s in various ways, providing constraints on
     the textual layout. For example, some are listed below. (See the library
     documentation for *many* more.)

          -- An empty document, with no width and no height.
          empty :: Doc

          -- Beside. Combines two documents horizontally with no space between.
          (<>) :: Doc -> Doc -> Doc

          -- Beside, separated by space, unless one of the arguments is `empty`.
          (<+>) :: Doc -> Doc -> Doc

          -- Nest (or indent) a document by a given number of positions
          -- (which may also be negative).
          nest :: Int -> Doc -> Doc

          -- Above. Combines two documents vertically (with overlap if
          -- possible: if the last line of the first argument stops at
          -- least one position before the first line of the second begins,
          -- these two lines are overlapped).
          ($$) :: Doc -> Doc -> Doc

          -- List version of $$.
          vcat :: [Doc] -> Doc

          -- wrap document in (..)
          parens :: Doc -> Doc

          -- wrap document in [..]
          brackets :: Doc -> Doc

          -- wrap document in {..}
          braces :: Doc -> Doc

-}

{- | Pretty-Pretter implementation |
   ---------------------------------

Your job will be to complete the following functionality, ensuring that the output
of the pretty printer is valid Dafny---that is, you can parse it in Visual Studio
if you load it as a .dfy.

-}

-- | We've given you the `PP` instances for String and Int to get started.
instance PP String where
  pp = PP.text

instance PP Int where
  pp = PP.int

-- | TODO: Implement pretty printing for Booleans and lists of integers
instance PP Bool where
  pp b = if b then pp "true" else pp "false"

instance PP [Int] where
  pp l = PP.brackets $ ppAux l where 
    ppAux [] = PP.empty
    ppAux (x:xs@[]) = PP.int x <> ppAux xs
    ppAux (x:xs) = PP.int x <> PP.text "," <> ppAux xs

-- | That should allow you to also pretty pring values if needed.
instance PP Value where
  pp (IntVal i)  = pp i
  pp (BoolVal b) = pp b
  pp (ArrayVal l)  = pp l

-- | TODO: Implement pretty printing for binary operators
instance PP Bop where
  pp Plus   = PP.char '+'
  pp Minus  = PP.char '-'
  pp Times  = PP.char '*'
  pp Divide = PP.char '/'
  pp Modulo = PP.char '%'
  pp Eq     = PP.text "=="
  pp Neq    = PP.text "!="
  pp Gt     = PP.char '>'
  pp Ge     = PP.text ">="
  pp Lt     = PP.char '<'
  pp Le     = PP.text "<="
  pp Conj   = PP.text "&&"
  pp Disj   = PP.text "||"
  pp Implies = PP.text "==>"
  pp Iff    = PP.text "<==>"

-- | Types and bindings can be pretty printed

instance PP Type where
  pp TInt  = PP.text "int"
  pp TBool = PP.text "bool"
  pp TArrayInt = PP.text "array<int>"

-- | TODO: Implement pretty printing for bindings
instance PP Binding where
  pp (x, t) = PP.text x <+> PP.text ":" <+> pp t 

{- |
   Expressions are trickier if you want to avoid putting parentheses everywhere.

   The following code uses two heuristics to ensure minimal parentheses
   are placed:

   * When printing single-operand operations, we don't wrap
     "base" expressions in parentheses.
   * When printing binary operations, we take the precedence level
     of the operator into account.

   We've implemented this logic for you, but there are is one thing
   missing: the array ".Length" operation is now not handled.

-} 

instance PP Expression where
  pp (Var v) = pp v
  pp (Val v) = pp v
  pp (Op1 Len v) = (if isBase v then pp v else PP.parens (pp v)) <> pp Len  
  pp (Op1 o v) = pp o <+> if isBase v then pp v else PP.parens (pp v)
  pp e@Op2{} = ppPrec 0 e  where
     ppPrec n (Op2 e1 bop e2) =
        ppParens (level bop < n) $
           ppPrec (level bop) e1 <+> pp bop <+> ppPrec (level bop + 1) e2
     ppPrec _ e' = pp e'
     ppParens b = if b then PP.parens else id

isBase :: Expression -> Bool
isBase Val{} = True
isBase Var{} = True
isBase Op1{} = True
isBase _ = False

level :: Bop -> Int
level Times  = 7
level Divide = 7
level Plus   = 5
level Minus  = 5
level Conj   = 1
level Disj   = 1
level Implies = 1
level Iff     = 1
level _      = 2    -- comparison operators

-- | TODO: Implement pretty printing for variables
instance PP Var where
  pp (Name n) = PP.text n
  pp (Proj n e1) = pp n <> (PP.brackets $ pp e1)

-- | TODO: Implement pretty printing for blocks

instance PP Block where
  pp (Block []) = PP.empty
  pp (Block (x:xs)) = pp x PP.$$ pp (Block xs)

-- | TODO: Implement the rest of pretty printing for statements.

instance PP Statement where
  pp Empty = PP.empty
  pp (Decl b e) = (<>) (pp "var" <+> pp b <+> pp ":=" <+> pp e) (pp ";")
  pp (Assert p) = PP.parens $ pp p <+> pp ";"
  pp (Assign v e) = pp v <+> pp ":=" <+> pp e <+> pp ";"
  pp (If e b1 (Block [])) = PP.vcat [ pp "if" <+> PP.parens (pp e) <+> pp "{"
                                    , PP.nest 2 (pp b1)
                                    , pp "}" ]
  pp (If e b1 b2) = PP.vcat [ pp "if" <+> PP.parens (pp e) <+> pp "{"
                                    , PP.nest 2 (pp b1)
                                    , pp "}"
                                    , pp "else {"
                                    , PP.nest 2 (pp b2)
                                    , pp "}" ]
  pp (While p e b) = PP.vcat [ pp "while" <+> PP.parens (pp e)
                              , PP.nest 2 $ (pp "invariant") <+> (pp p)
                              , pp "{ "
                              , PP.nest 2 (pp b)
                              , pp "}" ]

instance PP [Binding] where
  pp bs = PP.parens $ PP.hsep $ PP.punctuate PP.comma $ map pp bs

-- | TODO: Implement pretty printing for predicates
instance PP Predicate where
  pp (Predicate e) = pp e

-- | TODO: Finally, implement pretty printing for MiniDafny methods

instance PP Method where
  pp (Method n args rets specs b) = PP.vcat [ pp "method" <+> pp n <+>
                                              (pp args) <+>
                                              pp "returns" <+> (pp rets)
                                            , PP.nest 2 (pp specs)
                                            , pp "{ "
                                            , PP.nest 2 (pp b)
                                            , pp "}" ]

-- | Added instances for method pretty printing
instance PP Specification where 
  pp (Requires p) = pp "requires" <+> pp p 
  pp (Ensures p) = pp "ensures" <+> pp p
  pp (Modifies p) = pp "modifies" <+> pp p

instance PP [Specification] where 
  pp [] = PP.empty
  pp [s] = pp s 
  pp (s:ss) = pp s PP.$+$ pp ss

instance PP [Predicate] where 
  pp ([]) = PP.empty
  pp (p:ps) = pp p <+> pp ps

