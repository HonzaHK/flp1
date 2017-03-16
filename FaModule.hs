-- FIT VUTBR - FLP - project1 - dka-2-mka
-- Jan Kubis / xkubis13
module FaModule ( 
    Fa(..),
    Transition(..),
    State,
    Symbol,
    Alphabet,
    printFormatFa,
    validSymbols,
    isFaValid
) where

import Data.Char --isDigit
import Data.List --intersperse
import qualified Data.Set as Set

validSymbols:: Alphabet
validSymbols = map (\c->[c]) ['a'..'b']

type State = String
type Symbol = String
type Alphabet = [Symbol]
data Transition = Transition { tr_src::State,tr_sym::Symbol,tr_dst::State }
data Fa = Fa {
    fa_states::Set.Set State,
    fa_alpha::Alphabet,
    fa_trans::[Transition],
    fa_init::State,
    fa_fin:: Set.Set State,

    fa_nonFin:: Set.Set State
}

instance Show Transition where
    show (Transition tr_src tr_sym tr_dst) = 
        "("++show tr_src++","++show tr_sym++","++show tr_dst++")"

instance Show Fa where
    show (Fa fa_states fa_alpha fa_trans fa_init fa_fin fa_nonFin) = 
        "states:\n" ++ show fa_states ++ "\n" ++
        "symbols:\n" ++ show fa_alpha ++ "\n" ++
        "trans:\n" ++ show fa_trans ++ "\n" ++
        "init:\n" ++ show fa_init ++ "\n" ++
        "final:\n" ++ show fa_fin ++ "\n" ++
        "non-f:\n" ++ show fa_nonFin

printFormatFa :: Fa -> IO()
printFormatFa fa = do
    let q = concat (intersperse "," (Set.toList $ fa_states fa))
    let q0 = fa_init fa
    let f = concat (intersperse "," (Set.toList $ fa_fin fa))
    let d = concat (intersperse "\n" (map (\tr->tr_src tr++","++tr_sym tr++","++tr_dst tr) (fa_trans fa)))
    putStrLn q
    putStrLn q0
    putStrLn f
    putStrLn d


isStateDescrValid:: State -> Bool
isStateDescrValid s = all isDigit s
isStateDefined:: State -> Fa -> Bool
isStateDefined s fa = elem s (fa_states fa)
isInSymDescrValid:: Symbol -> Bool
isInSymDescrValid a = elem a validSymbols
isInSymDefined:: Symbol -> Fa -> Bool
isInSymDefined a fa = elem a (fa_alpha fa)
isTransValid:: Transition -> Fa -> Bool
isTransValid tr fa =   isStateDefined s0 fa &&
                        isInSymDefined a fa &&
                        isStateDefined s1 fa
                        where (s0,a,s1) = (tr_src tr,tr_sym tr,tr_dst tr)

isStatesValid:: Fa -> Bool
isStatesValid fa = all isStateDescrValid (fa_states fa)
isAlphabetValid:: Fa -> Bool
isAlphabetValid fa = all (\sym->isInSymDescrValid sym) (fa_alpha fa)
isTransitionsValid:: Fa -> Bool
isTransitionsValid fa = all (\tr->isTransValid tr fa) (fa_trans fa)
isInitStateValid:: Fa -> Bool
isInitStateValid fa = isStateDefined (fa_init fa) fa
isFinStatesValid:: Fa -> Bool
isFinStatesValid fa = all (\s->isStateDefined s fa) (fa_fin fa)
isFaValid::Fa -> Bool
isFaValid fa =  isStatesValid fa && 
                isAlphabetValid fa && 
                isTransitionsValid fa &&
                isInitStateValid fa &&
                isFinStatesValid fa

