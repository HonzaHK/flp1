-- FIT VUTBR - FLP - project 1
-- Jan Kubis / xkubis13
import System.Environment
import System.Exit
import Control.Monad
import Data.Typeable --typeOf
import Data.List.Split --splitOn
import Data.List -- \\
import qualified Data.Set as Set

import FaModule

helpText = "\nFIT VUTBR - FLP - project 1 - dka2mka\nauthor: Jan Kubis / xkubis13\nusage: ./dka2mka [ -i | -t ] [ FILE ]"
printHelp = putStrLn helpText

isArgsValid :: [[Char]] -> Bool
isArgsValid ["-t"] = True
isArgsValid ["-t",filename] = True
isArgsValid ["-i"] = True
isArgsValid ["-i",filename] = True
isArgsValid others = False

isArgsMinimize :: [[Char]] -> Bool
isArgsMinimize ["-t"] = True
isArgsMinimize ["-t",filename] = True
isArgsMinimize others = False

getArgsFilename :: [[Char]] -> [Char]
getArgsFilename [_,filename] = filename
getArgsFilename others = ""

getInput :: [Char] -> IO [Char]
getInput "" = getContents
getInput filename = readFile filename

--input: list of lines from user input
--output: KA tuple (Q,T,D,q0,F)
parseInput :: [[Char]] -> Fa
parseInput (l_states:l_init:l_final:l_trans) = Fa {
        fa_states=allStates,
        fa_alpha=validSymbols,
        fa_trans=allTrans,
        fa_init=initState,
        fa_fin=finStates,
        fa_nonFin= Set.difference allStates finStates
    }
    where   allStates = parseAllStates l_states
            allTrans = parseAllTrans l_trans
            initState = parseInitState l_init
            finStates = parseFinStates l_final

parseInput others = error "Invalid input!"

parseAllStates l_states = Set.fromList $ splitOn "," l_states
parseInitState l_init = l_init
parseFinStates l_final = Set.fromList $ splitOn "," l_final
parseAllTrans l_trans = map (\t->parseTrans t) l_trans
parseTrans t
    | length(exploded)/=3 = error "Invalid input!"
    | otherwise = Transition { tr_src=from, tr_sym=over,tr_dst=to}
    where   exploded = splitOn "," t
            from = exploded!!0
            over = exploded!!1
            to = exploded!!2


minimizeFa:: Fa -> IO()
minimizeFa fa = do--Fa {
--    fa_states= newStates,
--    fa_alpha= fa_alpha(fa),
--    fa_trans= [Transition{tr_src="a",tr_sym="b",tr_dst="c"}],
--    fa_init="i",
--    fa_fin= Set.fromList ["fin"],
--    fa_nonFin=  Set.fromList ["non-fin"]
--} where
    print $ hopcroft (Set.fromList [fa_fin fa, fa_nonFin fa]) (Set.singleton (fa_fin fa)) fa

    --newStates = Set.fromList (map (\x->unwords $ Set.toList x) (distinguish [fa_fin fa, fa_nonFin fa] [fa_fin fa] fa))

hopcroft:: Set.Set(Set.Set State) -> Set.Set(Set.Set State) -> Fa -> Set.Set(Set.Set State)
hopcroft p w fa
    | (Set.size w)==0 = p
    | otherwise = hopcroft mod_p mod_w fa
                    where   mod_p = modifyP p w fa
                            mod_w = modifyW p w fa

modifyP:: Set.Set(Set.Set State) -> Set.Set(Set.Set State) -> Fa -> Set.Set(Set.Set State)
modifyP p w fa = Set.union old_y (modifyY mod_y x)
                    where   wFirst = Set.elemAt 0 w
                            wRest = Set.deleteAt 0 w
                            x = hopcroftX (fa_trans fa) "b" wFirst
                            mod_y = filterY p x
                            old_y = Set.difference p mod_y


modifyW:: Set.Set(Set.Set State) -> Set.Set(Set.Set State) -> Fa -> Set.Set(Set.Set State)
modifyW p w fa = Set.empty
                    where   wFirst = Set.elemAt 0 w
                            wRest = Set.deleteAt 0 w

filterY:: Set.Set(Set.Set State) -> Set.Set State -> Set.Set(Set.Set State)
filterY p x = Set.filter (\y->( not(null(Set.intersection x y)) && not(null(Set.difference y x)) )) p

modifyY:: Set.Set(Set.Set State) -> Set.Set State -> Set.Set(Set.Set State)
modifyY mod_y x = Set.union (Set.map (\y->Set.intersection x y) mod_y) (Set.map (\y->Set.difference x y) mod_y)

hopcroftX:: [Transition] -> Symbol -> Set.Set State -> Set.Set State
hopcroftX trans sym dsts = getMultTransSrc $ filterTransByMultDst dsts (filterTransBySym sym trans)

filterAllTransBySym:: Symbol -> Fa -> [Transition]
filterAllTransBySym sym fa = filterTransBySym sym (fa_trans fa)
filterTransBySym:: Symbol -> [Transition] -> [Transition]
filterTransBySym sym trans = filter (\t->(tr_sym t == sym)) trans

filterTransByMultDst:: Set.Set State ->  [Transition] -> [Transition]
filterTransByMultDst states trans = filter (\t->(elem (tr_dst t) states)) trans
filterTransByDst:: State -> [Transition] -> [Transition]
filterTransByDst state trans = filter (\t->(tr_dst t == state)) trans

getMultTransSrc:: [Transition] -> Set.Set State
getMultTransSrc trans = Set.fromList $ map (\t->(tr_src t)) trans


main :: IO()
main = do
    argv <- getArgs
    when (not $ isArgsValid argv) $ error helpText

    input <- getInput $ getArgsFilename argv
    let fa = parseInput $ lines input
    when (not $ isFaValid fa) $ error "Invalid input!"
    --error "-------"
    if (isArgsMinimize argv) then do
        minimizeFa fa-- print $ fa_states $ minimizeFa fa
    else
        printFormatFa fa

    --print $ typeOf $ hopcroftX (fa_trans fa) "b" (Set.fromList ["1","6"])

    return ()