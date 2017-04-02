-- FIT VUTBR - FLP - project1 - dka-2-mka
-- Jan Kubis / xkubis13
import System.Environment
import System.Exit
import Control.Monad
import Data.Typeable --typeOf
import Data.List.Split --splitOn
import Data.List -- \\
import qualified Data.Set as Set
import Debug.Trace

import FaModule

debug = flip trace

helpText = "\nFIT VUTBR - FLP - project 1 - dka-2-mka\nauthor: Jan Kubis / xkubis13\nusage: ./dka-2-mka [ -i | -t ] [ FILE ]"
printHelp = putStrLn helpText

--checks whether argv combination given is valid
isArgsValid :: [[Char]] -> Bool
isArgsValid ["-t"] = True
isArgsValid ["-t",filename] = True
isArgsValid ["-i"] = True
isArgsValid ["-i",filename] = True
isArgsValid others = False

--checks whether minimization flag was specified in argv
isArgsMinimize :: [[Char]] -> Bool
isArgsMinimize ["-t"] = True
isArgsMinimize ["-t",filename] = True
isArgsMinimize others = False

--returns filename specified from argv, if none was specified, returns "" (means stdin)
getArgsFilename :: [[Char]] -> [Char]
getArgsFilename [_,filename] = filename
getArgsFilename others = ""

--gets string content from stdin/file
getInput :: [Char] -> IO [Char]
getInput "" = getContents
getInput filename = readFile filename

--input: list of lines from user input
--output: KA tuple (Q,T,D,q0,F)
parseInput :: [[Char]] -> Fa
parseInput (l_states:l_init:l_final:l_trans) = Fa {
        fa_states=allStates,
        fa_alpha=alpha,
        fa_trans=allTrans,
        fa_init=initState,
        fa_fin=finStates,
        fa_nonFin= Set.difference allStates finStates
    }
    where   allStates = parseAllStates l_states
            allTrans = parseAllTrans l_trans
            alpha = parseAlpha allTrans
            initState = parseInitState l_init
            finStates = parseFinStates l_final
parseInput others = error "Invalid input!"

--parses separate line from user input
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
parseAlpha trans = Set.fromList(map (\t->tr_sym t) trans)


minimizeFa:: Fa -> Fa
minimizeFa fa = Fa {
    fa_states= states_min_f,
    fa_alpha= fa_alpha fa,
    fa_trans= trans_min,
    fa_init= init_min_f,
    fa_fin= fin_min_f,
    fa_nonFin=  Set.difference states_min_f fin_min_f
}   where   states_min = hopcroft (Set.fromList [fa_fin fa, fa_nonFin fa]) (Set.singleton (fa_fin fa)) fa -- raw data
            states_min_f = Set.map (\s->intercalate "" (Set.toList s)) states_min --transformed data (merged states)
            init_min = choose_init states_min (fa_init fa)
            init_min_f = intercalate "" (Set.toList init_min)
            fin_min = choose_fin states_min (fa_fin fa)
            fin_min_f = Set.map (\s->intercalate "" (Set.toList s)) fin_min
            trans_min = nub $ gen_trans states_min (fa_trans fa)

--chooses new init superstate according to old init state
choose_init:: Set.Set(Set.Set State) -> State -> Set.Set(State)
choose_init states_min init_old = Set.elemAt 0 $ Set.filter (\s->Set.member init_old s) states_min

--chooses new fin superstates according to old fin states
choose_fin:: Set.Set(Set.Set State) -> Set.Set State -> Set.Set(Set.Set State)
choose_fin states_min fin_old = Set.filter (\s->not(null(Set.intersection s fin_old))) states_min

--generates new transitions (with superstates on its sides) according to old transitions and states
gen_trans:: Set.Set(Set.Set State) -> [Transition] -> [Transition]
gen_trans states_min trans = map gen_t trans
                                where   gen_t t = Transition { tr_src=new_src, tr_sym=(tr_sym t),tr_dst=new_dst}
                                                    where   new_src = find_adq_min_state states_min $ tr_src t
                                                            new_dst = find_adq_min_state states_min $ tr_dst t

--given all superstates and one old state, this picks superstate containing the old state
find_adq_min_state:: Set.Set(Set.Set State) -> State -> State
find_adq_min_state states_min s = intercalate "" $ Set.toList $ Set.elemAt 0 $ Set.filter (\sm->(Set.member s sm)) states_min

--hopcroft while
hopcroft:: Set.Set(Set.Set State) -> Set.Set(Set.Set State) -> Fa -> Set.Set(Set.Set State)
hopcroft p w fa
    | (Set.size w)==0 = p
    | otherwise = hopcroft mod_p mod_w fa
                    where   (mod_p,(a,mod_w)) = hopcroftC (p,Set.deleteFindMin w) fa (fa_alpha fa)

--hopcroft foreach c
-- p,w have to be in tuple - recursion (param type == ret type)
hopcroftC:: (Set.Set(Set.Set State),(Set.Set State,Set.Set(Set.Set State))) -> Fa -> Alphabet -> (Set.Set(Set.Set State),(Set.Set State,Set.Set(Set.Set State)))
hopcroftC (p,(a,w)) fa cIter = if (Set.size cIter) == 0
                                    then (p, (a,w))
                                    else hopcroftC (modify_wp (p,(a,w)) fa c) fa cs
                                where   (c,cs) = Set.deleteFindMin cIter

--hopcroft foreach y
modify_wp:: (Set.Set(Set.Set State),(Set.Set State,Set.Set(Set.Set State))) -> Fa -> Symbol -> (Set.Set(Set.Set State),(Set.Set State,Set.Set(Set.Set State)))
modify_wp (p,(a,w)) fa c = (modify_p old_y mod_y x,(a,modify_w w mod_y x))
                            where   x = hopcroftX (fa_trans fa) c a
                                    mod_y = filterY p x
                                    old_y = Set.difference p mod_y

--hopcroft replacing Y in P
modify_p:: Set.Set(Set.Set State) -> Set.Set(Set.Set State) -> Set.Set State -> Set.Set(Set.Set State)
modify_p old_y mod_y x = Set.union old_y (replaceY mod_y x)

--hopcroft replacing Y in W
modify_w:: Set.Set(Set.Set State) -> Set.Set(Set.Set State) -> Set.Set State -> Set.Set(Set.Set State)
modify_w w mod_y x = Set.union w (Set.map cond mod_y)
                        where cond y = if (elem y w)
                                        then Set.union (Set.intersection x y) (Set.difference y x) --careful, Y\\X !!!!!
                                        else    if (Set.size(Set.intersection x y) <= Set.size(Set.difference y x))
                                                then Set.intersection x y
                                                else Set.difference y x

--computes Y set
filterY:: Set.Set(Set.Set State) -> Set.Set State -> Set.Set(Set.Set State)
filterY p x = Set.filter (\y->( not(null(Set.intersection x y)) && not(null(Set.difference y x)) )) p

--replaces single y set in P
replaceY:: Set.Set(Set.Set State) -> Set.Set State -> Set.Set(Set.Set State)
replaceY mod_y x = Set.union (Set.map (\y->Set.intersection x y) mod_y) (Set.map (\y->Set.difference y x) mod_y)

--computes X set
hopcroftX:: [Transition] -> Symbol -> Set.Set State -> Set.Set State
hopcroftX trans sym dsts = getMultTransSrc $ filterTransByMultDst dsts (filterTransBySym sym trans)

--filters fa transitions by sym
filterAllTransBySym:: Symbol -> Fa -> [Transition]
filterAllTransBySym sym fa = filterTransBySym sym (fa_trans fa)
--filters transitions by sym
filterTransBySym:: Symbol -> [Transition] -> [Transition]
filterTransBySym sym trans = filter (\t->(tr_sym t == sym)) trans
--filters transitions by multiple dst
filterTransByMultDst:: Set.Set State ->  [Transition] -> [Transition]
filterTransByMultDst states trans = filter (\t->(elem (tr_dst t) states)) trans
--filters transitions by dst
filterTransByDst:: State -> [Transition] -> [Transition]
filterTransByDst state trans = filter (\t->(tr_dst t == state)) trans
--gets a set of src states from given transitions
getMultTransSrc:: [Transition] -> Set.Set State
getMultTransSrc trans = Set.fromList $ map (\t->(tr_src t)) trans

--removes states without termination (states, which are not leading to a final state ANYTIME) and transitions correspondingly
reduceFa:: Fa -> Fa
reduceFa fa = Fa{
    fa_states= states_red,
    fa_alpha= fa_alpha fa,
    fa_trans= trans_min,
    fa_init= init_red,
    fa_fin= fin_red,
    fa_nonFin=  nonFin_min
}   where   states_red = getNonOverStates fa (fa_fin fa)
            init_red = fa_init fa
            fin_red = fa_fin fa
            trans_min = filter (\t->(Set.member (tr_src t) states_red)&&(Set.member (tr_dst t) states_red)) (fa_trans fa)
            nonFin_min = Set.fromList ["aaaa"]

--computes set of states, which are leading to a final state after N symbols
getNonOverStates:: Fa -> Set.Set State -> Set.Set State
getNonOverStates fa reached = if (reached==newReached)
                                then reached
                                else getNonOverStates fa newReached
                                where newReached = Set.union reached ( getMultTransSrc (filterTransByMultDst reached (fa_trans fa)) )

main :: IO()
main = do
    argv <- getArgs
    when (not $ isArgsValid argv) $ error helpText

    input <- getInput $ getArgsFilename argv
    let fa = parseInput $ lines input
    when (not $ isFaValid fa) $ error "Invalid input!"

    if (isArgsMinimize argv) then do
        printFormatFa $ reduceFa $ minimizeFa fa
    else
        printFormatFa $ fa

    return ()