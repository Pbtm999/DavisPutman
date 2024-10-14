import Data.Maybe

type Literal = String
type Clause = [Literal] 
type CNF = [Clause]

-- Returns the negation of a literal adding or removing ~
negation :: Literal -> Literal
negation ('~':lit) = lit -- If there is a ~ on literal (the literal is a negation) returns only the literal
negation lit = '~':lit -- If theres is no ~ adds it to the literal

-- Find a literal in the first unitary clause present to propagate from the set of clauses
findLiteralToPropagate :: CNF -> Maybe Literal
findLiteralToPropagate [] = Nothing -- If there is no unitary clauses returns Nothing
findLiteralToPropagate (x:xs)
    | (tail x) == [] = Just (head x) -- Returns the literal from the first unitary clause
    | otherwise = findLiteralToPropagate xs -- Recursive to the next clause

-- Remove a literal from a clause, returning Nothing if the clause should be removed
removeFromClause :: Literal -> Clause -> Maybe Clause
removeFromClause _ [] = Just [] -- Keep empty clauses
removeFromClause lit clause
    | elem lit clause = Nothing -- Remove the entire clause from the CNF if the literal is found
    | elem (negation lit) clause = Just (filter (/= negation lit) clause) -- Remove the negation of the literal from the clause
    | otherwise = Just clause -- Otherwise return the clause without modifing it

-- Executes the propagation, finding the literal to propagate (could be improved to use a list of literals (the ones in unitary clauses)) 
-- removing the clause with the literal and the negation of literal from the clauses
propagation :: CNF -> Maybe CNF
propagation [] = Just [] -- If trys to propagate and empty CNF (usually wont happens since the DLL should return satisfable before propagating an empty CNF)
propagation set =
    case findLiteralToPropagate set of -- Finds the literal to use
        Nothing -> Nothing -- Returns nothing in case theres is no unitary clause
        Just p -> Just (catMaybes (map (removeFromClause p) set)) -- Returns the CNF with the modified clauses and without the ones removed

-- Needs to be improved, find a better heuristic
-- Basicly choose the first litteral in a CNF | used to select a literal to use on backtracking
selectLiteral :: CNF -> Literal
selectLiteral [[]] = ""
selectLiteral ((x:_):_) = x

-- Main Algoritm
-- Returns whatever the CNF is Satisfable or not returns TRUE if CNF is empty False in case of any empty clause inside CNF 
-- otherwise propagate in case of existence o unitary clauses if theres is not backtracks
davisPutnam :: CNF -> Bool
davisPutnam [] = True
davisPutnam set
    | any null set = False
    | otherwise =
        case propagation set of
            Nothing -> davisPutnam (set ++ [[lit]]) || davisPutnam (set ++ [[negation lit]])
                where lit = selectLiteral set
            Just set' -> davisPutnam set'
