// ----------------------------------------------------------------------------
// 03 - Searching for clauses & variable renaming
// ----------------------------------------------------------------------------

type Term = 
  | Atom of string
  | Variable of string
  | Predicate of string * Term list

type Clause =
  { Head : Term
    Body : Term list }

type Program = Clause list

let fact p = { Head = p; Body = [] }

let rule p b = { Head = p; Body = b }

// ----------------------------------------------------------------------------
// Substitutions and unification of terms
// ----------------------------------------------------------------------------

let rec substitute (subst:Map<string, Term>) term = 
  match term with
  | Variable var ->
  //oneliner?
    if Map.containsKey var subst then 
      subst.[var]
    else
      var |> Variable
  | Predicate (p, args) ->
    Predicate (p, args |> List.map (substitute subst))
  | term ->
    term


let rec substituteSubst (newSubst:Map<string, Term>) (subst:list<string * Term>) = 
  match subst with
  | [] ->
    []
  | (var, term)::tail ->
    (var, substitute newSubst term)::(substituteSubst newSubst tail)


let substituteTerms subst (terms:list<Term>) = 
  terms |> List.map (substitute subst)


let rec unifyLists l1 l2 = 
  match l1, l2 with
  | [], [] -> 
      [] |> Some
  | h1::t1, h2::t2 ->
    let s1 = unify h1 h2
    match s1 with
    | Some s1 ->
      let sm1 = Map.ofList s1
      let s2 = unifyLists (substituteTerms sm1 t1) (substituteTerms sm1 t2)
      match s2 with
      | Some s2 ->
        let sm2 = Map.ofList s2
        let ss1 = substituteSubst  sm2 s1
        ss1 @ s2 |> Some
      | _ -> None
    | _ -> None
  | _, _ -> None


and unify t1 t2 = 
  match t1, t2 with 
  | Atom a1, Atom a2 when a1 = a2 ->
    [] |> Some
  | Predicate (p1, args1), Predicate (p2, args2) when p1 = p2 ->
    unifyLists args1 args2
  | Variable v1, anythg ->
    [v1, anythg] |> Some
  | anythg, Variable v2 ->
    [v2, anythg] |> Some
  | _ ->
      None

// ----------------------------------------------------------------------------
// Searching the program (database) and variable renaming
// ----------------------------------------------------------------------------

let nextNumber = 
  let mutable n = 0
  fun () -> n <- n + 1; n

let rec freeVariables term = 
  // TODO: Return a list of all variables that appear in 'term'
  // (this may contain duplicates, we will eliminate them below)
  // HINT: Use List.collect: ('a -> list<'b>) -> list<'a> -> list<'b>
  match term with
  | Variable var ->
    [var]
  | Predicate (p, args) ->
    args |> List.collect freeVariables
  | _ ->
    []


let withFreshVariables (clause:Clause) : Clause =
  let headVars = clause.Head |> freeVariables |> List.distinct
  let bodyVars = clause.Body |> List.collect freeVariables |> List.distinct
  let subst = headVars @ bodyVars |> List.map (fun v -> v, Variable (v + (nextNumber ()).ToString()))
  { Head = clause.Head |> substitute (Map.ofList subst) ;
    Body = clause.Body |> List.map (substitute (Map.ofList subst)) }

let query (program:list<Clause>) (query:Term) 
    : list<Clause * list<string * Term>> =
  program |> List.choose (fun clause ->
    let freshClause = withFreshVariables clause
    match unify freshClause.Head query with
    | Some subst ->
      Some (freshClause, subst)
    | _ ->
      None
  )


// ----------------------------------------------------------------------------
// Querying the British royal family 
// ----------------------------------------------------------------------------

// Generating fresh variables - repeated calls
// should append new number to all variable names
rule (Predicate("grandparent", [Variable("X"); Variable("Y")])) [
  Predicate("parent", [Variable("X"); Variable("Z")])
  Predicate("parent", [Variable("Z"); Variable("Y")]) ]
|> withFreshVariables

// Some information about the British royal family 
let family = [ 
  fact (Predicate("male", [Atom("William")]))
  fact (Predicate("female", [Atom("Diana")]))
  fact (Predicate("male", [Atom("Charles")]))
  fact (Predicate("male", [Atom("George")]))
  fact (Predicate("parent", [Atom("Diana"); Atom("William")]))
  fact (Predicate("parent", [Atom("Charles"); Atom("William")]))
  fact (Predicate("parent", [Atom("William"); Atom("George")]))
  rule (Predicate("father", [Variable("X"); Variable("Y")])) [
    Predicate("parent", [Variable("X"); Variable("Y")])
    Predicate("male", [Variable("X")])
  ]
]

// Query: male(X)
// Match #1: male(William)
// Match #2: male(Charles)
// Match #3: male(George)
query family (Predicate("male", [Variable("X")]))

// Query: father(X, William)
// Match #1: father(X, Y) :- parent(X, Y), male(X)
query family (Predicate("father", [Variable("X"); Atom("William")]))
