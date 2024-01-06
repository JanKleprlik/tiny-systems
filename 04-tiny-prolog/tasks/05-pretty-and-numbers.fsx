// ----------------------------------------------------------------------------
// 05 - Pretty printing & adding numbers to TinyProlog
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
// Pretty printing terms
// ----------------------------------------------------------------------------

let rec (|Number|_|) term = 
  match term with 
  | Atom "zero" -> Some 0
  | Predicate("succ", [n]) -> 
    match n with 
    | Number n -> Some (n + 1)
    | _ -> None
  | _ -> None


let rec formatTerm term = 
  match term with 
  // Simple cases for number, atom and variable are done already...
  | Number n -> string n
  | Atom s -> s
  | Variable v -> v
  | Predicate(p, items) ->
      // TODO: format all arguments recursively using 'formatTerm'
      // You can then concatenate the arguments using 'String.concat'
      items |> List.map formatTerm |> String.concat " "

// ----------------------------------------------------------------------------
// Searching the program (database) and variable renaming
// ----------------------------------------------------------------------------

let nextNumber = 
  let mutable n = 0
  fun () -> n <- n + 1; n

let rec freeVariables term = 
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
let rec solve program subst goals =
  // TODO: When printing the computed substitution 'subst', print
  // the terms nicely using 'formatTerm'. You can use 'for' loop like:
  // 'for var, term in subst do printfn ...'
  match goals with 
  | g::goals -> 
      let matches = g |> query program
      for clause, newSubst in matches do
        let newGoals = clause.Body @ goals |> substituteTerms (Map.ofList newSubst)
        let newSubst2 = substituteSubst (Map.ofList newSubst) subst
        solve program (newSubst2 @ newSubst) (newGoals)

  | [] -> 
    for var, term in subst do printfn "%s = %s" var (formatTerm term)

// ----------------------------------------------------------------------------
// Querying the British royal family 
// ----------------------------------------------------------------------------

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

// Queries from previous step (now with readable output)
solve family [] [ Predicate("father", [Variable("X"); Atom("William")]) ]
solve family [] [ Predicate("father", [Variable("X"); Variable("Y")]) ]


// ----------------------------------------------------------------------------
// Calculating with numbers
// ----------------------------------------------------------------------------

// Helper that generates a term representing a number
let rec num n = 
  // TODO: Write a helper that generates a term representing number.
  // This should return Atom("zero") when n is 0 and otherwise
  // succ(succ(...(zero))) with appropriate number of 'succ's.
  if n = 0 then
    Atom("zero")
  else
    Predicate("succ", [num (n - 1)])


// Addition and equality testing for Peano arithmetic
// $ add(zero, X, X)
// $ add(succ(X), Y, succ(Z)) :- add(X, Y, Z)
// $ eq(X, X)
let nums = [
  fact (Predicate("add", [Atom("zero"); Variable("X"); Variable("X")]))
  rule (Predicate("add", [Predicate("succ", [ Variable("X") ]); Variable("Y"); Predicate("succ", [ Variable("Z")]) ])) [
    Predicate("add", [Variable("X"); Variable("Y"); Variable("Z")])
  ]
  fact (Predicate("eq", [Variable("X"); Variable("X")]))
]


// Query: add(2, 3, X)
// Output should include: 'X = 5' 
//   (and other variables resulting from recursive calls)
solve nums [] [ Predicate("add", [num 2; num 3; Variable("X")]) ]

// Query: add(2, X, 5)
// Output should include: 'X = 3' 
//   (we can use 'add' to calculate subtraction too!)
solve nums [] [ Predicate("add", [num 2; Variable("X"); num 5]) ]

// Query: add(2, Y, X)
// Output should include: 'Y = Z??' and 'X = succ(succ(Z??))' 
//   (with some number for ?? - indicating that this can be any term)
solve nums [] [ Predicate("add", [num 2; Variable("Y"); Variable("X")]) ]
