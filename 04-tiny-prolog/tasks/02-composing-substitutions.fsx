// ----------------------------------------------------------------------------
// 02 - Composing and applying substitutions
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
  | Variable var when Map.containsKey var subst->
      subst.[var]
  | Variable var ->
      var |> Variable
  | Predicate (p, args) ->
    Predicate (p, args |> List.map (substitute subst))
  | Atom a -> a |> Atom


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
// Advanced unification tests requiring correct substitution
// ----------------------------------------------------------------------------

// Rquires (1)
// Example: loves(narcissus, narcissus) ~ loves(X, X)
// Returns: [ X -> narcissus ]
unify
  (Predicate("loves", [Atom("narcissus"); Atom("narcissus")]))
  (Predicate("loves", [Variable("X"); Variable("X")]))

// Requires (1)
// Example: loves(odysseus, penelope) ~ loves(X, X)
// Returns: None (cannot unify)
unify
  (Predicate("loves", [Atom("odysseus"); Atom("penelope")]))
  (Predicate("loves", [Variable("X"); Variable("X")]))

// Requires (1)
// Example: add(zero, succ(zero)) ~ add(Y, succ(Y))
// Returns: [ Y -> zero ]
unify
  (Predicate("add", [Atom("zero"); Predicate("succ", [Atom("zero")])]))
  (Predicate("add", [Variable("Y"); Predicate("succ", [Variable("Y")])]))

// Requires (2)
// Example: loves(X, narcissus) ~ loves(Y, X)
// Returns: [ X -> narcissus; Y -> narcissus ]
unify
  (Predicate("loves", [Variable("X"); Atom("narcissus")]))
  (Predicate("loves", [Variable("Y"); Variable("X")]))

// Requires (2)
// Example: add(succ(X), X) ~ add(Y, succ(Z))
// Returns: [ X -> succ(Z); Y -> succ(succ(Z)) ]
unify
  (Predicate("add", 
      [ Predicate("succ", [Variable("X")]); 
        Variable("X") ]))
  (Predicate("add", 
      [ Variable("Y"); 
        Predicate("succ", [Variable("Z")]) ]))

