// ----------------------------------------------------------------------------
// 01 - Complete the simple numerical constraint solver
// ----------------------------------------------------------------------------

type Number =
  | Zero
  | Succ of Number
  | Variable of string


// NOTE: The four functions below currently return a wrong 
// result, but one that makes the code run. As you implement
// them (one by one), the tests should graudally start working.


let rec occursCheck (v:string) (n:Number) = 
  match n with
  | Zero -> false
  | Succ n -> occursCheck v n
  | Variable v' -> v = v'

let rec substite (v:string) (subst:Number) (n:Number) =
  // TODO: Replace all occurrences of variable 'v' in the
  // number 'n' with the replacement number 'subst'
  match n with
  | Zero -> Zero
  | Succ n -> Succ (substite v subst n)
  | Variable v' -> if v = v' then subst else n

let substituteConstraints (v:string) (subst:Number) (constraints:list<Number * Number>) = 
  constraints |> List.map (fun (n1, n2) -> (substite v subst n1, substite v subst n2))

let substituteAll (subst:list<string * Number>) (n:Number) =
  (n, subst) ||> List.fold (fun n (v, subst) -> substite v subst n)

let rec solve constraints = 
  match constraints with 
  | [] -> []
  | (Succ n1, Succ n2)::constraints ->
      solve ((n1, n2)::constraints)
  | (Zero, Zero)::constraints -> solve constraints
  | (Succ _, Zero)::_ | (Zero, Succ _)::_ -> 
      failwith "Cannot be solved"
  | (n, Variable v)::constraints 
  | (Variable v, n)::constraints ->
      if occursCheck v n then failwith "Cannot be solved (occurs check)"
      let constraints = substituteConstraints v n constraints
      let subst = solve constraints
      let n = substituteAll subst n
      (v, n)::subst

// Should work: x = Zero
solve 
  [ Succ(Variable "x"), Succ(Zero) ]

// Should faild: S(Z) <> Z
solve 
  [ Succ(Succ(Zero)), Succ(Zero) ]

// Should fail: No 'x' such that S(S(x)) = S(Z)
solve 
  [ Succ(Succ(Variable "x")), Succ(Zero) ]

// Not done: Need to substitute x/Z in S(x)
solve 
  [ Succ(Variable "x"), Succ(Zero)
    Variable "y", Succ(Variable "x") ]

// Not done: Need to substitute z/Z in S(S(z))
solve 
  [ Variable "x", Succ(Succ(Variable "z"))
    Succ(Variable "z"), Succ(Zero) ]

// Not done: Need occurs check
solve
  [ Variable "x", Succ(Variable "x") ]
