// ----------------------------------------------------------------------------
// 02 - Solving type constraints with numbers and Booleans
// ----------------------------------------------------------------------------

// NOTE: We will only need lists later, but to make this exercise 
// a bit more interesting, we will implement constraint resolution 
// for lists here already. This will help you in the next steps!
type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type

let rec occursCheck vcheck ty =
  match ty with
  | TyVariable v -> v = vcheck
  | TyBool | TyNumber -> false
  | TyList ty -> occursCheck vcheck ty

 
let rec substType (subst:Map<string, Type>) ty = 
  match ty with
  | TyVariable v -> 
      match subst.TryFind v with
      | Some ty -> ty
      | None -> ty
  | TyBool | TyNumber -> ty
  | TyList ty -> TyList (substType subst ty)


let substConstrs (subst:Map<string, Type>) (cs:list<Type * Type>) = 
  // TODO: Apply substitution 'subst' to all types in constraints 'cs'
  cs |> List.map (fun (ty1, ty2) -> (substType subst ty1, substType subst ty2))
 

let rec solve cs =
  match cs with 
  | [] -> []
  | (TyNumber, TyNumber)::cs -> solve cs
  | (TyBool, TyBool)::cs -> solve cs
  | (TyList ty1, TyList ty2)::cs -> solve ((ty1, ty2)::cs)
  | (TyVariable v, ty)::cs | (ty, TyVariable v)::cs ->
      if occursCheck v ty then failwith "Cannot be solved"
      let cs = substConstrs (Map.ofList [v, ty]) cs
      let subst = solve cs
      let ty = substType (Map.ofList subst) ty
      (v, ty)::subst
  | _ -> failwith "Cannot be solved"


// ----------------------------------------------------------------------------
// Constraint solving tests
// ----------------------------------------------------------------------------

// Can be solved ('a = number, 'b = list<number>)
solve  
  [ TyList(TyVariable("a")), TyList(TyNumber)
    TyVariable("b"), TyList(TyVariable("a")) ]

// Cannot be solved (list<'a> <> bool)
solve  
  [ TyList(TyVariable("a")), TyVariable("b")
    TyVariable("b"), TyBool ]

// Can be solved ('a = number, 'b = list<number>)
solve  
  [ TyList(TyVariable("a")), TyVariable("b")
    TyVariable("b"), TyList(TyNumber) ]

// Cannot be solved ('a <> list<'a>)
solve  
  [ TyList(TyVariable("a")), TyVariable("a") ]
