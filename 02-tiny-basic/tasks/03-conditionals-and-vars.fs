// ----------------------------------------------------------------------------
// 03 - Add variables, conditionals and integer values
// ----------------------------------------------------------------------------
module TinyBASIC

type Value =
  | StringValue of string
  // NOTE: Added numerical and Boolean values
  | NumberValue of int
  | BoolValue of bool

type Expression = 
  | Const of Value
  // NOTE: Added functions and variables. Functions  are used for both 
  // functions (later) and binary operators (in this step). We use only
  // 'Function("-", [e1; e2])' and 'Function("=", [e1; e2])' in the demo.
  | Function of string * Expression list
  | Variable of string

type Command = 
  | Print of Expression
  | Run 
  | Goto of int
  // NOTE: Assign expression to a given variable and conditional that 
  // runs a given Command only if the expression evaluates to 'BoolValue(true)'
  | Assign of string * Expression
  | If of Expression * Command

type State = 
  { Program : list<int * Command> 
    // TODO: Add variable context to the program state
    Variables : Map<string, Value>
  }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------
let printValue value = 
  match value with
  | StringValue s -> printf "%s" s
  | NumberValue n -> printf "%d" n
  | BoolValue b -> printf "%b" b


let getLine state line =
  match state.Program |> List.tryFind (fun (l, _) -> l = line) with
  | Some (_, cmd) -> cmd
  | None -> failwith "Line not found"

let addLine state (line, cmd) = 
  List.sortBy (fun (l, _) -> l) (List.filter (fun (l, _) -> l <> line) state.Program @ [line, cmd])


// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evalExpression expr state = 
  match expr with
  | Const value -> value
  | Function(name, [e1; e2]) ->
      match name with
      | "+" -> 
          let v1 = evalExpression e1 state
          let v2 = evalExpression e2 state
          match v1, v2 with
          | NumberValue n1, NumberValue n2 -> NumberValue (n1 + n2)
          | _ -> failwith "Invalid arguments"
      | "-" ->
          let v1 = evalExpression e1 state
          let v2 = evalExpression e2 state
          match v1, v2 with
          | NumberValue n1, NumberValue n2 -> NumberValue (n1 - n2)
          | _ -> failwith "Invalid arguments"
      | "=" ->
          let v1 = evalExpression e1 state
          let v2 = evalExpression e2 state
          match v1, v2 with
          | NumberValue n1, NumberValue n2 -> BoolValue (n1 = n2)
          | _ -> failwith "Invalid arguments"
      | _ -> failwith "Invalid function"
  | Variable v ->
      match state.Variables.TryFind v with
      | Some v -> v
      | None -> failwith "Variable not found"
  

let rec runCommand state (line, cmd) =
  match cmd with 
  | Print(expr) ->
      evalExpression expr state |> printValue
      runNextLine state line
  | Run ->
      let first = List.head state.Program    
      runCommand state first
  | Goto(line) ->
      let cmd = getLine state line
      runCommand state (line, cmd)
  | Assign (n, e) ->
      let v = evalExpression e state
      let state = { state with Variables = state.Variables.Add(n, v) }
      runNextLine state line 
  | If (cond, fals) ->
      let v = evalExpression cond state
      match v with
      | BoolValue true -> runCommand state (line, fals)
      | BoolValue false -> runNextLine state line
      | _ -> failwith "Invalid arguments"


and runNextLine state line = 
  match state.Program |> List.tryFind (fun (l, _) -> l > line) with
  | Some (l, cmd) -> runCommand state (l, cmd)
  | None -> state

// ----------------------------------------------------------------------------
// Interactive program editing
// ----------------------------------------------------------------------------

let runInput state (line, cmd) =
  match line with
  | Some ln -> { state with Program = addLine state (ln, cmd) }
  | None -> runCommand state (System.Int32.MaxValue, cmd)
      

let runInputs state cmds =
  List.fold runInput state cmds

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let empty = { Program = []; Variables = Map.empty}

let helloOnce = 
  [ Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let helloInf = 
  [ Some 20, Goto 10
    Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let testVariables = 
  [ Some 10, Assign("S", Const(StringValue "HELLO WORLD\n")) 
    Some 20, Assign("I", Const(NumberValue 1))
    Some 30, Assign("B", Function("=", [Variable("I"); Const(NumberValue 1)]))
    Some 40, Print(Variable "S") 
    Some 50, Print(Variable "I") 
    Some 60, Print(Variable "B")
    None, Run ]

// NOTE: Simpler test program without 'If" (just variables and '=' function) 
runInputs empty testVariables |> ignore

let helloTen = 
  [ Some 10, Assign("I", Const(NumberValue 10))
    Some 20, If(Function("=", [Variable("I"); Const(NumberValue 1)]), Goto(60))
    Some 30, Print (Const(StringValue "HELLO WORLD\n")) 
    Some 40, Assign("I", Function("-", [ Variable("I"); Const(NumberValue 1) ]))
    Some 50, Goto 20
    Some 60, Print (Const(StringValue "")) 
    None, Run ]

// NOTE: Prints hello world ten times using conditionals
runInputs empty helloTen |> ignore
