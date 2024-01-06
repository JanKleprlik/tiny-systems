// ----------------------------------------------------------------------------
// 02 - Implement interactive program editing
// ----------------------------------------------------------------------------
module TinyBASIC

type Value =
  | StringValue of string

type Expression = 
  | Const of Value

type Command = 
  | Print of Expression
  | Run 
  | Goto of int

type State = 
  { Program : list<int * Command> }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------
let printValue value = 
  // TODO: Take 'value' of type 'Value', pattern match on it and print it nicely.
  match value with
  | StringValue s -> printf "%s" s


let getLine state line =
  // TODO: Get a line with a given number from 'state.Program' (this can fail 
  // if the line is not there.) You need this in the 'Goto' command case below.
  match state.Program |> List.tryFind (fun (l, _) -> l = line) with
  | Some (_, cmd) -> cmd
  | None -> failwith "Line not found"

let addLine state (line, cmd) = 
  // TODO: Add a given line to the program state. This should overwrite 
  // a previous line (if there is one with the same number) and also ensure
  // that state.Program is sorted by the line number.
  // HINT: Use List.filter and List.sortBy. Use F# Interactive to test them!
  List.sortBy (fun (l, _) -> l) (List.filter (fun (l, _) -> l <> line) state.Program @ [line, cmd])

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------


let rec evalExpression expr = 
  match expr with
  | Const value -> value


let rec runCommand state (line, cmd) =
  match cmd with 
  | Print(expr) ->
      evalExpression expr |> printValue
      runNextLine state line
  | Run ->
      let first = List.head state.Program    
      runCommand state first
  | Goto(line) ->
      let cmd = getLine state line
      runCommand state (line, cmd)

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

let helloOnce = 
  [ Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let helloInf = 
  [ Some 20, Goto 10
    Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let empty = { Program = [] }


runInputs empty helloOnce |> ignore
runInputs empty helloInf |> ignore
