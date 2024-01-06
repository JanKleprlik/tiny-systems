// ----------------------------------------------------------------------------
// 02 - Implementing (basic) message sending
// ----------------------------------------------------------------------------

type Slot = 
  { Name : string
    Contents : Objekt
    IsParent : bool } 

and Objekt = 
  { mutable Slots : Slot list 
    mutable Code : Objekt option
    mutable Special : Special option }

and Special = 
  | String of string
  | Native of (Objekt -> Objekt)

// ----------------------------------------------------------------------------
// Helpers for creating things that we will often need
// ----------------------------------------------------------------------------

let makeObject slots code = 
  { Code = Some code; Special = None; Slots = slots }
let makeDataObject slots = 
  { Code = None; Special = None; Slots = slots }
let makeSpecialObject slots special = 
  { Code = None; Special = Some special; Slots = slots }

let makeSlot n contents = 
  { Name = n; Contents = contents; IsParent = false }
let makeParentSlot n contents = 
  { Name = n; Contents = contents; IsParent = true }


// Native method has a special object (F# function) as code
let makeNativeMethod f =
  makeObject [] (makeSpecialObject [] (Native(f)))

let addSlot (n:string) (contents:Objekt) (obj:Objekt) : unit = 
  // TODO: Make a new non-parent slot and add it to 'Slots' 
  // (create a new list and assign it to the 'Slots' field.)
  obj.Slots <- obj.Slots @ [makeSlot n contents]

let addParentSlot (n:string) (contents:Objekt) (obj:Objekt) : unit = 
  // TODO: Make a new parent slot and add it to 'Slots' 
  // (create a new list and assign it to the 'Slots' field.)
  obj.Slots <- obj.Slots @ [makeParentSlot n contents]

let cloneObject (obj:Objekt) : Objekt = 
  // TODO: Return a new 'Objekt' with exactly the same slots, code & special
  // as 'obj' (we are not doing "deep copy" - the two clones will share
  // references to the same objects in after their slots are copied)
  makeDataObject obj.Slots

// ----------------------------------------------------------------------------
// Lookup and message sending
// ----------------------------------------------------------------------------


let rec lookup (msg:string) (obj:Objekt) : list<Slot> = 
  // TODO: Implement message lookup (as documented in the Self handbook)
  // * If there is a slot named 'msg' in 'obj', return that 
  // * Otherwise, return all slots named 'msg' slots in objects 
  //   contained in all the parent slots of 'obj' (concatenate them)
  match obj.Slots |> List.tryFind (fun s -> s.Name = msg) with
  | Some slot -> [slot]
  | None -> parentLookup msg obj
and parentLookup msg obj = obj.Slots |> List.collect (fun s -> 
      if s.IsParent then lookup msg s.Contents else [])


// See also �3.3.7 (https://handbook.selflanguage.org/SelfHandbook2017.1.pdf)
//
// Note that we do not need special "primitive sends". Instead, we have special
// objects and so we need to run the "native" method when we it is called.
//
// Also not that we do not yet support passing arguments to methods!

let eval (slotValue:Objekt) (instance:Objekt) =
  // TODO: Implement the evaluation logic:
  // * If the 'slotValue' is a data object (has no 'Code') it is returned
  // * If the 'slotValue' has 'Code', we should invoke it. For now, we only
  //   handle the case where 'Code' is 'Special' and has 'Native' method.
  // * If the 'slotValue' has 'Code' that'ss not 'Special' fail (for now)
  //
  // To run the method we need to clone the method object (using cloneObject),
  // make the receiver as the parent of the clone (using addParentSlot)
  // and invoke the native function with the clone as argument.
  //
  // NOTE: Why do we set the receiver as parent of the activation record?
  // We can then send messages to it directly to access the receiver's slots!
  match slotValue.Code with
  | None -> slotValue
  | Some code ->
    match code.Special with
    | Some(Native fce) ->
      let cloned = cloneObject code
      addParentSlot "self*" instance cloned
      fce cloned
    | _ -> failwith "not implemented!"

let send (msg:string) (instance:Objekt) : Objekt =
  // TODO: Use 'lookup' to find slots with the name of the message 'msg'. If
  // there is exactly one, evaluate it using 'eval', otherwise report an error.
  let mslots = instance |> lookup msg;

  match mslots with
  | [slot] -> eval slot.Contents instance
  | _ -> mslots |> printf "slots: %A\n";
         failwithf "message not understood: %s" msg

// ----------------------------------------------------------------------------
// Helpers for testing & object construction
// ----------------------------------------------------------------------------

let lookupSlotValue n o = 
  match lookup n o with 
  | [ { Contents = it } ] -> it
  | sl -> failwithf "lookupSlotValue: Expected slot '%s' (found %d)!" n sl.Length

let getStringValue o = 
  match lookupSlotValue "value" o with
  | { Special = Some(String s) } -> s
  | _ -> failwith "not a string value"

// TODO: Define empty object with no data in it (needed below)
let empty : Objekt = makeDataObject []

let printCode = makeNativeMethod (fun arcd ->
  let s = send "value" arcd
  match s.Special with
    | Some(String(s)) ->
      printfn "%s" s
    | _ -> failwith "invalid special"
  empty
)

let stringPrototype = makeDataObject [
  makeSlot "print" printCode
]
let makeString s = 
  makeDataObject [ 
    makeSlot "value" (makeSpecialObject [] (String s)) 
    makeParentSlot "parent*" stringPrototype
  ]

// ----------------------------------------------------------------------------
// Tests - still lookups in a hierarchy of cats!
// ----------------------------------------------------------------------------

let cat = makeDataObject [
  makeSlot "sound" (makeString "Meow")
]
let wonderland = makeDataObject [
  makeSlot "book" (makeString "Alice in Wonderland")
]

let larry = makeDataObject [
  makeParentSlot "parent*" cat
  makeSlot "name" (makeString "Larry")
]

// Sending 'book' to 'larry' will now throw 'message not understood'!
larry |> send "name" |> send "print"
larry |> send "sound" |> send "print"
larry |> send "book" |> send "print"

let cheshire = makeDataObject [
  makeParentSlot "parent*" cat
  makeParentSlot "parent*" wonderland
  makeSlot "name" (makeString "Cheshire Cat")
]
// All of these should be OK!
cheshire |> send "name" |> send "print" 
cheshire |> send "sound" |> send "print"
cheshire |> send "book" |> send "print"