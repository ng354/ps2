
(* PART 1: EXPRESSION TREES *)
type 'a exprTree = 
  | Val of 'a
  | Unop of ('a -> 'a) * 'a exprTree 
  | Binop of ('a -> 'a -> 'a) * 'a exprTree * 'a exprTree

(*returns the number of function app in an exprTree*)
let rec count_ops (et : 'a exprTree) : int = 
  match et with 
  | Val n -> 0
  | Unop (a,b) -> 1 + count_ops b
  | Binop (a,b,c) -> 1 + count_ops b + count_ops c


(*returns an exprTree representing the execution of fact n (as defined in the
  write-up for exercise 5)*)
let rec make_fact_tree (n : int) : int exprTree =
  if n < 0 then failwith "oh no" else 
  match n with
  | 0 -> Val 1
  | _ -> Binop(( * ), Val n, make_fact_tree(n-1))
  
(*computes the expression represented by [et]*)
let rec eval (et : 'a exprTree) : 'a =
  match et with
  | Val et -> et
  | let (x, e1, in_e2) -> let val_x = eval et e1 in eval ((x, val_x) :: et) in_e2
  | Binop (operator, e1, e2) -> let v1 = eval et e1 in let v2 = eval et e2 in
  eval_op operator v1 v2

  and eval_op operator v1 v2 = 
  match operator with 
  | "+" -> v1 + v2
  | "-" -> v1-v2
  | "*" -> v1 * v2
  |"/" -> v1 / v2
  | _ -> failwith "Unknown operator" 

(* PART 2: FOLDING*)

let product (lst : float list) : float =
  List.fold_left ( *.) 1.0 lst

let concat_left (lst : string list) : string = 
  List.fold_left (^) "" lst

let concat_right (lst : string list) : string = 
  List.fold_right (^) lst ""

let mapi_lst (f: (int -> 'a -> 'b)) (lst: 'a list) : 'b list =
  List.fold_left (fun acc x -> acc@f((List.hd(x)) index)) lst [] 0
  

let outline (lst: string list) : string list =
    mapi_lst (fun index str -> string_of_int(index+1)^". "^str) lst
      
let scan_right (f: 'a -> 'b -> 'a) (acc: 'a) (lst: 'b list) : 'a list =
  List.fold_right (fun a acc -> a@[acc]) List.rev(lst) [] 


      
let scan_left (f: 'a -> 'b -> 'a) (acc: 'a) (lst: 'b list) : 'a list =
  failwith "Now, you must wear the cone of shame."

(* requires: n >= 1 
   returns: the list [1;2;...;n] *)
let countup (n:int) : int list =
  (* tail-recursive helper function for countup:  
       accumulate the answer in l, 
       starting from n and working down *)
  let rec countup' i l =
    if i<=0 then l
    else countup' (i-1) (i::l)
  in countup' n []

let fact_list (n: int) : int list =
  failwith "I do not like the cone of shame."

(* PART 3: MATRICES *)

type vector = int list
type matrix = vector list

exception MatrixFailure of string

open Printf
let show (m : matrix) : unit = 
  let () = List.iter (printf "%d ") m in ()


let insert_col (m : matrix) (c : vector) : matrix = 
  failwith "Seize reason in your own hand / With your own teeth savor the fruit"

let transpose (m : matrix) : matrix = 
  failwith "It is a way of thought"

let add_matrices (m1 : matrix) (m2 : matrix) : matrix = 
  failwith "My brain is open"

let multiply_matrices (m1 : matrix) (m2 : matrix) : matrix = 
  failwith "If numbers aren't beautiful, I don't know what is"

(* PART 4: PATTERN MATCHING *)

(*type definitions: **********************************************************)
type pat =
  | WCPat (*short for "wildcard pattern"; equivalent to an underscore*)
  | VarPat of string
  | UnitPat
  | ConstPat of int
  | TuplePat of pat list
  | StructorPat of string * (pat option) (*Short for "constructor pattern"*)

type value = 
  | ConstVal of int
  | UnitVal
  | TupleVal of value list
  | StructorVal of string * (value option)

type bindings = (string * value) list option

(*1. *************************************************************************)

let rec z f1 f2 p =
  let r = z f1 f2 in
    match p with
    | WCPat -> f1 ()
    | VarPat x -> f2 x
    | TuplePat ps -> List.fold_left (fun acc e -> (r e) + acc) 0 ps
    | StructorPat (_,Some p) -> r p
    | _ -> 0

(*counts the number of wildcards that occur in a pattern*)
let count_wcs (p: pat) : int = 
  failwith "Listen Ness. I'm going to tell you something very important. You may
  want to take notes. Ready? ......You're the chosen one." 

                                          (*-Talking Rock, in the Underworld*)

(*counts the number of wildcards in a pattern, and adds that quantity to the sum
of the lengths of the variable names used in the pattern*)
let count_wcs_and_var_lengths (p: pat) : int = 
  failwith "I have fake teeth, so I like soft foods. Not like rocks or stones. 
  They're too hard." 

                                          (*-Old Lady at Summer's Restaurant*)

(*counts how oftern a variable occurs in a pattern*)
let count_var (var_name: string) (p: pat) : int = 
  failwith "Kidnapping is wrong! I'll be careful not to kidnap anyone!"

                         (*- Mr T guy in front of Department Store in Twoson*)

(*2. *************************************************************************)

let rec extract_names (p: pat) : string list = 
  failwith "We offer a special discount on tombstones for those that have passed
  away in our hospital."

                                                      (*-Onett Hospital Sign*)

let has_dups (l: 'a list) : bool = 
  failwith "If you stay here too long, you'll end up frying your brain. Yes, you
  will. No, you will...not. Yesno you will won't." 

                                                         (*- Guy in Moonside*)

let all_vars_unique (p: pat) : bool = 
  failwith "I've come up with another wacky invention that I think has real 
  potential. Maybe you won't, but anyway...It's called the 'Gourmet Yogurt 
  Machine.' It makes many different flavors of yogurt. The only problem is, 
  right now, it can only make trout-flavored yogurt..."

                                                                (*-Apple Kid*)
(*3. *************************************************************************)

let all_answers (f: 'a -> 'b list option) (l: 'a list) : 'b list option =
  failwith "Oh yes, yes. My co-worker, Big Foot, dislikes violence. He's such a 
  nice guy, and he loves people. He often shares his beef jerky with me..." 

                                                             (*-Dr. Andonuts*)

(*4. *************************************************************************)

let rec match_pat (v,p) : bindings =
  failwith "If they break their contract, they'll be in deep doodoo with the 
  police. The police would probably say, 'Hey you guys!' or something like 
  that..." 

                                                  (*-Topolla Theater Manager*)

(*5. *************************************************************************)
exception NoAnswer

let rec first_answer (f: 'a -> 'b option) (l: 'a list) =
  failwith "Didactically speaking, seminal evidence seems to explicate the fact
  that your repudiation of entropy supports my theory of space-time synthesis. 
  Of this, I am irrefutably confident." 

                                              (*-Wordy guy at the Stoic Club*)

(*6. *************************************************************************)

let match_pats ((v: value), (ps: pat list)) : bindings =
  failwith "My dad really got after me. He said I get no dessert for the rest of
  the decade..." 

                                                                    (*-Pokey*) 
