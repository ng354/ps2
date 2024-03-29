(*ng354, sda37*)

(* PART 1: EXPRESSION TREES *)

type 'a exprTree = 
  | Val of 'a
  | Unop of ('a -> 'a) * 'a exprTree 
  | Binop of ('a -> 'a -> 'a) * 'a exprTree * 'a exprTree

(*Returns the number of function applications in the input tree. Returns the "cost of the tree" *)
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
  
(*computes the expression represented by the tree. This function will behave as the REPL would behave
when asked to evaluate the equivalent OCaml expression.*)
let rec eval (et : 'a exprTree) : 'a =
  match et with
  | Val n -> n 
  | Unop (operator,b) -> operator(eval(b))
  | Binop (operator,a,b) -> operator (eval(a)) (eval(b))


(* PART 2: FOLDING*)

(*This function takes a float list and returns the product of the elements of that list. The product 
of an empty list is 1.0*)
let product (lst : float list) : float =
  List.fold_left ( *.) 1.0 lst

(*Given a string list, this functio will return the in-order concatenation of all the strings 
in the input list.*)
let concat_left (lst : string list) : string = 
  List.fold_left (^) "" lst

(*Given a string list, this functio will return the in-order concatenation of all the strings 
in the input list.*)
let concat_right (lst : string list) : string = 
  List.fold_right (^) lst ""

(*Similar to map, this function takes the index of the first element as the first argument and the element
itself as the second argument and applies the function. The helper function 'length' returns the length
of the 'a list in the argument*)
let mapi_lst (f: (int -> 'a -> 'b)) (lst: 'a list) : 'b list =
  let length (lst : 'a list) : int =
    List.fold_left (fun a _ -> a + 1) 0 lst 
  in List.rev(List.fold_left (fun a x -> (f (length(a)) x) :: a) [] lst )

(*This function produces a numbered outline from strings. It prepends a number, a period, and a space
to each string in the list.*)
let outline (lst: string list) : string list =
  mapi_lst (fun index str -> string_of_int(index+1)^". "^str) lst

(*This function returns a list of each value taken by the accumulator during processing. It is 
concatenating in the direction from right to left.*)      
let scan_right (f: 'a -> 'b -> 'a) (lst: 'b list) (acc: 'a)  : 'a list =
  List.rev(List.fold_right (fun x a -> (f (List.hd(a)) x) :: a) lst [acc]) 
      
(*This function returns a list of each value taken by the accumulator during processing. It is 
concatenating in the direction from left to right.*)      
let scan_left (f: 'a -> 'b -> 'a) (acc: 'a) (lst: 'b list) : 'a list =
  List.rev(List.fold_left (fun a x -> (f (List.hd(a)) x) :: a) [acc] lst)

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

(*This function takes an integer n and returns [1!; 2!; ...; (n-1)!; n!*)
let fact_list (n: int) : int list =
  match scan_left ( * ) 1 (countup n) with
  | [] -> []
  | h::t -> t;;

(* PART 3: MATRICES *)

type vector = int list
type matrix = vector list

exception MatrixFailure of string

(*This function prints the elements of the input matrix which is an int list list.*)
let show (m : matrix) : unit = 
  List.fold_left (fun acc x -> List.fold_left (fun acc1 y -> print_string(string_of_int(y)^" ")) 
    () x) () m 

(*Takes a matrix m and vector c and returns the insertion of c as the right-most
column in m. If the sizes of the matrix and vector are not the same, then the function
raises an exception. *)
let insert_col (m : matrix) (c : vector) : matrix = 
  if List.length c = List.length m then List.fold_right (fun x a -> 
    (x@(if List.length(a) < List.length(c) then [List.nth (List.rev(c))(List.length(a))] 
    else []))::a) m [] 
  else raise (MatrixFailure "cannot insert")
  

(*Takes in a matrix m as the argument and returns the tranpose of the matrix. The 
transpose of a matrix is another matrix with all of the rows and columns swapped. 
If matrix m initially had x rows and y columns, then the new matrix m' will have y rows
and x columns. The three invalid cases are matricies with no rows ([]) or no columns 
([ []; []; [] ]) and non-rectangular matrices where the sub-lists have different lengths).*)
let transpose (m : matrix) : matrix = 
  if List.length m = 0 || List.length(List.hd(m)) = 0 then raise (MatrixFailure "cannot transpose") 
  else
    let beginList = List.fold_left (fun x _ -> x @ [[]]) [] (List.hd m)
    in List.fold_left (fun b x -> (insert_col b x)) beginList m


(*In two input matrices are not of the same size, then add_matrices will fail and raise an exception. 
This function adds the two matrices as long as they are both the same size which means it has the same
number of rows and the same number of columns*)
let add_matrices (m1 : matrix) (m2 : matrix) : matrix = 
  List.fold_left (fun acc x -> acc @ [(List.map2 (fun z y -> z+y) x 
  (List.nth m2 (List.length(acc))))] ) []  m1
  

(*This function returns the matrix product of the two input matrices. If the two input matrices 
are not of the size that can be multiplied together, then the function will raise an exception. The number
of columns in the first matrix has to be the same as the number of rows in the second matrix for both of 
to be able to be multiplied to each other.*)
let multiply_matrices (m1 : matrix) (m2 : matrix) : matrix =
  let m2t = transpose m2 in 
  (*this helper function  multiplies a row and the transposed column*)
  let mult_rowcol (v1: vector)(v2: vector) : int = 
    (snd(List.fold_left (fun (a, b) x -> (List.tl(a), (x * List.hd(a)) + b)) 
      (v2, 0) v1)) in

  (*multiply the row and the matrix*)
  let mul_rowmat (v1: vector) (m2_t: matrix) : vector = 
    snd(List.fold_left (fun (x,y) v -> (x, y@[(mult_rowcol x v)])) (v1, []) m2t) in
  if (List.fold_left (fun x _ -> x+1) 0 m1)
  = (List.fold_left (fun x _ -> x+1) 0 m2t) then
  List.fold_left (fun x y -> x@[(mul_rowmat y m2t)]) [] m1

  else raise (MatrixFailure "cannot be multiplied")


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

let rec z (f1: unit -> int) (f2 : string -> int) (p : pat) : int =
  let r = z f1 f2 in
    match p with
    | WCPat -> f1 () (* counts wild card if need be*)
    | VarPat x -> f2 x (* counts num of variable if need be*)
    | TuplePat ps -> List.fold_left (fun acc e -> (r e) + acc) 0 ps    (* e is the next pat on the list ps, so applying f1 or f2 to it *)
    | StructorPat (_,Some p) -> r p   (* whatever the string is, just apply z to p*)
    | _ -> 0

(*counts the number of wildcards that occur in a pattern*)
let count_wcs (p: pat) : int = 
  z (fun () -> 1) (fun _ -> 0) p 

                                          (*-Talking Rock, in the Underworld*)

(*counts the number of wildcards in a pattern, and adds that quantity to the sum
of the lengths of the variable names used in the pattern*)
let count_wcs_and_var_lengths (p: pat) : int = 
  z (fun () -> 1) (fun x -> String.length x) p

                                          (*-Old Lady at Summer's Restaurant*)

(*counts how oftern a variable occurs in a pattern*)
let count_var (var_name: string) (p: pat) : int = 
  z (fun () -> 0) (fun x -> if var_name = x then 1 else 0) p

                         (*- Mr T guy in front of Department Store in Twoson*)

(*2. *************************************************************************)

let rec extract_names (p: pat) : string list = 
  match p with
  | VarPat x ->  [x]
  | TuplePat ps -> List.fold_left (fun acc e -> (extract_names e) @ acc) [] ps
  | StructorPat (_, Some p) -> extract_names p
  | _ -> []

                                                      (*-Onett Hospital Sign*)

let has_dups (l: 'a list) : bool = 
  let rec helper  (acc : 'b) (lst : 'a list): bool =
    if acc then acc else
    match lst with
    | [] -> acc
    | h::t -> helper (List.fold_right (fun x a -> if a then a else h = x) t false) t in helper false l
                                         
  (* probably a way to do this knowing a is false*) 
                                                                                                 (*- Guy in Moonside*)


(*This function determines whether all variables in the pattern have unique names.*)
let all_vars_unique (p: pat) : bool = 
  has_dups (extract_names p) = false


(*3. *************************************************************************)

(*This function applies the function's argument to every element of the list argument. If the function is
applied successfully to each element, the result is appended in any order. If the function applications
return None, then this functio should also return None. *)
let all_answers (f: 'a -> 'b list option) (l: 'a list) : 'b list option =
  let rec helper  (g: 'a -> 'b list option) (lst : 'a list) : 'b list =
    match lst with 
    | [] -> []
    | h::t -> match g h with
              | None -> []
              | Some x -> (helper g t) @ x 
  in if (List.length(l) = List.length(helper f l)) then Some (helper f l) else None

(*4. *************************************************************************)


(*This helper function combines a value list and a pat list to the problem set specifications*)
let rec combine (vlst : value list) (plst : pat list) : (string*value) list =
  if List.length(vlst) = List.length(plst) then
    match plst with
    | [] -> []
    | h::t -> match h with
              | VarPat x -> (combine (List.tl(vlst)) t) @ [(x,List.hd(vlst))] 
              | TuplePat lst -> (combine (List.tl(vlst)) t) @ (List.fold_left (fun a x -> a@[(x,List.hd(vlst))]) [] (extract_names (TuplePat lst)))
              | WCPat -> []
              | UnitPat -> []
              | ConstPat a -> []
              | StructorPat (_, patopt) -> match patopt with
                                           | None -> []
                                           | Some x -> combine vlst [x]
  else [] 

(*This function checks whether a value matches a pattern. If it doesn, then it returns Some 1, where 1
is the list of bindings produced by the match. If it does not match, then return None.*)
let match_pat ((v:value),(p:pat)) : bindings =
  let rec helper (v1,p1) : (string * value) list = 
    match (v1,p1) with 
    | (_,WCPat) -> []
    | (x,VarPat s) -> [(s,x)]
    | (UnitVal,UnitPat) -> []
    | (TupleVal vlst, TuplePat plst) -> combine vlst plst
    | (StructorVal (s,v_opt), StructorPat (s',p_opt)) -> if s = s' then match (v_opt,p_opt) with
                                                                         | (Some v, Some p) -> helper (v,p)
                                                                         | (None, None) -> [] 
                                                                         | (_,_) -> [("Failure", UnitVal)] 
                                                          else [("Failure", UnitVal)]              
    | (_,_) -> [("Failure", UnitVal)]
  in if helper (v,p) = [("Failure",UnitVal)] then None else Some (helper (v,p))




(*5. *************************************************************************)
exception NoAnswer

(*This function applies the function arguments to elements of the list argument until that function 
returns Some v, in which the funciton first_answer will return v. If this function never encounters
an element that produces Some v, then it should raise the exception NoAnswer.*)

let rec first_answer (f: 'a -> 'b option) (l: 'a list) : 'b =
  match l with
  | [] -> raise NoAnswer
  | h::t -> match (f h) with
            | Some x ->  x
            | None -> first_answer f t

                                              (*-Wordy guy at the Stoic Club*)

(*6. *************************************************************************)

(*This function checks wehther a value matches any of the patterns in the list argument. 
If it does, then it returns Some b, where b is the list of bindings produced by the first 
pattern that matches. Otherwise it will return None. *)

let match_pats ((v: value), (ps: pat list)) : bindings =
  let rec helper (v1,ps1) : (string * value) list =
    match ps1 with 
    | [] -> [("Failure", UnitVal)] 
    | h::t -> if match_pat (v1,h) = None then helper (v1,t) else match match_pat (v1,h) with
                                                                 | None -> []
                                                                 | Some x -> x
  in if helper (v,ps)  = [("Failure", UnitVal)] then None else Some (helper (v,ps))


                                                                    (*-Pokey*) 
