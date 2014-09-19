open Ps2
open Assertions

(*Problem 1: Expresion Trees*)
let a = Binop ((+) ,
                Val 3,
                Unop ((~-) ,
                      Binop ((/) ,
                             Val 5,
                             Val 2)))

let b = Val 3

TEST_UNIT "count_ops_test1" = assert_true (count_ops a = 3)
TEST_UNIT "count_ops_test2" = assert_true (count_ops b = 0)

let c = Binop(( * ) ,
              Val 3,
              Binop (( * ) , 
              	     Val 2 ,
                     Binop (( * ) , 
                            Val 1 ,  
                            Val 1)))

TEST_UNIT "make_fact_tree_test1" = assert_true (make_fact_tree 3 = c)
TEST_UNIT "make_fact_tree_test2" = assert_true (make_fact_tree 0 = Val 1)
TEST_UNIT "make_fact_tree_test3" = assert_true (make_fact_tree 1 = Binop(( * ), Val 1, Val 1))

let d = Binop(( @ ) ,
               Val [2;3],
               Binop(( @ ) ,
                     Val [4;5],
                     Unop((List.rev) ,
                          Binop(( @ ) ,
                                Val [6;7],
                                Val [8;9]))))


TEST_UNIT "eval_test1" = assert_true (eval a = 1)
TEST_UNIT "eval_test1" = assert_true (eval b = 3)
TEST_UNIT "eval_test1" = assert_true (eval c = 6)
TEST_UNIT "eval_test1" = assert_true (eval d = [2;3;4;5;9;8;7;6])


(*Part 2: FOLDING*)

TEST_UNIT "product_test1" = assert_true (product [4.;5.;6.] = 120.)
TEST_UNIT "product_test2" = assert_true (product [] = 1.)
TEST_UNIT "product_test3" = assert_true (product [0.;5.] = 0.)
TEST_UNIT "product_test4" = assert_false (product [1.;3.5;4.] = 7.)

TEST_UNIT "concat_left_test1" = assert_true (concat_left ["nikita";"gupta"] = "nikitagupta")
TEST_UNIT "concat_left_test2" = assert_true (concat_left [] = "")
TEST_UNIT "concat_left_test3" = assert_true (concat_left ["nikita";""] = "nikita")
TEST_UNIT "concat_left_test4" = assert_true (concat_left ["nikita";" is";" cool"] = "nikita is cool")
TEST_UNIT "concat_left_test5" = assert_false (concat_left ["nikita";"gupta"] = "nikitagupt")

TEST_UNIT "concat_right_test1" = assert_true (concat_right ["nikita";"gupta"] = "nikitagupta")
TEST_UNIT "concat_right_test2" = assert_true (concat_right [] = "")
TEST_UNIT "concat_right_test3" = assert_true (concat_right ["nikita";""] = "nikita")
TEST_UNIT "concat_right_test4" = assert_true (concat_right ["nikita";" is";" cool"] = "nikita is cool")
TEST_UNIT "concat_right_test5" = assert_false (concat_right ["nikita";"gupta"] = "nikitagupt")

TEST_UNIT "mapi_lst1" = assert_true (mapi_lst (+) [3;0;-1;-3] = [3;1;1;0])
TEST_UNIT "mapi_lst2" = assert_true (mapi_lst ( * ) [7;8;9;10] = [0;8;18;30])
TEST_UNIT "mapi_lst3" = assert_false (mapi_lst ( / ) [2;4;6;8] = [0;3;5;7])

TEST_UNIT "outline1" = assert_true (outline ["point 1"; "point 2"; "point 3"] = ["1. point 1"; "2. point 2"; 
"3. point 3"])
TEST_UNIT "outline2" = assert_true (outline ["a"; "b"; "c"] = ["1. a"; "2. b"; "3. c"])
TEST_UNIT "outline3" = assert_false (outline ["a"; "b"; "c"] = ["2. a"; "3. b"; "1. c"])

TEST_UNIT "scan_left1" = assert_true (scan_left (^) "swag" ["zar"; "doz"] = ["swag"; "swagzar"; "swagzardoz"])
TEST_UNIT "scan_right1" = assert_true (scan_right (^) ["zar"; "doz"] "swag" = ["swag"; "dozswag"; "zardozswag"])

TEST_UNIT "fact_list1" = assert_true (fact_list 4 = [1;2;6;24])
TEST_UNIT "fact_list2" = assert_true (fact_list 0 = [])
TEST_UNIT "fact_list3" = assert_false (fact_list 10 = [1;2;6;24;120;720;1234;3434;213;12353])

TEST_UNIT "insert_col1" = assert_true (insert_col [[3;4];[5;6]] [7;9] = [[3;4;7];[5;6;9]])

TEST_UNIT "multiply_matrices1" = assert_true (multiply_matrices [[2; 4];[6; 8]] [[1; 3]; [4;5]]  = [[20; 28]; [38; 56]])



let wc1 = WCPat 
let wc2 = wc1
let wc3 = wc2
let var1 = VarPat "hello"
let var2 = var1
let var3 = var2
let var4 = VarPat "world2"
let var5 = VarPat "Suraj95"
let tuple0 = TuplePat []
let tuple1 = TuplePat [wc1;wc2;wc3]
let tuple2 = TuplePat [var1;var2;var3]
let tuple3 = TuplePat [var1;var4;var5]
let tuple4 = TuplePat [wc1;var1;wc2;var2;wc3;var5]
let tuple5 = TuplePat [var1;wc1;var4;wc2;var5;wc3]
let structor1 = StructorPat ("1",Some tuple4)
let const1 =  ConstPat 2

let val1 = ConstVal 1
let val2 = ConstVal 3
let val3 = TupleVal [val1;val2]
let val4 = StructorVal ("hello", Some val3)


TEST_UNIT "count_wcs0" = assert_true (count_wcs tuple0 = 0)
TEST_UNIT "count_wcs1" = assert_true (count_wcs tuple1 = 3)
TEST_UNIT "count_wcs2" = assert_true (count_wcs structor1 = 3)
TEST_UNIT "count_wcs3" = assert_true (count_wcs wc1 = 1)



TEST_UNIT "count_wcs_and_var_lengths1" = assert_true (count_wcs_and_var_lengths tuple0 = 0)
TEST_UNIT "count_wcs_and_var_lengths2" = assert_true (count_wcs_and_var_lengths tuple1 = 3)
TEST_UNIT "count_wcs_and_var_lengths3" = assert_true (count_wcs_and_var_lengths tuple2 = 15)
TEST_UNIT "count_wcs_and_var_lengths2" = assert_true (count_wcs_and_var_lengths tuple4 = 20)
TEST_UNIT "count_wcs_and_var_lengths2" = assert_true (count_wcs_and_var_lengths structor1 = 20)

TEST_UNIT "count_var0" = assert_true (count_var "world" tuple0 = 0)
TEST_UNIT "count_var1" = assert_true (count_var "world" tuple1 = 0)
TEST_UNIT "count_var2" = assert_true (count_var "hello" tuple1 = 0)
TEST_UNIT "count_var3" = assert_true (count_var "hello" tuple4 = 2)
TEST_UNIT "count_var4" = assert_true (count_var "hello" structor1 = 2)

TEST_UNIT "extract_names4" = assert_true (extract_names tuple0 = [])
TEST_UNIT "extract_names4" = assert_true (extract_names tuple1 = [])
TEST_UNIT "extract_names4" = assert_true (extract_names tuple3 = ["Suraj95";"world2";"hello"])
TEST_UNIT "extract_names4" = assert_true (extract_names tuple4 = ["Suraj95";"hello";"hello"])
TEST_UNIT "extract_names4" = assert_true (extract_names structor1 = ["Suraj95";"hello";"hello"])

TEST_UNIT "all_vars_unique0" = assert_true (all_vars_unique tuple0)
TEST_UNIT "all_vars_unique1" = assert_true (all_vars_unique tuple1)
TEST_UNIT "all_vars_unique2" = assert_true (all_vars_unique tuple3)
TEST_UNIT "all_vars_unique3" = assert_false (all_vars_unique tuple4)
TEST_UNIT "all_vars_unique4" = assert_false (all_vars_unique structor1)
TEST_UNIT "all_vars_unique5" = assert_true (all_vars_unique tuple5)

TEST_UNIT "all_answers1" = assert_true (all_answers (fun x -> if x = 0 then None else Some [x+1]) [1;2;3] = Some [4;3;2])
TEST_UNIT "all_answers2" = assert_true (all_answers (fun x -> if x = 0 then None else Some [x+1]) [] = Some [])
TEST_UNIT "all_answers5" = assert_true (all_answers (fun x -> if x = 0 then None else Some [x+1]) [0;1;2;3] = None)

TEST_UNIT "match_pat" = assert_true (match_pat (val1,const1) = Some [])


let () = Pa_ounit_lib.Runtime.summarize()