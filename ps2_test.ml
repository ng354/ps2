open ps2
open Assertions	

let a = Binop ((+) ,
                Val 3,
                Unop ((~ -) ,
                      Binop ((/) ,
                             Val 5,
                             Val 2)))

let b = Val 3

TEST_UNIT "count_ops_test1" = assert_true (count_ops t = 3)
TEST_UNIT "count_ops_test1" = assert_true (count_ops b = 0)

let c = Binop(( * ) 
              Val 3,
              Binop (( * ) , 
              	     Val 2 ,
                     Binop (( * ) , 
                            Val 1 ,  
                            Val 1)))

TEST_UNIT "make_fact_tree_test1" = assert_true (make_fact_tree 3 = c)
TEST_UNIT "make_fact_tree_test1" = assert_true (make_fact_tree 0 = Val 1)
TEST_UNIT "make_fact_tree_test1" = assert_true (make_fact_tree 1 = Binop(( * ), Val 1, Val 1))



TEST_UNIT "product_test1" = assert_true (product [4.;5.;6.] = 120.)
TEST_UNIT "product_test1" = assert_true (product [] = 1.)
TEST_UNIT "product_test1" = assert_true (product [0.;5.] = 0.)
TEST_UNIT "product_test1" = assert_false (product [1.;3.5;4.] = 7.)

TEST_UNIT "concat_left_test1" = assert_true (concat_left ["nikita";"gupta"] = "nikitagupta")
TEST_UNIT "concat_left_test1" = assert_true (concat_left [] = "")
TEST_UNIT "concat_left_test1" = assert_true (concat_left ["nikita";""] = "nikita")
TEST_UNIT "concat_left_test1" = assert_true (concat_left ["nikita";" gupta";" is";" cool"] = "nikita gupta is cool")
TEST_UNIT "concat_left_test1" = assert_false (concat_left ["nikita";"gupta"] = "nikitagupt")

TEST_UNIT "concat_right_test1" = assert_true (concat_right ["nikita";"gupta"] = "nikitagupta")
TEST_UNIT "concat_right_test1" = assert_true (concat_right [] = "")
TEST_UNIT "concat_right_test1" = assert_true (concat_right ["nikita";""] = "nikita")
TEST_UNIT "concat_right_test1" = assert_true (concat_right ["nikita";" gupta";" is";" cool"] = "nikita gupta is cool")
TEST_UNIT "concat_right_test1" = assert_false (concat_right ["nikita";"gupta"] = "nikitagupt")




let () = Pa_ounit_lib.Runtime.summarize()