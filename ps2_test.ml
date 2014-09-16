open ps2
open Assertions	

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