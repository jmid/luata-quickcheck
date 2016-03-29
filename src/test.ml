open OUnit

module L   = Last
module Str = Stringlattice
module VL  = Valuelattice
module EL  = Envlattice
module PL  = Proplattice
module ST  = Storelattice
module SL  = Statelattice
module AL  = Analysislattice

let vlcmp vl vl' = VL.leq vl vl' && VL.leq vl' vl
let slcmp sl sl' = SL.leq sl sl' && SL.leq sl' sl

let parse_analyze_lookup fname =
  let _,last = Frontend.parse_and_label fname in
  let alat   = Transfer.transfer_prog last in
  let slat   = AL.lookup alat last.L.ret_label in
  (last, alat, slat)

let test_syntax = "syntax tests" >:::
  [ "syntax01" >:: (fun () -> let _,_,_ = parse_analyze_lookup "examples/syntax01.lua" in
			      assert_bool "syntax01" true);
    "syntax02" >:: (fun () -> let _,_,_ = parse_analyze_lookup "examples/syntax02.lua" in
			      assert_bool "syntax02" true);
    "syntax03" >:: (fun () -> let _,_,_ = parse_analyze_lookup "examples/syntax03.lua" in
			      assert_bool "syntax03" true);
    "syntax04" >:: (fun () -> let _,_,_ = parse_analyze_lookup "examples/syntax04.lua" in
			      assert_bool "syntax04" true);
  ]

let test_simpl = "simple tests" >:::
  [ "simpl01" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/simpl01.lua" in
			     assert_equal (SL.read_name slat "x") VL.number);
    "simpl02" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/simpl02.lua" in
			     assert_equal ~cmp:slcmp slat SL.init);
    "simpl03" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/simpl03.lua" in
			     let vlat     = SL.read_name slat "f" in
			     assert_bool "simpl03" (VL.may_be_proc vlat));
    "simpl04" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/simpl04.lua" in
			     let vlat     = SL.read_name slat "f" in
			     assert_bool "simpl04" (VL.may_be_proc vlat));
    "simpl05" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/simpl05.lua" in
			     assert_equal slat SL.bot);
    "simpl06" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/simpl06.lua" in
			     assert_equal ~cmp:slcmp slat SL.init);
    "simpl07" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/simpl07.lua" in
			     assert_equal ~cmp:slcmp slat SL.bot);
    "simpl08" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/simpl08.lua" in
			     assert_equal ~cmp:slcmp slat SL.bot);
    "simpl09" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/simpl09.lua" in
			     assert_equal ~cmp:slcmp slat SL.init);
    "simpl10" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/simpl10.lua" in
			     assert_equal ~cmp:slcmp slat SL.init);
    "simpl11" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/simpl11.lua" in
			     let vlat     = SL.read_name slat "x" in
			     assert_equal ~cmp:vlcmp vlat VL.number);
    "simpl12" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/simpl12.lua" in
			     let vlat     = SL.read_name slat "x" in
			     assert_equal ~cmp:vlcmp vlat (VL.join VL.number (VL.string "foo")));
    "simpl13" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/simpl13.lua" in
			     let vlat     = SL.read_name slat "y" in
			     assert_bool "simpl13" (VL.leq (VL.join VL.nil VL.number) vlat));
    "simpl14" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/simpl14.lua" in
			     let vlat     = SL.read_name slat "y" in
			     assert_equal ~cmp:vlcmp vlat VL.nil);
    "simpl15" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/simpl15.lua" in
			     assert_equal ~cmp:slcmp slat SL.init);
    "simpl16" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/simpl16.lua" in
			     assert_equal ~cmp:slcmp slat SL.init);
    "simpl17" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/simpl17.lua" in
			     let vlat_x   = SL.read_name slat "x" in
			     assert_equal ~cmp:vlcmp vlat_x VL.number);
    "simpl18" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/simpl18.lua" in
			     let vlat_x   = SL.read_name slat "x" in
			     assert_equal ~cmp:vlcmp vlat_x VL.number);
    "simpl19" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/simpl19.lua" in
			     let vlat_r   = SL.read_name slat "r" in
			     assert_equal ~cmp:vlcmp vlat_r VL.nil);
    "simpl20" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/simpl20.lua" in
			     let vlat_r   = SL.read_name slat "r" in
			     assert_equal ~cmp:vlcmp vlat_r VL.number);
    "simpl21" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/simpl21.lua" in
			     let vlat_r   = SL.read_name slat "r" in
			     assert_equal ~cmp:vlcmp vlat_r (VL.string "foo"));
    "simpl22" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/simpl22.lua" in
			     let vlat_r   = SL.read_name slat "r" in
			     assert_equal ~cmp:vlcmp vlat_r VL.number);
    "simpl23" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/simpl23.lua" in
			     let vlat_r   = SL.read_name slat "r" in
			     assert_bool "simpl23" (VL.may_be_number vlat_r));
    "simpl24" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/simpl24.lua" in
			     let vlat_a   = SL.read_name slat "a" in
			     assert_bool "simpl24" (VL.may_be_number vlat_a
						    && VL.leq (VL.string "foo") vlat_a));
    "simpl25" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/simpl25.lua" in
			     let vlat_pr  = SL.read_name slat "print" in
			     assert_equal ~cmp:vlcmp vlat_pr (VL.builtin VL.Print));
    "simpl26" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/simpl26.lua" in
			     let vlat_x   = SL.read_name slat "x" in
			     assert_equal ~cmp:vlcmp vlat_x VL.number);
    "simpl27" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/simpl27.lua" in
			     let vlat_x   = SL.read_name slat "x" in
			     assert_equal ~cmp:vlcmp vlat_x (VL.string "10"));
    "simpl32" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/simpl32.lua" in
			     let vlat_y   = SL.read_name slat "y" in
			     assert_bool "simpl32" (VL.leq (VL.string "wink") vlat_y));
    "simpl34" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/simpl34.lua" in
			     let vlat_y   = SL.read_name slat "y" in
			     assert_bool "simpl34" (VL.leq (VL.string "wink") vlat_y));
    "simpl35" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/simpl35.lua" in
			     let vlat_y   = SL.read_name slat "y" in
			     assert_bool "simpl35" (VL.leq (VL.string "wink") vlat_y));
    "simpl36" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/simpl36.lua" in
			     let vlat_b   = SL.read_name slat "b" in
			     assert_bool "simpl36" (VL.may_be_nil vlat_b));
    "simpl37" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/simpl37.lua" in
			     let vlat_r   = SL.read_name slat "r" in
			     assert_bool "simpl37" (VL.may_be_number vlat_r));
    "simpl38" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/simpl38.lua" in
			     let vlat_r   = SL.read_name slat "r" in
			     assert_bool "simpl38"
			       (VL.leq (VL.string "19406028921940602892") vlat_r));
    "simpl39" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/simpl39.lua" in
			     let vlat_i   = SL.read_name slat "i" in
			     let vlat_i2  = SL.read_name slat "i2" in
			     assert_bool "simpl39" (VL.may_be_number vlat_i &&
						    VL.may_be_number vlat_i2)); ]

let test_for = "for loop tests" >:::
  [ "for01" >:: (fun () -> let _,_,slat   = parse_analyze_lookup "examples/for01.lua" in
			   let vlat_var   = SL.read_name slat "__var" in
			   let vlat_limit = SL.read_name slat "__limit" in
			   assert_bool "for01" (vlat_var = VL.nil && vlat_limit = VL.nil));
    "for02" >:: (fun () -> let _,_,slat   = parse_analyze_lookup "examples/for02.lua" in
			   let vlat_foo   = SL.read_name slat "foo" in
			   assert_bool "for02" (VL.leq (VL.string "12345678910") vlat_foo));
    "for03" >:: (fun () -> let _,_,slat   = parse_analyze_lookup "examples/for03.lua" in
			   let vlat_var   = SL.read_name slat "__var" in
			   let vlat_limit = SL.read_name slat "__limit" in
			   let vlat_step  = SL.read_name slat "__step" in
			   assert_bool "for03" (vlat_var = VL.nil && vlat_limit = VL.nil && vlat_step = VL.nil));
    "for04" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/for04.lua" in
			   let vlat_foo = SL.read_name slat "foo" in
			   assert_bool "for04" (VL.leq (VL.string "0246810") vlat_foo));
    "for05" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/for05.lua" in
			   let vlat_key = SL.read_name slat "key" in
			   let vlat_val = SL.read_name slat "value" in
			   let vlat_f   = SL.read_name slat "__f" in
			   let vlat_s   = SL.read_name slat "__s" in
			   let vlat_var = SL.read_name slat "__var" in
			   let vlat_t   = SL.read_name slat "t" in
			   let vlat_keys  = ST.lookup_all_keys slat.SL.store vlat_t in
			   let vlat_props = ST.lookup_all_props slat.SL.store vlat_t in
			   assert_bool "for05" (VL.nil = vlat_key && VL.nil = vlat_val &&
			       VL.nil = vlat_f && VL.nil = vlat_s && VL.nil = vlat_var &&
			       VL.may_be_table vlat_t && VL.may_be_number vlat_props &&
			       VL.leq (VL.string "a") vlat_keys && 
			       VL.leq (VL.string "b") vlat_keys && 
			       VL.leq (VL.string "c") vlat_keys ));
    "for06" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/for06.lua" in
			   let vlat_s     = SL.read_name slat "s" in
			   let vlat_keys  = ST.lookup_all_keys slat.SL.store vlat_s in
			   let vlat_props = ST.lookup_all_props slat.SL.store vlat_s in
			   let vlat_def   = ST.lookup_default_prop slat.SL.store vlat_s VL.anystring in
			   assert_bool "for06" (VL.may_be_table vlat_s && vlat_keys = VL.anystring &&
				                vlcmp vlat_props vlat_def &&
						VL.leq (VL.string "str") vlat_def &&
						VL.may_be_number vlat_def &&
						VL.may_be_bool vlat_def &&
						VL.leq (VL.builtin VL.Print) vlat_def &&
						VL.may_be_proc vlat_def &&
						VL.may_be_table vlat_def));
    "for07" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/for07.lua" in
			   let vlat_s     = SL.read_name slat "s" in
			   let vlat_keys  = ST.lookup_all_keys slat.SL.store vlat_s in
			   let vlat_props = ST.lookup_all_props slat.SL.store vlat_s in
			   let vlat_def   = ST.lookup_default_prop slat.SL.store vlat_s VL.anystring in
			   assert_bool "for07" (VL.may_be_table vlat_s &&
				                  vlcmp vlat_props vlat_def &&
						  VL.leq (VL.string "str") vlat_keys &&
						  VL.may_be_number vlat_keys &&
						  VL.may_be_bool vlat_keys &&
						  VL.leq (VL.builtin VL.Print) vlat_keys &&
						  VL.may_be_proc vlat_keys &&
						  VL.may_be_table vlat_keys &&
						  VL.leq (VL.string "a") vlat_props && 
						  VL.leq (VL.string "b") vlat_props && 
						  VL.leq (VL.string "c") vlat_props &&
						  VL.leq (VL.string "d") vlat_props &&
						  VL.leq (VL.string "e") vlat_props && 
						  VL.leq (VL.string "f") vlat_props ));
    "for08" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/for08.lua" in
			   let vlat_s     = SL.read_name slat "s" in
			   let vlat_keys  = ST.lookup_all_keys slat.SL.store vlat_s in
			   let vlat_props = ST.lookup_all_props slat.SL.store vlat_s in
			   let vlat_def   = ST.lookup_default_prop slat.SL.store vlat_s VL.anystring in
			   assert_bool "for08" (VL.may_be_table vlat_s && vlat_keys = VL.anystring &&
				                vlcmp vlat_props vlat_def &&
						VL.leq (VL.string "str") vlat_def &&
						VL.may_be_number vlat_def &&
						VL.may_be_bool vlat_def &&
						VL.leq (VL.builtin VL.Print) vlat_def &&
						VL.may_be_proc vlat_def &&
						VL.may_be_table vlat_def));
    "for09" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/for09.lua" in
			   let vlat_s     = SL.read_name slat "s" in
			   let vlat_keys  = ST.lookup_all_keys slat.SL.store vlat_s in
			   let vlat_props = ST.lookup_all_props slat.SL.store vlat_s in
			   let vlat_def   = ST.lookup_default_prop slat.SL.store vlat_s VL.anystring in
			   assert_bool "for09" (VL.may_be_table vlat_s &&
				                  vlcmp vlat_props vlat_def &&
						  VL.leq (VL.string "str") vlat_keys &&
						  VL.may_be_number vlat_keys &&
						  VL.may_be_bool vlat_keys &&
						  VL.leq (VL.builtin VL.Print) vlat_keys &&
						  VL.may_be_proc vlat_keys &&
						  VL.may_be_table vlat_keys &&
						  VL.leq (VL.string "a") vlat_props && 
						  VL.leq (VL.string "b") vlat_props && 
						  VL.leq (VL.string "c") vlat_props &&
						  VL.leq (VL.string "d") vlat_props &&
						  VL.leq (VL.string "e") vlat_props && 
						  VL.leq (VL.string "f") vlat_props ));
    "for10" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/for10.lua" in
			   let vlat_s     = SL.read_name slat "s" in
			   let vlat_keys  = ST.lookup_all_keys slat.SL.store vlat_s in
			   let vlat_props = ST.lookup_all_props slat.SL.store vlat_s in
			   let vlat_def   = ST.lookup_default_prop slat.SL.store vlat_s VL.number in
			   assert_bool "for10" (VL.may_be_table vlat_s &&
						  vlcmp vlat_keys VL.number &&
				                  vlcmp vlat_props vlat_def &&
						  VL.leq (VL.string "str") vlat_props &&
						  VL.may_be_number vlat_props &&
						  VL.may_be_bool vlat_props &&
						  VL.leq (VL.builtin VL.Print) vlat_props &&
						  VL.may_be_proc vlat_props &&
						  VL.may_be_table vlat_props ));
    "for11" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/for11.lua" in
			   let vlat_s     = SL.read_name slat "s" in
			   let vlat_keys  = ST.lookup_all_keys slat.SL.store vlat_s in
			   let vlat_props = ST.lookup_all_props slat.SL.store vlat_s in
			   let vlat_def   = ST.lookup_default_prop slat.SL.store vlat_s VL.number in
			   assert_bool "for11" (VL.may_be_table vlat_s &&
						  vlcmp vlat_keys VL.number &&
				                  vlcmp vlat_props vlat_def &&
						  VL.leq (VL.string "str") vlat_props &&
						  VL.may_be_number vlat_props &&
						  VL.may_be_bool vlat_props &&
						  VL.leq (VL.builtin VL.Print) vlat_props &&
						  VL.may_be_proc vlat_props &&
						  VL.may_be_table vlat_props ));
    "for12" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/for12.lua" in
			   let vlat_s     = SL.read_name slat "s" in
			   let vlat_keys  = ST.lookup_all_keys slat.SL.store vlat_s in
			   let vlat_props = ST.lookup_all_props slat.SL.store vlat_s in
			   let vlat_def   = ST.lookup_default_prop slat.SL.store vlat_s VL.number in
			   assert_bool "for12" (VL.may_be_table vlat_s &&
						  vlcmp vlat_keys VL.bot &&
				                  vlcmp vlat_props VL.bot &&
						  vlcmp vlat_def VL.nil));
  ]

let test_unop = "unary operation tests" >:::
  [ "unop01" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/unop01.lua" in
			    let vlat_y   = SL.read_name slat "y" in
			    assert_equal ~cmp:vlcmp VL.number vlat_y);
    "unop02" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/unop02.lua" in
			    let vlat_y   = SL.read_name slat "y" in
			    assert_equal ~cmp:vlcmp VL.number vlat_y);
  ]

let test_func = "function tests" >:::
  [ "func01" >:: (fun () -> let _,_,slat       = parse_analyze_lookup "examples/func01.lua" in
			    let vlat_f, vlat_g = SL.read_name slat "f", SL.read_name slat "g" in
			    assert_bool "func01" (VL.may_be_proc vlat_f && VL.may_be_proc vlat_g));
    "func02" >:: (fun () -> let _,_,slat       = parse_analyze_lookup "examples/func02.lua" in
			    let vlat_f, vlat_g = SL.read_name slat "f", SL.read_name slat "g" in
			    assert_bool "func02" (VL.may_be_proc vlat_f && VL.may_be_proc vlat_g));
    "func03" >:: (fun () -> let _,_,slat       = parse_analyze_lookup "examples/func03.lua" in
			    let vlat_f, vlat_z = SL.read_name slat "f", SL.read_name slat "z" in
			    assert_bool "func03" (VL.may_be_proc vlat_f && vlcmp (VL.string "foo") vlat_z));
    "func04" >:: (fun () -> let _,_,slat       = parse_analyze_lookup "examples/func04.lua" in
			    let vlat_f, vlat_g = SL.read_name slat "f", SL.read_name slat "g" in
			    assert_bool "func04" (VL.may_be_proc vlat_f && VL.may_be_proc vlat_g));
    "func05" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/func05.lua" in
			    let vlat_z   = SL.read_name slat "z" in
			    assert_equal ~cmp:vlcmp vlat_z (VL.join VL.number (VL.string "foo")));
    "func06" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/func06.lua" in
			    let vlat_v   = SL.read_name slat "v" in
			    assert_bool "func06" (VL.leq (VL.string "foo") vlat_v));
    "func07" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/func07.lua" in
			    let vlat_v   = SL.read_name slat "v" in
			    assert_bool "func07" (VL.leq (VL.string "foo") vlat_v
						    && VL.leq (VL.number) vlat_v ));
    "func08" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/func08.lua" in
			    let vlat_v   = SL.read_name slat "v" in
			    assert_equal ~cmp:vlcmp vlat_v VL.number);
    "func09" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/func09.lua" in
			    let vlat_f   = SL.read_name slat "f" in
			    assert_bool "func09" (VL.leq (VL.builtin VL.Tonumber) vlat_f));
    "func10" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/func10.lua" in
			    let vlat_f   = SL.read_name slat "f" in
			    assert_bool "func10" (VL.may_be_proc vlat_f));
    "func11" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/func11.lua" in
			    let vlat_r1   = SL.read_name slat "r1" in
			    let vlat_r2   = SL.read_name slat "r2" in
			    assert_bool "func11" (vlat_r1 = VL.number && vlat_r2 = VL.number));
    "func12" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/func12.lua" in
			    let vlat_r1   = SL.read_name slat "r1" in
			    let vlat_r2   = SL.read_name slat "r2" in
			    let vlat_r3   = SL.read_name slat "r3" in
			    assert_bool "func12" (vlat_r1 = VL.string "foo" &&
				                  vlat_r2 = VL.nil && vlat_r3 = VL.nil ));
    "func13" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/func13.lua" in
			    let vlat_r   = SL.read_name slat "r" in
			    assert_bool "func13" (vlat_r = VL.number));
    "func14" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/func14.lua" in
			    let vlat_r   = SL.read_name slat "r" in
			    assert_bool "func14" (vlat_r = VL.number));
    "func15" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/func15.lua" in
			    let vlat_r   = SL.read_name slat "r" in
			    assert_bool "func15" (VL.leq (VL.string "hello") vlat_r));
    "func16" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/func16.lua" in
			    let vlat_r   = SL.read_name slat "r" in
			    assert_bool "func16" (VL.leq (VL.string "hello") vlat_r)); ]

let test_method = "method tests" >:::
  [ "method01" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/method01.lua" in
			      let vlat_r   = SL.read_name slat "r" in
			      assert_bool "method01" (VL.leq (VL.string "boo") vlat_r));
    "method02" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/method02.lua" in
			      let vlat_r   = SL.read_name slat "r" in
			      assert_bool "method02" (VL.leq (VL.string "boo") vlat_r));
  ]

let test_string = "string tests" >:::
  [ "string01" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/string01.lua" in
			      let vlat_z   = SL.read_name slat "z" in
			      assert_equal ~cmp:vlcmp (VL.string "foobar") vlat_z);
    "string02" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/string02.lua" in
			      let vlat_z   = SL.read_name slat "z" in
			      assert_bool "string02" (VL.may_be_strings vlat_z));
    "string03" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/string03.lua" in
			      let vlat_s   = SL.read_name slat "s" in
			      assert_equal ~cmp:vlcmp (VL.string "foo\\n") vlat_s);
    "string04" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/string04.lua" in
			      let vlat_s   = SL.read_name slat "s" in
			      assert_equal ~cmp:vlcmp (VL.string "foo\\\"") vlat_s);
    "string05" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/string05.lua" in
			      let vlat_s   = SL.read_name slat "s" in
			      assert_equal ~cmp:vlcmp (VL.string "<html>:?!@#$%^&*()_-\\\"+=\\\\") vlat_s);
    "string06" >:: (fun () -> let _,_,slat  = parse_analyze_lookup "examples/string06.lua" in
			      let vlat_desc = SL.read_name slat "description" in
			      assert_bool "string06" (VL.may_be_strings vlat_desc));
    "string07" >:: (fun () -> let _,_,slat  = parse_analyze_lookup "examples/string07.lua" in
			      let vlat_str1 = SL.read_name slat "str1" in
			      let vlat_str2 = SL.read_name slat "str2" in
			      let vlat_str3 = SL.read_name slat "str3" in
			      assert_bool "string07" (VL.may_be_strings vlat_str1
							&& VL.may_be_strings vlat_str2
							&& VL.may_be_strings vlat_str3 ));
    "string08" >:: (fun () -> let _,_,slat  = parse_analyze_lookup "examples/string08.lua" in
			      let vlat_str2 = SL.read_name slat "str2" in
			      assert_equal ~cmp:vlcmp (VL.string " sfdsf ") vlat_str2);
    "string09" >:: (fun () -> let _,_,slat  = parse_analyze_lookup "examples/string09.lua" in
			      let vlat_len  = SL.read_name slat "len" in
			      assert_equal ~cmp:vlcmp VL.number vlat_len);
    "string10" >:: (fun () -> let _,_,slat  = parse_analyze_lookup "examples/string10.lua" in
			      assert_bool "string10" (not (slcmp SL.bot slat)));
    "string11" >:: (fun () -> let _,_,slat  = parse_analyze_lookup "examples/string11.lua" in
			      let vlat_u    = SL.read_name slat "u" in
			      let vlat_l    = SL.read_name slat "l" in
			      assert_bool "string11" (vlcmp (VL.string "FOOBAR") vlat_u
						      && vlcmp (VL.string "foobar") vlat_l));
    "string12" >:: (fun () -> let _,_,slat  = parse_analyze_lookup "examples/string12.lua" in
			      let vlat_c    = SL.read_name slat "c" in
			      let vlat_s1   = SL.read_name slat "s1" in
			      let vlat_s2   = SL.read_name slat "s2" in
			      assert_bool "string11" (VL.leq (VL.string "ABC") vlat_c
						      && VL.may_be_strings vlat_s1
						      && VL.may_be_strings vlat_s2));
  ]


let test_coerce = "coercion tests" >:::
  [ "coerce01" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/coerce01.lua" in
			      let vlat_z   = SL.read_name slat "z" in
			      assert_bool "coerce01" (VL.may_be_strings vlat_z));
    "coerce02" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/coerce02.lua" in
			      let vlat_z   = SL.read_name slat "z" in
			      assert_bool "coerce02" (VL.may_be_number vlat_z));
    "coerce03" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/coerce03.lua" in
			      let vlat_z   = SL.read_name slat "z" in
			      assert_bool "coerce03" (VL.may_be_number vlat_z));
    "coerce04" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/coerce04.lua" in
			      let vlat_z   = SL.read_name slat "z" in
			      assert_bool "coerce04" (VL.may_be_number vlat_z));
    "coerce05" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/coerce05.lua" in
			      let vlat_z   = SL.read_name slat "z" in
			      assert_bool "coerce05" (VL.may_be_number vlat_z));
    "coerce06" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/coerce06.lua" in
			      let vlat_z   = SL.read_name slat "z" in
			      assert_bool "coerce06" (VL.may_be_number vlat_z));
    "coerce07" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/coerce07.lua" in
			      let vlat_z   = SL.read_name slat "z" in
			      assert_bool "coerce07" (VL.may_be_number vlat_z));
    "coerce08" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/coerce08.lua" in
			      let vlat_z   = SL.read_name slat "z" in
			      assert_bool "coerce08" (VL.may_be_number vlat_z));
    "coerce09" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/coerce09.lua" in
			      let vlat_z   = SL.read_name slat "z" in
			      assert_bool "coerce08" (VL.may_be_number vlat_z));
    "coerce10" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/coerce10.lua" in
			      let vlat_z   = SL.read_name slat "z" in
			      assert_bool "coerce10" (VL.may_be_bool vlat_z));
    "coerce11" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/coerce11.lua" in
			      let vlat_z   = SL.read_name slat "z" in
			      assert_bool "coerce11" (VL.leq vlat_z VL.bot));
    "coerce12" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/coerce12.lua" in
			      let vlat_z   = SL.read_name slat "z" in
			      assert_bool "coerce12" (VL.leq vlat_z VL.bot));
    "coerce13" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/coerce13.lua" in
			      let vlat_z   = SL.read_name slat "z" in
			      assert_bool "coerce13" (VL.leq vlat_z VL.bot));
    "coerce14" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/coerce14.lua" in
			      let vlat_z   = SL.read_name slat "z" in
			      assert_bool "coerce14" (VL.leq vlat_z VL.bot));
    "coerce15" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/coerce15.lua" in
			      let vlat_z   = SL.read_name slat "z" in
			      assert_bool "coerce15" (VL.may_be_bool vlat_z)); ]

let test_return = "return tests" >:::
  [ "return01" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/return01.lua" in
			      assert_equal ~cmp:slcmp slat SL.bot); 
    "return02" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/return02.lua" in
			      let vlat_r   = SL.read_name slat "r" in
			      assert_equal ~cmp:vlcmp vlat_r VL.nil);
    "return03" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/return03.lua" in
			      let vlat_r1  = SL.read_name slat "r1" in
			      let vlat_r2  = SL.read_name slat "r2" in
			      assert_bool "return03" (vlat_r1 = (VL.string "foo") && vlat_r2 = VL.bool));
    "return04" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/return04.lua" in
			      let vlat_r1  = SL.read_name slat "r1" in
			      let vlat_r2  = SL.read_name slat "r2" in
			      assert_bool "return04" (vlat_r1 = (VL.string "foo") && vlat_r2 = (VL.string "jens")));
    "return05" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/return05.lua" in
			      let vlat_r3  = SL.read_name slat "r3" in
			      let vlat_r4  = SL.read_name slat "r4" in
			      let vlat_r5  = SL.read_name slat "r5" in
			      assert_bool "return05" (vlat_r3 = VL.nil && vlat_r4 = VL.nil && vlat_r5 = VL.nil));
    "return06" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/return06.lua" in
			      let vlat_t   = SL.read_name slat "t" in
			      let vlat_r1  = ST.lookup_prop slat.SL.store vlat_t "r1" in
			      let vlat_l   = SL.read_name slat "l" in
			      let vlat_r2  = ST.lookup_prop slat.SL.store vlat_t "r2" in
			      let vlat_r3  = ST.lookup_prop slat.SL.store vlat_t "r3" in
			      let vlat_r4  = SL.read_name slat "r4" in
			      assert_bool "return06" (VL.may_be_table vlat_t &&
							VL.leq (VL.string "foo") vlat_r1 &&
							(vlcmp vlat_l (VL.string "jens")) &&
							vlat_r2 = VL.nil &&
				                        vlat_r3 = VL.nil && vlat_r4 = VL.nil));
    "return07" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/return07.lua" in
			      let vlat_t   = SL.read_name slat "t" in
			      let vlat_r1  = ST.lookup_prop slat.SL.store vlat_t "r1" in
			      let vlat_l   = SL.read_name slat "l" in
			      let vlat_r2  = ST.lookup_prop slat.SL.store vlat_t "r2" in
			      let vlat_r3  = SL.read_name slat "r3" in
			      assert_bool "return07" (VL.may_be_table vlat_t &&
							VL.leq (VL.string "foo") vlat_r1 &&
							vlat_l = (VL.string "jens") &&
				                        VL.leq (VL.string "foo") vlat_r2 &&
				                        vlat_r3 = (VL.string "hest") ));
  ]

let test_scope = "scope tests" >:::
  [ "scope01" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/scope01.lua" in
			     let vlat_x   = SL.read_name slat "x" in
			     let vlat_y   = SL.read_name slat "y" in
			     let vlat_z   = SL.read_name slat "z" in
			     assert_bool "scope01" (vlat_x = VL.number &&
				                    vlat_y = VL.number && vlat_z = VL.nil));
    "scope02" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/scope02.lua" in
			     let vlat_x   = SL.read_name slat "x" in
			     let vlat_y   = SL.read_name slat "y" in
			     let vlat_z   = SL.read_name slat "z" in
			     let vlat_w   = SL.read_name slat "w" in
			     assert_bool "scope02" (vlat_x = VL.number && vlat_y = VL.number
 						    && vlat_z = VL.nil && vlat_w = VL.nil));
    "scope03" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/scope03.lua" in
			     let vlat_r   = SL.read_name slat "r" in
			     assert_bool "scope03" (VL.may_be_proc vlat_r));
    "scope04" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/scope04.lua" in
			     let vlat_r   = SL.read_name slat "r" in
			     assert_equal ~cmp:vlcmp VL.nil vlat_r);
    "scope05" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/scope05.lua" in
			     let vlat_r   = SL.read_name slat "r" in
			     assert_equal ~cmp:vlcmp VL.bool vlat_r);
    "scope06" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/scope06.lua" in
			     let vlat_r   = SL.read_name slat "r" in
			     assert_equal ~cmp:vlcmp VL.bot vlat_r);
    "scope07" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/scope07.lua" in
			     let vlat_r   = SL.read_name slat "r" in
			     assert_equal ~cmp:vlcmp VL.bot vlat_r);
    "scope08" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/scope08.lua" in
			     let vlat_x   = SL.read_name slat "x" in
			     let vlat_y   = SL.read_name slat "y" in
			     assert_bool "scope08" (vlat_x = VL.bool && vlat_y = VL.number));
    "scope09" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/scope09.lua" in
			     let vlat_r   = SL.read_name slat "r" in
			     assert_bool "scope09" (vlat_r = VL.nil)); ]

let test_table = "table tests" >:::
  [ "table01" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/table01.lua" in
			     let vlat_x   = SL.read_name slat "x" in
			     assert_bool "table01" (VL.may_be_table vlat_x));
    "table02" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/table02.lua" in
			     let vlat_x   = SL.read_name slat "x" in
			     let vlat     = ST.lookup_prop slat.SL.store vlat_x "height" in
			     assert_equal ~cmp:vlcmp VL.number vlat);
    "table03" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/table03.lua" in
			     let vlat_y   = SL.read_name slat "y" in
			     assert_equal ~cmp:vlcmp VL.number vlat_y);
    "table04" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/table04.lua" in
			     let vlat_y   = SL.read_name slat "y" in
			     assert_equal ~cmp:vlcmp VL.number vlat_y);
    "table05" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/table05.lua" in
			     let vlat_x   = SL.read_name slat "x" in
			     assert_bool "table05" (VL.leq (VL.string "onenumber") vlat_x));
    "table06" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/table06.lua" in
			     let vlat_x   = SL.read_name slat "x" in
			     assert_bool "table06" (VL.may_be_number vlat_x && VL.may_be_bool vlat_x
						    && VL.leq (VL.string "hello") vlat_x
						    && VL.may_be_proc vlat_x ));
    "table07" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/table07.lua" in
			     let vlat_t   = SL.read_name slat "t" in
			     assert_bool "table07" (VL.may_be_table vlat_t));
    "table08" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/table08.lua" in
			     let vlat_t   = SL.read_name slat "t" in
			     let vlat_y   = ST.lookup_prop slat.SL.store vlat_t "y" in
			     let vlat_1   = ST.lookup_prop slat.SL.store vlat_t "1" in
			     assert_bool "table08" (VL.number = vlat_y && VL.may_be_strings vlat_1));
    "table09" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/table09.lua" in
			     let vlat_x   = SL.read_name slat "x" in
			     assert_bool "table09" (VL.may_be_number vlat_x &&
						    VL.leq (VL.string "z") vlat_x));
    "table10" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/table10.lua" in
			     let vlat_t   = SL.read_name slat "t" in
			     let vlat_def = ST.lookup_default_prop slat.SL.store vlat_t VL.number in
			     assert_bool "table10" (VL.may_be_bool vlat_def));
    "table11" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/table11.lua" in
			     let vlat_r   = SL.read_name slat "r" in
			     assert_bool "table11" (VL.may_be_nil vlat_r));
    "table12" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/table12.lua" in
			     let vlat_r   = SL.read_name slat "r" in
			     assert_bool "table12" (VL.may_be_table vlat_r));
    "table13" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/table13.lua" in
			     let vlat_r   = SL.read_name slat "r" in
			     assert_bool "table13" (VL.may_be_nil vlat_r));
    "table14" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/table14.lua" in
			     let vlat_k   = SL.read_name slat "k" in
			     let vlat_v   = SL.read_name slat "v" in
			     assert_bool "table14"
			       (VL.leq (VL.string "a") vlat_k && VL.leq (VL.string "str") vlat_v &&
				VL.leq (VL.string "b") vlat_k && VL.may_be_number vlat_v &&
				VL.leq (VL.string "c") vlat_k && VL.may_be_bool vlat_v &&
				VL.leq (VL.string "d") vlat_k && VL.leq (VL.builtin VL.Print) vlat_v &&
				VL.leq (VL.string "e") vlat_k && VL.may_be_proc vlat_v &&
				VL.leq (VL.string "f") vlat_k && VL.may_be_table vlat_v ));
    "table15" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/table15.lua" in
			     let vlat_k   = SL.read_name slat "k" in
			     let vlat_v   = SL.read_name slat "v" in
			     assert_bool "table15"
			       (VL.leq (VL.string "a") vlat_v && VL.leq (VL.string "str") vlat_k &&
				VL.leq (VL.string "b") vlat_v && VL.may_be_number vlat_k &&
				VL.leq (VL.string "c") vlat_v && VL.may_be_bool vlat_k &&
				VL.leq (VL.string "d") vlat_v && VL.leq (VL.builtin VL.Print) vlat_k &&
				VL.leq (VL.string "e") vlat_v && VL.may_be_proc vlat_k &&
				VL.leq (VL.string "f") vlat_v && VL.may_be_table vlat_k ));
    "table16" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/table16.lua" in
			     let vlat_r   = SL.read_name slat "r" in
			     assert_bool "table16" (VL.may_be_strings vlat_r && VL.may_be_nil vlat_r));
    "table17" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/table17.lua" in
			     let vlat_r   = SL.read_name slat "r" in
			     assert_bool "table17" (VL.may_be_strings vlat_r));
    "table18" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/table18.lua" in
			     let vlat_r1  = SL.read_name slat "r1" in
			     let vlat_r2  = SL.read_name slat "r2" in
			     assert_bool "table18" (VL.leq (VL.string "foo") vlat_r1 &&
						    VL.may_be_nil vlat_r2));
    "table19" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/table19.lua" in
			     let vlat_k  = SL.read_name slat "k" in
			     let vlat_v  = SL.read_name slat "v" in
			     assert_bool "table19" (VL.may_be_nil vlat_k && VL.may_be_number vlat_k
						    && VL.may_be_nil vlat_v && VL.may_be_strings vlat_v));
    "table20" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/table20.lua" in
			     let vlat_k  = SL.read_name slat "k" in
			     let vlat_v  = SL.read_name slat "v" in
			     assert_bool "table20" (VL.nil = vlat_k && VL.nil = vlat_v));
    "table21" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/table21.lua" in
			     let vlat_s     = SL.read_name slat "s" in
			     let vlat_keys  = ST.lookup_all_keys slat.SL.store vlat_s in
			     let vlat_props = ST.lookup_all_props slat.SL.store vlat_s in
			     let vlat_def   = ST.lookup_default_prop slat.SL.store vlat_s VL.anystring in
			     assert_bool "table21" (VL.may_be_table vlat_s && vlat_keys = VL.anystring &&
				                    vlcmp vlat_props vlat_def &&
						    VL.leq (VL.string "str") vlat_def &&
						    VL.may_be_number vlat_def &&
						    VL.may_be_bool vlat_def &&
						    VL.leq (VL.builtin VL.Print) vlat_def &&
						    VL.may_be_proc vlat_def &&
						    VL.may_be_table vlat_def));
    "table22" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/table22.lua" in
			     let vlat_s     = SL.read_name slat "s" in
			     let vlat_keys  = ST.lookup_all_keys slat.SL.store vlat_s in
			     let vlat_props = ST.lookup_all_props slat.SL.store vlat_s in
			     let vlat_def   = ST.lookup_default_prop slat.SL.store vlat_s VL.anystring in
			     assert_bool "table22" (VL.may_be_table vlat_s &&
				                    vlcmp vlat_props vlat_def &&
						    VL.leq (VL.string "str") vlat_keys &&
						    VL.may_be_number vlat_keys &&
						    VL.may_be_bool vlat_keys &&
						    VL.leq (VL.builtin VL.Print) vlat_keys &&
						    VL.may_be_proc vlat_keys &&
						    VL.may_be_table vlat_keys &&
						    VL.leq (VL.string "a") vlat_props && 
						    VL.leq (VL.string "b") vlat_props && 
						    VL.leq (VL.string "c") vlat_props &&
						    VL.leq (VL.string "d") vlat_props &&
						    VL.leq (VL.string "e") vlat_props && 
						    VL.leq (VL.string "f") vlat_props ));
    (* table 23 missing *)
    "table24" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/table24.lua" in
			     let vlat_r = SL.read_name slat "r" in
			     assert_bool "table24" (VL.leq (VL.string "hey jude") vlat_r));
  ]


let test_meta = "meta tests" >:::
  [ "meta01" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/meta01.lua" in
			    let vlat_m   = SL.read_name slat "m" in
			    assert_bool "meta01" (VL.may_be_table vlat_m
						  && vlcmp vlat_m (SL.read_name slat "mt")));
    "meta02" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/meta02.lua" in
			    let vlat_yval  = SL.read_name slat "yval" in
			    assert_equal ~cmp:vlcmp vlat_yval VL.number);
    "meta03" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/meta03.lua" in
			    assert_equal ~cmp:slcmp slat SL.bot);
    "meta04" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/meta04.lua" in
			    let vlat_yval  = SL.read_name slat "yval" in
			    assert_equal ~cmp:vlcmp vlat_yval VL.number);
    "meta05" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/meta05.lua" in
			    let vlat_yval  = SL.read_name slat "yval" in
			    assert_equal ~cmp:vlcmp vlat_yval VL.number);
    "meta06" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/meta06.lua" in
			    let vlat_yval  = SL.read_name slat "yval" in
			    assert_equal ~cmp:vlcmp vlat_yval VL.number);
    "meta07" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/meta07.lua" in
			    let vlat_yval  = SL.read_name slat "yval" in
			    assert_equal ~cmp:vlcmp vlat_yval VL.number);
    "meta08" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/meta08.lua" in
			    let vlat_yval  = SL.read_name slat "yval" in
			    assert_equal ~cmp:vlcmp vlat_yval VL.number);
    "meta09" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/meta09.lua" in
			    let vlat_yval  = SL.read_name slat "yval" in
			    assert_equal ~cmp:vlcmp vlat_yval VL.number);
    "meta10" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/meta10.lua" in
			    let vlat_yval  = SL.read_name slat "yval" in
			    assert_equal ~cmp:vlcmp vlat_yval VL.number);
    "meta11" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/meta11.lua" in
			    assert_equal ~cmp:slcmp slat SL.bot);
    "meta12" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/meta12.lua" in
			    let vlat_yval  = SL.read_name slat "yval" in
			    assert_equal ~cmp:vlcmp vlat_yval VL.number);
    "meta13" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/meta13.lua" in
			    let vlat_a,vlat_b,vlat_c =
			      SL.read_name slat "a",SL.read_name slat "b",SL.read_name slat "c" in
			    assert_bool "meta13" (vlcmp vlat_a VL.number && vlcmp vlat_b VL.nil && vlcmp vlat_c VL.nil));
    "meta14" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/meta14.lua" in
			    let vlat_a,vlat_b,vlat_c =
			      SL.read_name slat "a",SL.read_name slat "b",SL.read_name slat "c" in
			    assert_bool "meta14" (vlcmp vlat_a VL.number && vlcmp vlat_b VL.nil && vlcmp vlat_c VL.nil));
    "meta15" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/meta15.lua" in
			    assert_equal ~cmp:slcmp slat SL.bot);
    "meta16" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/meta16.lua" in
			    assert_equal ~cmp:slcmp slat SL.bot);
    "meta17" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/meta17.lua" in
			    assert_bool "meta017" (VL.may_be_table (SL.read_name slat "mt2")));
    "meta18" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/meta18.lua" in
			    assert_bool "meta018" (VL.may_be_nil (SL.read_name slat "mt2")));
    "meta19" >:: (fun () -> let _,_,slat = parse_analyze_lookup "examples/meta19.lua" in
			    assert_bool "meta019" (VL.may_be_nil (SL.read_name slat "mt2")));
 ]

let test_builtin = "builtin tests" >:::
  let s_maybe_number_in prog_name =
    let _,_,slat = parse_analyze_lookup prog_name in
    let sval = SL.read_name slat "s" in
    assert_bool "tonumber" (VL.may_be_number sval) in

  [ "builtin_os_exit"  >:: (fun () -> let _,_,slat =
					parse_analyze_lookup "examples/builtin_os_exit.lua" in
				      assert_bool "os_exit" (slcmp SL.bot slat));
    "builtin_type" >:: (fun () -> let _,_,slat =
				    parse_analyze_lookup "examples/builtin_type.lua" in
				  assert_bool "type"
				    ((VL.leq (VL.string "function") (SL.read_name slat "f")) &&
  				     (VL.leq (VL.string "function") (SL.read_name slat "bf")) &&
				     (VL.leq (VL.string "number") (SL.read_name slat "i")) &&
				     (VL.leq (VL.string "boolean") (SL.read_name slat "b")) &&
				     (VL.leq (VL.string "string") (SL.read_name slat "s")) &&
				     (VL.leq (VL.string "table") (SL.read_name slat "t")) ));
    "builtin_tostring" >:: (fun () -> let _,_,slat =
					parse_analyze_lookup "examples/builtin_tostring.lua" in
				      let sval = SL.read_name slat "s" in
				      assert_bool "tostring" (VL.may_be_strings sval));
    "builtin_tonumber"      >:: (fun () -> s_maybe_number_in "examples/builtin_tonumber.lua");
    "builtin_rawget"        >:: (fun () -> s_maybe_number_in "examples/builtin_rawget.lua" );
    "builtin_rawset"        >:: (fun () -> s_maybe_number_in "examples/builtin_rawset.lua" );
    "builtin_math_sqrt"     >:: (fun () -> s_maybe_number_in  "examples/builtin_math_sqrt.lua");
    "builtin_math_floor"    >:: (fun () -> s_maybe_number_in "examples/builtin_math_floor.lua");
    "builtin_math_random"   >:: (fun () -> s_maybe_number_in "examples/builtin_math_random.lua");
    "builtin_string_sub"    >:: (fun () -> s_maybe_number_in "examples/builtin_string_sub.lua");
    "builtin_string_byte"   >:: (fun () -> s_maybe_number_in "examples/builtin_string_byte.lua");
    "builtin_string_format" >:: (fun () -> s_maybe_number_in "examples/builtin_string_format.lua");
    "builtin_table_concat"  >:: (fun () -> s_maybe_number_in "examples/builtin_table_concat.lua");
  ]
      
let test_warn = "warning tests" >:::
  let unreachable prog_name = 
     (fun () -> let _,_,slat = parse_analyze_lookup prog_name in
		assert_bool prog_name (slcmp SL.bot slat)) in
  [ "warn01" >:: (unreachable "examples/warn01.lua");
    "warn02" >:: (unreachable "examples/warn02.lua");
  ]

let basic_tests =
  [ test_syntax;
    test_simpl;
    test_func;
    test_method;
    test_for;
    test_unop;
    test_string;
    test_coerce;
    test_return;
    test_scope;
    test_table;
    test_meta;
    test_builtin;
    test_warn; ]

let test_programs = "program tests" >:::
  let may_terminate prog_name = 
     (fun () -> let _,_,slat = parse_analyze_lookup prog_name in
		assert_bool prog_name (not (slcmp SL.bot slat))) in
  [ "fac" >::           (fun () -> let _,_,slat = parse_analyze_lookup "examples/fac.lua" in
				   let vlat_r   = SL.read_name slat "r" in
				   assert_equal ~cmp:vlcmp vlat_r VL.number);
    "hello" >::         may_terminate "benchmarks/lua.org/hello.lua";
    "account" >::       may_terminate "benchmarks/lua.org/account.lua";
    "bisect" >::        may_terminate "benchmarks/lua.org/bisect.lua";
    (* from Eric Mertens *)
    "ericmertens1" >::  may_terminate "benchmarks/ericmertens1.lua";
    (* adapted from PPDP13 to Lua 5+ *)
    "ackermann" >::     may_terminate "benchmarks/PPDP13-Lua5/ackermann.lua";
    "ary" >::           may_terminate "benchmarks/PPDP13-Lua5/ary.lua";
    "ary2" >::          may_terminate "benchmarks/PPDP13-Lua5/ary2.lua";
    "ary3" >::          may_terminate "benchmarks/PPDP13-Lua5/ary3.lua";
    "deep_return" >::   may_terminate "benchmarks/PPDP13-Lua5/deep_return.lua";
    "fibo" >::          may_terminate "benchmarks/PPDP13-Lua5/fibo.lua";
    "funcall-count" >:: (fun () ->
                           let _,_,slat = parse_analyze_lookup "benchmarks/PPDP13-Lua5/funcall-count.lua" in
			   assert_equal ~cmp:vlcmp VL.number (SL.read_name slat "c"));
    "hash" >::          (fun () ->
                           let _,_,slat = parse_analyze_lookup "benchmarks/PPDP13-Lua5/hash.lua" in
			   assert_equal ~cmp:vlcmp VL.number (SL.read_name slat "c"));
    "hash2" >::         may_terminate "benchmarks/PPDP13-Lua5/hash2.lua";
    "heapsort" >::      may_terminate "benchmarks/PPDP13-Lua5/heapsort.lua";
    "lists" >::         may_terminate "benchmarks/PPDP13-Lua5/lists.lua";
    "matrix" >::        may_terminate "benchmarks/PPDP13-Lua5/matrix.lua";
    "n_body" >::        may_terminate "benchmarks/PPDP13-Lua5/n_body.lua";
    "nestedloop" >::    (fun () ->
                           let _,_,slat = parse_analyze_lookup "benchmarks/PPDP13-Lua5/nestedloop.lua" in
			   assert_equal ~cmp:vlcmp VL.number (SL.read_name slat "x"));
    "random" >::        may_terminate "benchmarks/PPDP13-Lua5/random.lua";
    "sieve" >::         (fun () ->
                           let _,_,slat = parse_analyze_lookup "benchmarks/PPDP13-Lua5/sieve.lua" in
			   assert_equal ~cmp:vlcmp VL.number (SL.read_name slat "num_prime"));
    "spectral-norm" >:: (fun () ->
                           let _,_,slat = parse_analyze_lookup "benchmarks/PPDP13-Lua5/spectral-norm.lua" in
			   assert_bool "spectral-norm" (vlcmp VL.number (SL.read_name slat "vBv")
							&& vlcmp VL.number (SL.read_name slat "vv")));
    "strcat" >::        may_terminate "benchmarks/PPDP13-Lua5/strcat.lua";
    "strcat2" >::       may_terminate "benchmarks/PPDP13-Lua5/strcat2.lua";
 (* "binary-trees" >::  (fun () ->
                           let _,_,slat = parse_analyze_lookup "benchmarks/benchmarkgame/binary-trees.lua" in
			   assert_bool "binary-trees" ((vlcmp VL.number (SL.read_name slat "maxdepth")) &&
						      (VL.may_be_table (SL.read_name slat "longlivedtree"))));
    "spectral-norm" >:: (fun () ->
                           let _,_,slat = parse_analyze_lookup "benchmarks/benchmarkgame/spectral-norm.lua" in
			   assert_bool "spectral-norm" ((vlcmp VL.number (SL.read_name slat "vBv")) &&
						       (vlcmp VL.number (SL.read_name slat "vv"))));
    "n-body" >::        may_terminate "benchmarks/benchmarkgame/n-body.lua";
    "richards_loop" >:: may_terminate "benchmarks/LuaCLR/richards/richards_loop.lua"; *)
  ]

let functional_tests = [ test_programs; ]

let usagemsg = "Usage: ./test [option]"
let all,basic,functional = ref false, ref false, ref false
let argspec  =
  Arg.align
    [ ("-all",        Arg.Set all,        " Run all tests");
      ("-basic",      Arg.Set basic,      " Run basic tests (default)");
      ("-functional", Arg.Set functional, " Run functional tests"); ]
let _ = 
  begin
    Arg.parse argspec (fun _ -> (Arg.usage argspec usagemsg; exit 1)) usagemsg;
    all := !all || (!basic && !functional);
    if !all
    then run_test_tt_main ("OUnit all testsuite" >::: (basic_tests @ functional_tests))
    else if !functional
    then run_test_tt_main ("OUnit functional testsuite" >::: functional_tests)
    else run_test_tt_main ("OUnit basic testsuite" >::: basic_tests)
  end
