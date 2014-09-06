open OUnit2

let suite = "Test ppx_yojson" >::: []

let _ =
  run_test_tt_main suite
