let test (module State_monad : State_monad.Interface.Int_state_monad) =
  let open State_monad in
  let open Let_syntax in
  let add_one =
    let%bind current = get in
    let%bind () = set (current + 1) in
    return ()
  in
  let add_three =
    let%bind () = add_one in
    let%bind () = add_one in
    let%bind () = add_one in
    return ()
  in
  let (), s = run add_three 1 in
  assert (s = 4)
;;

let () =
  test (module State_monad.Improvized);
  test (module State_monad.Canonical)
;;
