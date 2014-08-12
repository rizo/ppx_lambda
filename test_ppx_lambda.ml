

(* 1-arity lambdas. *)
let inc = x => x + 1
let ignore = (x) => 42
let hello = name => "Hello, " ^ name ^ "!"


let test () =
    assert (inc 1 = 2);
    assert (inc 101 = 102);
    assert (inc (-1) = 0);
    assert (ignore 987 = 42);
    assert (ignore true = 42);
    assert (ignore () = 42);
    assert (hello "Bob" = "Hello, Bob!");
    assert (((x => x + 1) 1) = 2)

let demo () =
    print_endline ("inc 1 = " ^ string_of_int (inc 1));
    print_endline ("ignore 5 = " ^ string_of_int (ignore 5));
    print_endline ("hello \"Bob\" = " ^ hello "Bob")


let () =
    test ();
    demo ()