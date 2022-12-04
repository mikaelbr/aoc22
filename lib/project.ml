let luke1_1 () =
  Timer.run "Luke 1-2" Luke1_2.run
    (Read.read_int_file "./files/luke1/actual.txt")

let luke2_1 () =
  Timer.run "Luke 2-1" Luke2_1.run (Read.read_file "./files/luke2/actual.txt")

let luke3_1 () =
  Timer.run "Luke 3-1" Luke3_1.run (Read.read_file "./files/luke3/actual.txt")

let luke3_2 () =
  Timer.run "Luke 3-2" Luke3_2.run (Read.read_file "./files/luke3/actual.txt")

let luke4_1 () =
  Timer.run "Luke 4-1" Luke4_1.run (Read.read_file "./files/luke4/actual.txt")

let luke4_2 () =
  Timer.run "Luke 4-2" Luke4_2.run (Read.read_file "./files/luke4/actual.txt")

let run_all () =
  luke1_1 ();
  luke2_1 ();
  luke3_1 ();
  luke3_2 ();
  luke4_1 ();
  luke4_2 ()
