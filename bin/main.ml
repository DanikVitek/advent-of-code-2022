let () =
  print_endline "Day 1:";
  Printf.printf "Part 1: %d\n" (Advent_of_code.Day1.process1 "./input/day1/actual.txt");
  Printf.printf "Part 2: %d\n" (Advent_of_code.Day1.process2 "./input/day1/actual.txt");
  print_newline ()
;;

let () =
  print_endline "Day 2:";
  List.iteri
    (fun i process -> Printf.printf "Part %d: %d\n" i (process "./input/day2/actual.txt")) 
    [Advent_of_code.Day2.process1; Advent_of_code.Day2.process2];
  print_newline ()
;;

let () =
  print_endline "Day 3:";
  Printf.printf "Part 1: %d\n" (Advent_of_code.Day3.process1 "./input/day3/actual.txt");
  Printf.printf "Part 2: %d\n" (Advent_of_code.Day3.process2 "./input/day3/actual.txt");
  print_newline ()
;;

let () =
  print_endline "Day 4:";
  Printf.printf "Part 1: %d\n" (Advent_of_code.Day4.process1 "./input/day4/actual.txt");
  Printf.printf "Part 2: %d\n" (Advent_of_code.Day4.process2 "./input/day4/actual.txt");
  print_newline ()
;;

let () =
  print_endline "Day 5:";
  Printf.printf "Part 1: %s\n" (Advent_of_code.Day5.process1 "./input/day5/actual.txt");
  Printf.printf "Part 2: %s\n" (Advent_of_code.Day5.process2 "./input/day5/actual.txt");
  print_newline ()
;;

let () =
  print_endline "Day 6:";
  Printf.printf "Part 1: %d\n" (Advent_of_code.Day6.process1 "./input/day6/actual.txt");
  (* Printf.printf "Part 2: %d\n" (Advent_of_code.Day6.process2 "./input/day6/actual.txt"); *)
  print_newline ()
;;