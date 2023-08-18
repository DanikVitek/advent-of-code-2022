module To_test = struct
  let (day1_part1: string -> int) = Advent_of_code.Day1.process1
  let (day1_part2: string -> int) = Advent_of_code.Day1.process2
  let (day2_part1: string -> int) = Advent_of_code.Day2.process1
  let (day2_part2: string -> int) = Advent_of_code.Day2.process2
  let (day3_part1: string -> int) = Advent_of_code.Day3.process1
  let (day3_part2: string -> int) = Advent_of_code.Day3.process2
  let (day4_part1: string -> int) = Advent_of_code.Day4.process1
  let (day4_part2: string -> int) = Advent_of_code.Day4.process2
  let (day5_part1: string -> string) = Advent_of_code.Day5.process1
  let (day5_part2: string -> string) = Advent_of_code.Day5.process2
  let (day6_part1: string -> int option) = Advent_of_code.Day6.process1
  let (day6_part2: string -> int option) = Advent_of_code.Day6.process2
  let (day7_part1: string -> int) = Advent_of_code.Day7.process1
end

let test_day1_part1 () =
  Alcotest.(check int) "same int" 24000 (To_test.day1_part1 "../../../input/day1/test.txt")
;;
let test_day1_part2 () =
  Alcotest.(check int) "same int" 45000 (To_test.day1_part2 "../../../input/day1/test.txt")
;;
let test_day2_part1 () =
  Alcotest.(check int) "same int" 15 (To_test.day2_part1 "../../../input/day2/test.txt")
;;
let test_day2_part2 () =
  Alcotest.(check int) "same int" 12 (To_test.day2_part2 "../../../input/day2/test.txt")
;;
let test_day3_part1 () =
  Alcotest.(check int) "same int" 157 (To_test.day3_part1 "../../../input/day3/test.txt")
;;
let test_day3_part2 () =
  Alcotest.(check int) "same int" 70 (To_test.day3_part2 "../../../input/day3/test.txt")
;;
let test_day4_part1 () =
  Alcotest.(check int) "same int" 2 (To_test.day4_part1 "../../../input/day4/test.txt")
;;
let test_day4_part2 () =
  Alcotest.(check int) "same int" 4 (To_test.day4_part2 "../../../input/day4/test.txt")
;;
let test_day5_part1 () =
  Alcotest.(check string) "same string" "CMZ" (To_test.day5_part1 "../../../input/day5/test.txt")
;;
let test_day5_part2 () =
  Alcotest.(check string) "same string" "MCD" (To_test.day5_part2 "../../../input/day5/test.txt")
;;
let test_day6_part1 () =
  Alcotest.(check (option int)) "same int" (Some 7) (To_test.day6_part1 "../../../input/day6/test1.txt");
  Alcotest.(check (option int)) "same int" (Some 5) (To_test.day6_part1 "../../../input/day6/test2.txt");
  Alcotest.(check (option int)) "same int" (Some 6) (To_test.day6_part1 "../../../input/day6/test3.txt");
  Alcotest.(check (option int)) "same int" (Some 10) (To_test.day6_part1 "../../../input/day6/test4.txt");
  Alcotest.(check (option int)) "same int" (Some 11) (To_test.day6_part1 "../../../input/day6/test5.txt");
;;
let test_day6_part2 () =
  Alcotest.(check (option int)) "same int" (Some 19) (To_test.day6_part2 "../../../input/day6/test1.txt");
  Alcotest.(check (option int)) "same int" (Some 23) (To_test.day6_part2 "../../../input/day6/test2.txt");
  Alcotest.(check (option int)) "same int" (Some 23) (To_test.day6_part2 "../../../input/day6/test3.txt");
  Alcotest.(check (option int)) "same int" (Some 29) (To_test.day6_part2 "../../../input/day6/test4.txt");
  Alcotest.(check (option int)) "same int" (Some 26) (To_test.day6_part2 "../../../input/day6/test5.txt");
;;
let test_day7_part1 () =
  Alcotest.(check int) "same int" 95437 (To_test.day7_part1 "../../../input/day7/test.txt")
;;

let () =
  let open Alcotest in
  run "Basic inputs" [
    "day1-part1", [ test_case "Day1 Part1" `Quick test_day1_part1 ];
    "day1-part2", [ test_case "Day1 Part2" `Quick test_day1_part2 ];
    "day2-part1", [ test_case "Day2 Part1" `Quick test_day2_part1 ];
    "day2-part2", [ test_case "Day2 Part2" `Quick test_day2_part2 ];
    "day3-part1", [ test_case "Day3 Part1" `Quick test_day3_part1 ];
    "day3-part2", [ test_case "Day3 Part2" `Quick test_day3_part2 ];
    "day4-part1", [ test_case "Day4 Part1" `Quick test_day4_part1 ];
    "day4-part2", [ test_case "Day4 Part2" `Quick test_day4_part2 ];
    "day5-part1", [ test_case "Day5 Part1" `Quick test_day5_part1 ];
    "day5-part2", [ test_case "Day5 Part2" `Quick test_day5_part2 ];
    "day6-part1", [ test_case "Day6 Part1" `Quick test_day6_part1 ];
    "day6-part2", [ test_case "Day6 Part2" `Quick test_day6_part2 ];
    "day7-part1", [ test_case "Day7 Part1" `Quick test_day7_part1 ];
  ]
;;