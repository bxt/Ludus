fn is_fifteen(num: int) -> bool {
  is_three(num) && is_five(num)
}

fn is_three(num: int) -> bool {
  num % 3 == 0
}

fn is_five(num: int) -> bool {
  num % 5 == 0
}

#[test]
fn test_is_three_with_not_three() {
  assert!(!is_three(1));
}

#[test]
fn test_is_three_with_three() {
  assert!(is_three(3));
}

#[test]
fn test_is_five_with_not_five() {
  assert!(!is_five(1));
}

#[test]
fn test_is_five_with_five() {
  assert!(is_five(5));
}


fn main() {
  for num in range(1, 101) {
    println(
      if is_fifteen(num) { ~"FizzBuzz" }
      else if is_three(num) { ~"Fizz" }
      else if is_five(num) { ~"Buzz" }
      else { num.to_str() });
  }
}
