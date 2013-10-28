fn main() {
  let a_vector = ~[1, 2, 3];
  let mut another_vector = a_vector;
  another_vector[1] = 4;

  println(fmt!("The second number is %d",another_vector[1]));
}
