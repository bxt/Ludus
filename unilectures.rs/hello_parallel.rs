fn main() {
  do 10.times {
    do spawn {
      let hello_message = "Hello!?";
      println(hello_message);
    }
  }
}
