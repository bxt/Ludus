trait Monster {
  fn attack(&self);
  fn new() -> Self;
}

struct SpiderMonkey {
  legs: int
}

struct RubyGargoyle {
  strength: int
}

struct TigerChimera {
  strength: int
}

impl Monster for SpiderMonkey {
  fn attack(&self) {
    println(fmt!("The spider monkey attacks for %d",self.legs*2))
  }
  fn new() -> SpiderMonkey { SpiderMonkey {legs: 8} }
}

impl Monster for RubyGargoyle {
  fn attack(&self) {
    println(fmt!("The ruby gargoyle attacks for %d",self.strength))
  }
  fn new() -> RubyGargoyle {RubyGargoyle {strength: 15} }
}

impl Monster for TigerChimera {
  fn attack(&self) {
    println(fmt!("The tiger chimera attacks for %d",self.strength))
  }
  fn new() -> TigerChimera { TigerChimera {strength: 17} }
}

fn monsters_attack(monsters: &[~Monster]) {
  for m in monsters.iter() {
    m.attack();
  }
}

fn main() {
  let monkey:   ~SpiderMonkey = ~Monster::new();
  let gargoyle: ~RubyGargoyle = ~Monster::new();
  let chimera:  ~TigerChimera = ~Monster::new();

  let monsters = [monkey as ~Monster, gargoyle as ~Monster, chimera as ~Monster];
  monsters_attack(monsters);
}
