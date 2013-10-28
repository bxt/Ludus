trait Monster {
  fn attack(&self);
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
}

impl Monster for RubyGargoyle {
  fn attack(&self) {
    println(fmt!("The ruby gargoyle attacks for %d",self.strength))
  }
}

impl Monster for TigerChimera {
  fn attack(&self) {
    println(fmt!("The tiger chimera attacks for %d",self.strength))
  }
}

fn monsters_attack(monsters: &[~Monster]) {
  for m in monsters.iter() {
    m.attack();
  }
}

fn main() {
  let monkey   = ~SpiderMonkey {legs: 8};
  let gargoyle = ~RubyGargoyle {strength: 15};
  let chimera  = ~TigerChimera {strength: 17};

  let monsters = [monkey as ~Monster, gargoyle as ~Monster, chimera as ~Monster];
  monsters_attack(monsters);
}
