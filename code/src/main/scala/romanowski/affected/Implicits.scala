package romanowski.affected

/**
  * Author: Krzysztof Romanowski
  */
class Implicits(i: Int) {
  implicit class FooBarOps(from: FooImpl[_]){
    def fooBar = 1
  }
}
