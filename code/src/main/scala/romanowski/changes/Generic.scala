package romanowski.changes

import  scala.language.higherKinds

/**
  * Author: Krzysztof Romanowski
  */
trait Dep

class Generic[E[V] <: Seq[V with Dep]]
