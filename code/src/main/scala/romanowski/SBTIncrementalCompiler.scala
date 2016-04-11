package romanowski

import java.io.File


object SBTIncrementalCompiler {

  case class Compilation(allSources: Seq[File], output: File, options: Seq[String])

  case class API(source: File, hash: Hash)

  case class NamedAPI(source: File, hashes: Map[String,Hash])



  def compile(compilation: Compilation): Unit ={
    var changed = changedSources(compilation)

    while(changed.nonEmpty){
      val oldAPIs = changed.map(currentAPI)
      val newAPIs = doCompile(changed)

      val changedAPIs = diff(oldAPIs, newAPIs)

      changed = changedAPIs.flatMap(allDependecies)
    }
  }

  def changedSources(compilation: Compilation): Seq[File] = {
    def outputChanged = compilation.output != previousCompilation.output
    def optionsChanged = compilation.output != previousCompilation.output

    if(outputChanged || optionsChanged) // Then compile all
      compilation.allSources
    else // Compile only changed files
      compilation.allSources.filter(changedSource)
  }



  private def doCompile(source: Seq[File]): Seq[API] = {
    val listener = new SourceListener

    runScalac(listener)

    updateDependecies(listener.dependencies)

    listener.apis
  }

  class SourceListener{
    private var _apis: Set[API] = Set.empty
    private var _dependencies: Map[File, Set[File]] = Map.empty

    def process(unit: CompilationUnit): Unit = {

      // Compute hashes
      val unitTraverser = new HashingTraverser
      unitTraverser.traverse(unit.tree)

      _apis +=  API(unit.source, unitTraverser.unitHash)

      // Compute dependencies
      val depsTraverser = new DepsTraverser
      depsTraverser.traverse(unit.tree)

      _dependencies += (unit.source -> depsTraverser.dependecies)
    }

    def apis: Seq[API] = _apis.toSeq
    def dependencies: Map[File, Set[File]] = _dependencies
  }

  trait TreeTraverser {
    def traverse(tree: Tree): Unit
  }

  class HashingTraverser extends TreeTraverser {
    var unitHash = EmptyHash

    override def traverse(tree: Tree): Unit = tree match{
      case ClassTree(signature, members) =>
        unitHash.hash(signature)
        members.foreach(traverse)
      case  Method(signature, body) =>
        unitHash.hash(signature)
        traverse(body)
      //  ...
    }
  }

  class DepsTraverser extends TreeTraverser {
    var dependecies: Set[File] = Set.empty

    override def traverse(tree: Tree): Unit = tree match{
      case ClassTree(signature, members) =>
        dependecies += signature.declaredIn
        members.foreach(traverse)
      case  Method(signature, body) =>
        dependecies += signature.declaredIn
        traverse(body)
      case Operation(tpe, _) =>
        dependecies += tpe.declaredIn
      // ...
    }
  }


  trait Tree

  case class ClassTree(signature: Type, members: Seq[Tree])

  case class Type(signature: String){
    def declaredIn: File = ???
  }

  case class Method(signature: Type, body: Tree)

  case class Operation(from: Type, code: String)

  case class CompilationUnit(source: File, tree: Tree)

  def updateDependecies(deps: Map[File, Set[File]]): Unit = ???

  def runScalac(listner: SourceListener) = ???

  def changedSource(file: File): Boolean = ???

  def hash(v: Any): Hash = ???

  def aggregate(hashes: Seq[Hash]): Hash = ???

  def currentAPI(source: File): API = ???

  def previousCompilation: Compilation = ???

  def diff(oldApis: Seq[API], newApis: Seq[API]): Seq[File] = ???

  def allDependecies(file: File): Seq[File] = ???

  class Hash{
    def hash(a: Any): Hash = ???
  }

  val EmptyHash: Hash = ???
}
