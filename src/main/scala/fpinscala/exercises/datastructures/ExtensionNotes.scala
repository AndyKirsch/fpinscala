package fpinscala.exercises.datastructures

class ExtensionNotes {
  extension (t: Tree[Int]) def first: Int = t match
    case Leaf(i) => i
    case Branch(l,_) => l.first
}
