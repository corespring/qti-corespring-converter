package org.corespring.common.xml

import scala.xml.Node

object NodeWithFinder {

  /**
    * Decorator for Node which adds a method to return all nodes matching a predicate function.
    */
  implicit class NodeWithFinder(node: Node) {

    def matching(predicate: Node => Boolean) = recurse(node, predicate)

    private def recurse(node: Node, predicate: Node => Boolean, matches: Seq[Node] = Seq.empty): Seq[Node] =
      (predicate(node), node.child.nonEmpty) match {
        case (true, true) => matches ++ node ++ node.child.map(recurse(_, predicate)).flatten
        case (false, true) => matches ++ node.child.map(recurse(_, predicate)).flatten
        case (true, false) => Seq(node)
        case (false, false) => Seq.empty
      }

  }

}
