package utexas.sim

import java.util.concurrent.Callable

import utexas.map.{Edge, Turn, Traversable}

import utexas.{Util, cfg}

abstract class Route() {
  // Define these
  def lookahead_step(n: Int): Option[Traversable]
  def request_route(from: Edge, to: Edge): Callable[List[Traversable]]

  // Common to most, but can change
  var steps: List[Traversable] = Nil

  def transition(from: Traversable, to: Traversable) = {
    if (steps.head == to) {
      steps = steps.tail      // moving right along
    } else {
      throw new Exception("We missed a move!")
    }
  }

  def got_route(response: List[Traversable]) = {
    steps = response
  }

  def next_step = lookahead_step(0)
}

// It's all there, all at once... just look at it
class StaticRoute() extends Route() {
  override def lookahead_step(n: Int) = if (n >= steps.size)
                                          None
                                        else
                                          Some(steps(n))
  
  override def request_route(from: Edge, to: Edge) = new Callable[List[Traversable]]() {
    def call(): List[Traversable] = {
      return Agent.sim.pathfind_astar(from, to)
    }
  }
}

// DOOMED TO WALK FOREVER
class DrunkenRoute() extends Route() {
  override def lookahead_step(n: Int): Some[Traversable] = {
    // Lazily fill in steps as we need to
    // TODO write the while with a for so we don't ask size() repeatedly
    while (steps.size <= n) {
      val add = steps.last match {
        case e: Edge => pick_turn(e)
        case t: Turn => t.to
      }
      // pattern matching gets wonky when these are combined directly
      steps :+= add
    }

    // TODO when to stop?

    return Some(steps(n))
  }

  // Schedule empty work! :P
  override def request_route(from: Edge, to: Edge) = new Callable[List[Traversable]]() {
    // just start with where we are.
    def call(): List[Traversable] = List(pick_turn(from))
  }

  // TODO choose a random point, or one leading in general direction of goal?
  def pick_turn(e: Edge) = Util.choose_rand[Turn](e.next_turns)
}
