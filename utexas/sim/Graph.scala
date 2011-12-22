package utexas.sim

import utexas.map.{Raw_Graph, Road, Edge, Vertex}
import utexas.map.make.Reader

import utexas.Util.{log, log_push, log_pop}

// TODO better name, i promise

// This just adds a notion of agents
class Graph(roads: List[Road], override val edges: List[Edge with Queue_of_Agents],
            vertices: List[Vertex], width: Double, height: Double)
  extends Raw_Graph(roads, edges, vertices, width, height)
{
}

object Graph {
  def load(fn: String) = (new Reader(fn)).load_with_agents
}
