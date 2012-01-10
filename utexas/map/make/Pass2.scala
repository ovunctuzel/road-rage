package utexas.map.make

// This is a totally useless class now

class Pass2(old_graph: PreGraph1) {
  val graph = new PreGraph2(old_graph)

  def run(): PreGraph2 = {
    return graph
  }
}
