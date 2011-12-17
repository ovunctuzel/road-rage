package utexas.map.make

class Pass2(old_graph: PreGraph1) {
  val graph = new PreGraph2(old_graph)
  // TODO stuff for the scc's

  def run(): PreGraph2 = {
    // TODO do the rest of Pass 2: removing tiny road parts

    return graph
  }
}
