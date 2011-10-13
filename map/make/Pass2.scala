package map.make

class Pass2(old_graph: PreGraph1) {
  val graph = new PreGraph2(old_graph)
  // TODO stuff for the scc's

  def run(): PreGraph2 = {
    // TODO do the rest of Pass 2: removing tiny road parts, tarjan's algo for
    // SCCs, nixing disconnected portions of the graph
    // TODO actually, in pass 3 once we're directed

    return graph
  }
}
