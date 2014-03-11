// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.ui

import java.awt.Color
import scala.collection.mutable

import utexas.aorta.map.{Road, CongestionRouter, FreeflowRouter}
import utexas.aorta.sim.Simulation
import utexas.aorta.experiments.ScenarioRoadUsage
import utexas.aorta.common.{Timer, RNG}
import utexas.aorta.common.algorithms.PathResult

trait Visualization {
  // TODO could use "self: OrganizerClass =>" pattern...
  private var state: GuiState = null
  private var canvas: MapCanvas = null
  private def sim(): Simulation = canvas.sim  // TODO whys compiler think this is recursive?

  def setup_viz(s: GuiState, c: MapCanvas) {
    state = s
    canvas = c
  }

  def show_pathfinding(from: Road, to: Road) {
    val timer = Timer("Pathfinding")
    // TODO Show each type of route in a different color...
    val colors = List(Color.CYAN, Color.RED, Color.BLUE, Color.GREEN, Color.YELLOW)
    val routers = List(new CongestionRouter(sim.graph))

    for ((router, color) <- routers.zip(colors)) {
      val route = router.path(from, to).path
      //route.foreach(step => println("  - " + step))
      println(s"for $color, we have $route")
      // TODO a hack, I want to name it properly!
      //state.road_colors.set_layer(s"${router.router_type} pathfinding")
      state.road_colors.add_layer("route")  // to get bold roads
      route.foreach(r => state.road_colors.set("route", r, color))
      state.set_cur_layer("route")
    }
    timer.stop()
    canvas.repaint()
  }

  // percentile is [0, 1]
  def show_heatmap(costs: Map[Road, Double], percentile: Double, layer: String) {
    // Exclude the top percent of costs; they're usually super high and throw off the visualization
    val sorted_costs = costs.values.toArray.sorted
    val max_cost = sorted_costs(math.min(
      sorted_costs.size - 1, (percentile * sorted_costs.size).toInt
    ))
    val heatmap = new Heatmap()
    state.road_colors.add_layer(layer)
    for ((r, cost) <- costs) {
      // Cap at 1 since we may chop off some of the largest values
      state.road_colors.set(layer, r, heatmap.color(math.min(cost / max_cost, 1.0)))
    }
    state.set_cur_layer(layer)
  }

  // Hardcoded to show first component of 2-tuple cost
  def show_path_costs(result: PathResult, percentile: Double = .99) {
    show_heatmap(result.costs.mapValues(_._1), percentile, "path costs")
    for (r <- result.path) {
      state.road_colors.set("path costs", r, Color.GREEN)
    }
    canvas.repaint()
  }

  def show_tolls(percentile: Double = .99) {
    val costs = sim.graph.roads.map(r => r -> r.road_agent.tollbooth.toll(sim.tick).dollars).toMap
    show_heatmap(costs, percentile, "tolls")
    canvas.repaint()
  }

  def show_road_usage(percentile: Double = .99) {
    for (fn <- canvas.prompt_fn("Select a road_usage.gz metric from an experiment")) {
      val metric = ScenarioRoadUsage(fn)
      // TODO delta btwn baseline and others. how to handle negatives?
      for (mode <- metric.usages_by_mode.keys) {
        show_heatmap(
          metric.usages_by_mode(mode).map(r => sim.graph.get_r(r.r) -> r.num_drivers.toDouble).toMap,
          percentile, s"road usage in $mode (number of drivers)"
        )
        show_heatmap(
          metric.usages_by_mode(mode).map(r => sim.graph.get_r(r.r) -> r.sum_priority.toDouble).toMap,
          percentile, s"road usage in $mode (sum of driver priority)"
        )
      }
      canvas.repaint()
    }
  }

  def show_pagerank() {
    val iterations = 50
    val alpha = .15
    val graph = sim.graph
    // TODO seed initial_weight with our own notion of importance?
    val initial_weight = 1.0 / graph.roads.size
    val rank_source = alpha / graph.roads.size
    var rank = graph.roads.map(r => r -> initial_weight).toMap
    for (i <- Range(0, iterations)) {
      println(s"Computing PageRank, iteration $i of $iterations...")
      val new_rank = graph.roads.map(r => r ->
        (rank_source + (1.0 - alpha) * r.preds.map(pred => rank(pred) / pred.succs.size).sum)
      ).toMap
      val sum_ranks = new_rank.values.sum
      rank = new_rank.mapValues(x => x / sum_ranks)
    }
    show_heatmap(rank, 1.0, "PageRank")
    canvas.repaint()
  }

  def show_popular_roads() {
    val num_routes = 3000
    val router = new FreeflowRouter(sim.graph)
    val rng = new RNG()
    val frequency = new mutable.HashMap[Road, Double]()
    sim.graph.roads.foreach(r => frequency(r) = 0)
    for (i <- Range(0, num_routes)) {
      if (i % 500 == 0) {
        println(s"Visualizing popular roads, $i / $num_routes...")
      }
      for (r <- router.path(rng.choose(sim.graph.roads), rng.choose(sim.graph.roads)).path) {
        frequency(r) += 1
      }
    }
    show_heatmap(frequency.toMap, 1.0, "Popularity for uniformly distributed paths")
    canvas.repaint()
  }
}
