/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package runner

import utexas.aorta.map.make.{Builder, Preroute}
import utexas.aorta.ui.GUI
import utexas.aorta.map.Graph
import utexas.aorta.{Util, RNG}
import utexas.aorta.analysis.PostProcess
import utexas.aorta.sim.{Scenario, MkSpecificPathRoute, IntersectionDistribution, IntersectionType, MkAgent, SystemWalletConfig, MkWallet, WalletType, OrderingType}

import java.io.{IOException, PrintWriter, File}
import java.util.Scanner
import scala.collection.mutable.HashMap



object Main {

  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]): Unit = {
    // *** experiment config ***
    
    // default values for wallet_config, will change later for experiments  
    val wallet_config = SystemWalletConfig(7, 25, 5, 2, 1, 5)
    val useAuction = false
    
    //Builder.main(Array("osm/austin.osm"))
    //MapToVISTA.main(args)
    
    
    //GUI.main(Array("maps/austin.map", "--log", "logs/test"))
    
    
    
    
    val graph = Graph.load("maps/austin.map");
    
    // *** Load link TT data ***
    var filein = new Scanner(new File("link_tt.txt"))
    
    val interval = filein.nextInt()
    val sim_max = filein.nextInt();
    val num_intervals = filein.nextInt();
    
    while(filein.hasNext())
    {
      val road_id = filein.nextInt();
      
      val td_costs = new Array[Double](num_intervals)
      
      for(i <- 0 until num_intervals)
      {
        td_costs.update(i, filein.nextDouble())
      }
      
      graph.directed_roads.apply(road_id).updateCosts(td_costs, interval)
    }
    
    filein.close()
    
    
    
    
    
    
    
    // *** Load routes *** 
    // read routes into map
    // format: id size {directed_roads}
    filein =  new Scanner(new File("routes.txt"))
    
    var route_lookup = HashMap[Int, List[Int]]()
    
    
    while(filein.hasNext())
    {
      val id = filein.nextInt()
      val size = filein.nextInt()
      
      var path : List[Int] = List[Int]()
      
      for( i <- 0 until size)
      {
        path = path :+ filein.nextInt()
      }
      
      route_lookup.put(id, path)  
    }

    filein.close();
    
    println(route_lookup.size)
     
      
      
    
    val rng = new RNG(1000)
    
    // creating graph from map file
    
    
    
    
    println(graph.directed_roads.length)
    
    
    
    
 
    
    
    // *** Vehicle input ***
    
    // read vehicles and route assignments
    // format: id dep_time route_id budget
    filein = new Scanner(new File("agents.txt"))
    val size = filein.nextInt();
    
    val agent_array = new Array[MkAgent](size)
   
    
    for(i <- 0 until size)
    {
      // read vehicle data
      val id = filein.nextInt()
      val dep_time = filein.nextInt()
      val route_id = filein.nextInt()
      val budget = filein.nextInt()
      
      filein.nextLine()
            
      val seed = i
      
      
      // read list of roads and create new route from it

      
      val path_roads = route_lookup.get(route_id).get
      val mk_route = new MkSpecificPathRoute(path_roads, i)
      
      // starting edge id is first edge on first road on path
      val start_road = path_roads.apply(0)
      val start_edge = graph.directed_roads.apply(start_road).edges.head
      val start_edge_id = start_edge.id
      
      // get spawn distance
      //val start_dist = start_edge.safe_spawn_dist(rng)
      val start_dist = 0;
      
      // driver wallet - spends proportionally per intersection
      var wallet = MkWallet(WalletType.Fair, budget, 1)


      // create agent  
      val agent = MkAgent(id, dep_time, seed, start_edge_id, start_dist, mk_route, wallet)
      agent_array.update(i, agent)
      
    }
    
    
    
    // *** System config ***

    // intersection ordering: auction or FIFO
    var intersection_array = 
    (  if(useAuction)
    {
      IntersectionDistribution.realistic(graph).map(i => i.copy(ordering = OrderingType.Auction))
    } 
    else
    {
      IntersectionDistribution.realistic(graph).map(i => i.copy(ordering = OrderingType.FIFO))
    } )
    
    
    // *** create and save scenario ***
    val scenario = Scenario("scenarios/austin_ctrl", "maps/austin.map", agent_array, intersection_array, wallet_config)
    scenario.save();
    
    
    // *** to load and run scenario ***
    GUI.main( Array("scenarios/austin_ctrl", "--log", "logs/austin_ctrl") )
    
    
   
    // *** Process travel times after ***
    //PostProcess.main(Array("logs/austin_ctrl"))
    
  }

}
