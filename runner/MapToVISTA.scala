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



object MapToVISTA {

  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]): Unit = {

    //Builder.main(Array("osm/austin.osm"))
    
    val graph = Graph.load("maps/austin.map")
    
    val dir_roads = graph.directed_roads

    println("Dir roads: "+dir_roads.length)
    
    val fileout = new PrintWriter(new File("road_data.txt"))

    var count = 0
    
    for (i <- 0 until dir_roads.length)
    {
      try
      {
        val dir_road = dir_roads.apply(i)
        fileout.println(i+"\t"+dir_road.from.id+"\t"+dir_road.to.id+"\t"+dir_road.road.length+"\t"+(dir_road.road.speed_limit/0.44704)+"\t"+dir_road.edges.length+"\t"+dir_road.road.capacity)
        
        val start_loc = Graph.world_to_gps(dir_road.start_pt.x, dir_road.start_pt.y)
        val end_loc = Graph.world_to_gps(dir_road.end_pt.x, dir_road.end_pt.y)
       
        fileout.print("("+start_loc.x+","+start_loc.y+") ("+end_loc.x+","+end_loc.y+")")
       
        fileout.println()
        
        count = count+1
      }
      catch {
        case e: Exception => println(i)
      }     
    }
    
    fileout.close()
   
    
    println("Found "+count+" dir roads")
    
    
    
  }
  
  

}
