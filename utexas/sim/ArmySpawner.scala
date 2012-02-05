package utexas.sim

import java.util.concurrent.{ExecutorService, Executors}

import utexas.map.Edge

import utexas.Util

class ArmySpawner(pool_size: Int, num_agents: Int, sim: Simulation, dynamic: Boolean)
  extends AnyRef with Runnable
{
  val pool = Executors.newFixedThreadPool(pool_size)

  def run() = {
    (0 until num_agents).foreach(i => pool.execute(new Spawner(sim, dynamic, i + 1, num_agents)))
    pool.shutdown
  }
}

class Spawner(sim: Simulation, dynamic: Boolean, agent_num: Int, agent_total: Int)
  extends AnyRef with Runnable
{
  def run() = {
    sim.random_edge(spawning = true, dynamicSpawning = dynamic) match {
      case Some(e) => {
        Util.log("Spawning agent " + agent_num + "/" + agent_total)
        sim.add_agent(e)
      }
      case None => Util.log("No room left for agent " + agent_total + "/" + agent_total)
    }
  }
}
