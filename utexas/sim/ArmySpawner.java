package utexas.sim;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import scala.Option;
import utexas.Util;
import utexas.map.Edge;

public class ArmySpawner implements Runnable {
	private final ExecutorService pool;
	private final int numAgents;
	private final Simulation sim;
	private final boolean dynamic;

   public ArmySpawner(int poolSize, int numAgents, Simulation sim, boolean dynamic) {
     pool = Executors.newFixedThreadPool(poolSize);
     this.numAgents = numAgents;
     this.sim = sim;
     this.dynamic = dynamic;
   }

   public void run() {
	   for (int i = 1; i<=numAgents; i++){
		   pool.execute(new Spawner(sim, dynamic, i, numAgents));
	   }
	   pool.shutdown();
   }
}

class Spawner implements Runnable{
	private final Simulation sim;
	private final boolean dynamic;
	private final int agentNum, agentTotal; //Used for printing
	
	public Spawner(Simulation sim, boolean dynamic, int agentNum, int agentTotal){
		this.sim = sim;
		this.dynamic = dynamic;
		this.agentNum = agentNum;
		this.agentTotal = agentTotal;
	}
	@Override
	public void run() {
		Option<Edge> opt = sim.random_edge(null, true,1.0,dynamic);
		if (opt.isDefined()){
			Edge e = opt.orNull(null);
			Util.log("Spawning agent "+agentNum+"/"+agentTotal);
			sim.add_agent(e);
		}
	}
}