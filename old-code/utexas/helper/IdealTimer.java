package utexas.helper;

/**
 * Calculate the ideal time across a distance (if we take the perfect route and never have to stop at
 * or slow down for intersections
 * (Ripped from Graph.calcTime method)
 * @author Mike Depinet
 * @version 1.0
 */
public class IdealTimer {
	public static void main(String[] args){
		//Values for test route from 30.2837973N, 97.7418975W to 30.2629241N, 97.7447225W
		//							 Guadalupe around 21st		 South Congress around Caesar Chavez
		double dist = 2642.9208; //m
		double curVeloc = 0.; //Start at 0 m/s
		double speedLimit = 13.4112; //30mph in m/s
		double acceleration = 2.7; //m/s^2
		
        double curVel = curVeloc;
        double t1 = (speedLimit - curVel) / acceleration;
        double xmid = curVel*t1 + (1./2.)*acceleration*t1*t1;
        double t2 = (dist-xmid)/speedLimit;
        t1 = (t2 > 0) ? t1 : (Math.sqrt(curVel*curVel - 2*(-1)*dist*acceleration)-curVel)/acceleration;
        curVel = (t2 > 0) ? speedLimit : curVel + acceleration*t1; //Update velocity
        System.out.println((t2 > 0) ? "" + (t1 + t2) : "" + t1);
	}
}
