/*
 * AORTA: Approximately Orchestrated Road Traffic Automata
 * Copyright (C) 2011 UT-Austin & Austin Robot Technology, Dustin Carlino, Mike Depinet
 * License: Modified BSD Software License
 */

package utexas.map;


// Represents something in R². (well, as much as doubles imitate reals)
public class Coordinate  {
    public double x, y;

    /**
     * 
     * Full Constructor
     * 
     * @param lon
     * @param lat
     * 
     */
    public Coordinate (double lon, double lat) {
        x = lon;
        y = lat;
    }

    /**
     * 
     * Cloning Constructor
     * 
     * @param pt
     */
    public Coordinate (Coordinate pt) {
        x = pt.x;
        y = pt.y;
    }

    /**
     * 
     * Overriding toString method
     * to print (x, y)
     * 
     */
    
    public String toString () {
        return "(" + x + ", " + y + ")";
    }

    
    
    public double distTo (Coordinate other) {
        return Math.sqrt( Math.pow(x - other.x, 2.0) + Math.pow(y - other.y, 2.0) );
    }

    @Override public boolean equals (Object rhs) {
        Coordinate other = (Coordinate) rhs;
        return other.x == x && other.y == y;
    }

    @Override public int hashCode () {
        Double xx = x;
        Double yy = y;
        return xx.hashCode() + 31 * yy.hashCode();
    }

    public Coordinate subtract (Coordinate operand) {
        return new Coordinate(x - operand.x, y - operand.y);
    }

    public double dot (Coordinate with) {
        return (x * with.x) + (y * with.y);
    }

    public double mag () {
        return Math.sqrt(x*x + y*y);
    }

    // in radians, 0 to pi
    public double angleBtwn (Coordinate vector) {
        return Math.acos(this.dot(vector) / (this.mag() * vector.mag()));
    }

    public double difference (Coordinate with) {
        return subtract(with).mag();
    }

    // TODO actually implement compare :p
    public boolean lessThan (Coordinate with) {
        return (x < with.x) || (y < with.y);
    }

    public static double getDistanceInMeters(Coordinate c1, Coordinate c2){
        if (c1.equals(c2)) {
            // This otherwise produces NaN. But this shouldn't happen... means some edge
            // has a repeat point. TODO figure out whats going on!
            return 0;
        }

    	//TODO This could probably be more accurate if we paid closer attention to the floating point representations.  The math is sound at least though.
    	double lon1 = Math.toRadians(c1.x);//Convert to radians
    	double lon2 = Math.toRadians(c2.x);
    	double lat1 = Math.toRadians(c1.y) + Math.PI/2.0; //I want 0 at the North Pole, not the equator
    	double lat2 = Math.toRadians(c2.y) + Math.PI/2.0;
    	final double r = 6378100.; //Radius of earth in meters
    	
    	//Convert to xyz coordinates
    	double x1 = Math.sin(lat1)*Math.cos(lon1); //Since all we really want is the angle between these, r shouldn't matter
    	double y1 = Math.sin(lat1)*Math.sin(lon1);
    	double z1 = Math.cos(lat1);
    	double x2 = Math.sin(lat2)*Math.cos(lon2);
    	double y2 = Math.sin(lat2)*Math.sin(lon2);
    	double z2 = Math.cos(lat2);
    	
    	double dotProd = x1*x2+y1*y2+z1*z2;
    	//double len1 = Math.hypot(x1, Math.hypot(y1,z1));//These better both be 1.0 anyway
    	//double len2 = Math.hypot(x2, Math.hypot(y2,z2));
    	
    	double radTheta = Math.acos(dotProd/*/(len1*len2)*/); //Don't need to handle +pi cases because if theta > pi then we should go the other direction around Earth
        double result = r * radTheta; // s = r*theta

        // this occurs when c1 == c2
        //if (Double.isNaN(result)) {
        //    // TODO this came up during a test run... total_dist from Agent's changedEdge()
        //    dbug("ERR: dist in meters btwn %s and %s was NaN", c1, c2);
        //}

        return result;
	}
    
    public static void main(String[] args){
    	System.out.println("ACES to RLM in meters: "+getDistanceInMeters(new Coordinate(30.28685,-97.73659),new Coordinate(30.28892,-97.73634)));
    }
};
