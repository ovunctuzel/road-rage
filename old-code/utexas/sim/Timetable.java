/**
 * AORTA: Approximately Orchestrated Road Traffic Automata
 * Copyright (C) 2011 UT-Austin & Austin Robot Technology, Dustin Carlino, Mike Depinet
 * License: Modified BSD Software License
 */

package utexas.sim;

import java.util.LinkedList;

import utexas.map.Turn;

import static utexas.helper.Log.dbug;


/**
 * Timetable manages turn events scheduled at a vertex.
 * This is responsible for making sure cars only traverse intersections when they're allowed to.
 * @author Mike Depinet
 * @version 1.0
 */
public class Timetable {

	Scheduler<IntersectionReservation> schedule = new SchedulingTree();

	/**
	 * Default constructor.  The Scheduler is empty
	 */
    public Timetable () {}

    /**
     * Given a desired time, duration, and action (turn), return the next available time slot.
     * If the requested time slot is available, return that.
     * @param start The earliest the requested time slot can be assigned
     * @param duration The time required for this action
     * @param turn The desired action (turn)
     * @return The start time of the assigned time slot
     */
    public double request (double start, double duration, Turn turn) {
        IntersectionReservation request = new IntersectionReservation(start, duration, turn);
        LinkedList<IntersectionReservation> conflicts = schedule.getConflicts(request);
        while (!conflicts.isEmpty()){
            //dbug("resolving, time %f", conflicts.getLast().getEnd());
        	request.setStart(conflicts.getLast().getEnd());
        	conflicts = schedule.getConflicts(request);
        }
        return request.getStart();
    }

    /**
     * Try to schedule a turn for a particular time slot.  This time slot should be determined by calling
     * the request method in most cases.
     * @param start The beginning of the time slot
     * @param duration The length required for the turn
     * @param turn The turn information
     * @throws SchedulingConflictException If the given turn cannot be executed during the time slot given
     */
    public void schedule (double start, double duration, Turn turn) throws SchedulingConflictException {
        //dbug("scheduling %s from %f to %f", turn, start, start + duration);
        IntersectionReservation request = new IntersectionReservation(start, duration, turn);
        if (schedule.canLegallyAdd(request)) schedule.add(request);
        else throw new SchedulingConflictException("Conflict when trying to schedule request: "+request);
    }
    
    /**
     * A SchedulingConflictException may be thrown when a scheduling conflict occurs
     * @author Mike Depinet
     * @version 1.0
     */
    public class SchedulingConflictException extends Exception{
    	private static final long serialVersionUID = 1082031536953665031L;
    	SchedulingConflictException(){
    		super();
    	}
    	SchedulingConflictException(String str){
    		super(str);
    	}
    }
}

/**
 * IntersectionReservation encodes turn information along with a start time and duration
 * IntersectionReservations may be used by the Scheduler to reserve action spaces
 * @author Mike Depinet
 * @version 1.0
 */
class IntersectionReservation implements Comparable<IntersectionReservation> {
	private Turn turn;
    private double start, duration;
	
    /**
     * Full constructor
     * @param start The starting time of this reservation
     * @param duration How long this reservation lasts
     * @param turn The turn information associated with this reservation
     */
	public IntersectionReservation (double start, double duration, Turn turn) {
        this.turn = turn;
        this.start = start;
        this.duration = duration;
	}

	/**
	 * Modifier method for the start time
	 * @param start The new start time
	 */
    public void setStart (double start) {
        this.start = start;
    }
    /**
     * Accessor method for the start time
     * @return The start time
     */
    public double getStart () {
        return start;
    }
    /**
     * Return the time at which this reservation will end (start + duration)
     * @return The end time
     */
    public double getEnd () {
        return start + duration;
    }
    /**
     * Accessor method for the duration
     * @return The duration of this reservation
     */
    public double getDuration () {
    	return duration;
    }

    /**
     * Compares two IntersectionReservation objects.  They are sorted by start time.
     * @override {@link #compareTo(IntersectionReservation)}
     */
	public int compareTo (IntersectionReservation other) {
        // Sort by time alone. Only worry about starting time
        return start < other.start ? -1 : start == other.start ? 0 : 1;
	}
	/**
	 * Determines the equality of two IntersectionReservation objects.  It does this in the obvious way.
	 * @override {@link #equals(Object)}
	 */
	public boolean equals (Object other){
		if (!(other instanceof IntersectionReservation)) return false;
		IntersectionReservation ir = (IntersectionReservation) other;
		return turn.equals(ir.turn) && start == ir.start && duration == ir.duration;
	}
	
    /**
     * Determines whether or not this reservation interferes with another.
     * On a high level, this indicates whether or not two cars - one with this reservation,
     * 		the other with the input reservation - will crash
     * @param other The reservation to consider
     * @return Whether or not the reservations conflict
     */
	public boolean conflicts (IntersectionReservation other) {
		return overlaps(other) && turn.getConflicts().contains(other.turn);
	}
    /**
     * Determines whether or not this reservation occurs at the same time as another.  Two reservations
     * 		can overlap without conflicting if their turns do not cross each other
     * @param other The reservation to consider
     * @return Whether or not the reservations' times overlap
     */
	public boolean overlaps (IntersectionReservation other) {
        return (start <= other.start && getEnd() > other.start) ||
               (other.start <= start && other.getEnd() > start);
	}

	/**
	 * Determines whether or not another reservation will be completed before this one begins
	 * @param other The other reservation to consider
	 * @return other.getEnd() < getStart
	 */
	public boolean occursBeforeStart (IntersectionReservation other) {
        return other.getEnd() < start;
	}
    /**
     * Determines whether or not another reservation occurs before the end of this one
     * @param other The other reservation to consider
     * @return other.getEnd() <= getEnd()
     */
	public boolean occursBeforeEnd (IntersectionReservation other) {
        return other.getEnd() <= getEnd();
	}
	/**
	 * Determines whether or not another reservation will begin after this one ends
	 * @param other The other reservation to consider
	 * @return other.getStart() > getEnd()
	 */
	public boolean occursAfterEnd (IntersectionReservation other) {
        return other.getStart() > getEnd();
	}
    /**
     * Determines whether or not another reservation will begin after this one begins
     * @param other The other reservation to consider
     * @return other.getStart() >= getStart()
     */
	public boolean occursAfterStart (IntersectionReservation other) {
        return other.getStart() >= getStart();
	}
	
	/**
	 * @override {@link #toString()}
	 */
	public String toString () {
        return "Reservation from " + start + " till " + getEnd() + ": " + turn;
	}
}

/**
 * A tree used to store and access IntersectionReservations.  It is used to quickly look up potential conflicts
 * @author Mike Depinet
 * @version 1.0 - incomplete
 */
class SchedulingTree implements Scheduler<IntersectionReservation>{
	private IntersectionReservation res;
	private SchedulingTree left;
	private SchedulingTree right;
	
	/**
	 * Default constructor
	 */
	public SchedulingTree(){
		res = null;
		left = right = null;
	}
	/**
	 * Constructor with root node
	 * @param res The value at the root
	 */
	public SchedulingTree(IntersectionReservation res){
		this.res = res;
		left = right = null;
	}
	
	/**
	 * Accessor method for the reservation
	 * @return The reservation at this node
	 */
	public IntersectionReservation getReservation(){
		return res;
	}
	/**
	 * Accessor method for the left subtree
	 * @return The root of the left subtree
	 */
	SchedulingTree left(){ //package access
		return left;
	}
	/**
	 * Accessor method for the right subtree
	 * @return The root of the right subtree
	 */
	SchedulingTree right(){
		return right;
	}
	
	/**
	 * Add an element to the SchedulingTree.  Note that this will NOT make sure the element
	 * can legally be added.  This is to make it more reusable.
	 * @param res The element to add
	 */
	public void add(IntersectionReservation res){ //TODO Add balancing
		if (this.res == null) this.res = res;
		else if (res.compareTo(this.res) < 0){
			if (left == null) left = new SchedulingTree(res);
			else left.add(res);
		}
		else{
			if (right == null) right = new SchedulingTree(res);
			else right.add(res);
		}
	}
	/**
	 * Remove an element from the SchedulingTree.
	 * @param res The element to remove
	 */
	public void remove(IntersectionReservation res){ //TODO Implement this (with balance maintenance)
		throw new UnsupportedOperationException("You can't remove stuff yet.  Get over it.");
	}
	
	/**
	 * Get all of the IntersectionReservations in this SchedulingTree that conflict with the one given.
	 * 		The resulting list is returned with the conflicts in order.
	 * 		If there are no conflicts, an empty list is returned.
	 * @param res The reservation to find conflicts for
	 * @return A LinkedList of the conflicts, in order by start time
	 */
	public LinkedList<IntersectionReservation> getConflicts(IntersectionReservation res){
		if (this.res == null) return new LinkedList<IntersectionReservation>();
		LinkedList<IntersectionReservation> conflicts = new LinkedList<IntersectionReservation>();
		if (left != null && res.compareTo(this.res) < 0) conflicts.addAll(left.getConflicts(res));
		if (res.conflicts(this.res)) conflicts.add(this.res);
		if (right != null && res.compareTo(this.res) > 0) conflicts.addAll(right.getConflicts(res));
		return conflicts;
	}
	
	/**
	 * Determines whether or not a reservation can be added to this SchedulingTree without conflicting with anything
	 * @return True if the reservation can be added without conflict
	 */
	public boolean canLegallyAdd(IntersectionReservation res){
		if (this.res == null) return true;
		else if (res.compareTo(this.res) < 0) return (left == null) ? true : left.canLegallyAdd(res);
		else if (res.compareTo(this.res) > 0) return (right == null) ? true : right.canLegallyAdd(res);
		else return !res.conflicts(this.res);
	}
	
	/**
	 * Traverses the tree to give the currently scheduled reservations
	 * @return A string of the tree
	 */
	public String printSchedule(){
		String output = "";
		output += left.printSchedule();
		output += ""+res+"\n";
		output += right.printSchedule();
		return output;
	}
}

/**
 * A Scheduler keeps track of events and potential conflicts between them
 * @author Mike Depinet
 * @version 1.0
 * @param <T> The type of events to keep track of
 */
interface Scheduler<T>{
	public boolean canLegallyAdd(T obj);
	public LinkedList<T> getConflicts(T obj);
	public void remove(T obj);
	public void add(T obj);
	public String printSchedule();
}
