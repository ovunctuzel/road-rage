// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.market

import utexas.aorta.sim.{Agent, Ticket}

import scala.collection.mutable.{HashMap, MultiMap}
import scala.collection.mutable.{Set => MutableSet}

import utexas.aorta.{Util, cfg}

abstract class IntersectionOrdering[T]() {
  // The client can muck with this directly if they, say, want to filter it out.
  var queue = List[T]()

  def shift_next(waiting_participants: Iterable[Ticket]): Option[T]

  def add(item: T) = {
    queue :+= item
  }
}

class FIFO_Ordering[T]() extends IntersectionOrdering[T]() {
  def shift_next(waiting_participants: Iterable[Ticket]): Option[T] = {
    if (queue.nonEmpty) {
      val next = queue.head
      queue = queue.tail
      return Some(next)
    } else {
      return None
    }
  }
}

// Who's willing to pay what
case class Bid(who: Ticket, amount: Double)

class AuctionOrdering[T]() extends IntersectionOrdering[T]() {
  def shift_next(participants: Iterable[Ticket]): Option[T] = {
    // Handle degenerate cases where we don't hold auctions.
    if (queue.isEmpty) {
      return None
    }
    if (queue.size == 1) {
      val next = queue.head
      queue = queue.tail
      return Some(next)
    }

    // Collect bids, remembering what each agent bids, and group by ticket.
    val bids = new HashMap[T, MutableSet[Bid]] with MultiMap[T, Bid]
    participants.foreach(who => {
      who.a.wallet.bid(queue, who) match {
        case (ticket, amount) if amount > 0.0 => {
          bids.addBinding(ticket, Bid(who, amount))
        }
        case _ =>
      }
    })

    if (bids.isEmpty) {
      // They're all apathetic, so just do FIFO.
      val next = queue.head
      queue = queue.tail
      return Some(next)
    } else {
      val winner = bids.keys.map(t => (bids(t).map(_.amount).sum, t)).maxBy(_._1)
      collect_payment(winner._1, bids(winner._2))
      queue = queue.filter(_ != winner._2)
      return Some(winner._2)
    }
  }

  private def collect_payment(total: Double, participants: Iterable[Bid]) = {
    // TODO track revenue.
    // TODO Simple division for now -- make them pay their full bid.
    participants.foreach(bid => bid.who.a.wallet.spend(bid.amount))
  }
}
