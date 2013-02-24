// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.market

import utexas.aorta.sim.{Agent, Ticket, IntersectionType}

import scala.collection.mutable.{HashMap, MultiMap}
import scala.collection.mutable.{Set => MutableSet}

import utexas.aorta.{Util, cfg}

abstract class IntersectionOrdering[T]() {
  // The client can muck with this directly if they, say, want to filter it out.
  var queue = List[T]()

  def shift_next(waiting_participants: Iterable[Ticket], itype: IntersectionType.Value): Option[T]

  def add(item: T) = {
    queue :+= item
  }
}

class FIFO_Ordering[T]() extends IntersectionOrdering[T]() {
  def shift_next(waiting_participants: Iterable[Ticket], itype: IntersectionType.Value): Option[T] = {
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
  def shift_next(participants: Iterable[Ticket], itype: IntersectionType.Value): Option[T] = {
    // Handle degenerate cases where we don't hold auctions.
    if (queue.isEmpty) {
      return None
    }
    if (queue.size == 1) {
      val next = queue.head
      queue = queue.tail
      return Some(next)
    }

    // Collect bids, remembering what each agent bids, and group by choice.
    val bids = new HashMap[T, MutableSet[Bid]] with MultiMap[T, Bid]
    participants.foreach(who => {
      who.a.wallet.bid(queue, who, itype) match {
        case Some((ticket, amount)) => {
          Util.assert_gt(amount, 0.0)
          bids.addBinding(ticket, Bid(who, amount))
        }
        case None =>
      }
    })

    return if (bids.isEmpty) {
      // They're all apathetic, so just do FIFO.
      val next = queue.head
      queue = queue.tail
      Some(next)
    } else {
      Some(process_auction(bids))
    }
  }

  private def process_auction(bids: MultiMap[T, Bid]): T = {
    val sums = bids.keys.map(
      t => (t, bids(t).map(_.amount).sum)
    ).toList.sortBy(_._2).reverse

    val winner = sums.head._1

    // Direct payment... all the winners pay their full bid.
    //bids(winner).foreach(bid => bid.who.a.wallet.spend(bid.amount))

    // Proportional payment... the winner must pay the amount that the runner-up
    // bid. Each member of the winner pays proportional to what they bid.
    val rate = sums.tail.head._2 / sums.head._2
    bids(winner).foreach(bid => bid.who.a.wallet.spend(bid.amount * rate))

    return winner
  }
}
