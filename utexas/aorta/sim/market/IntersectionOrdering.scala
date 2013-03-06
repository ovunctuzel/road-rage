// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.market

import utexas.aorta.sim.{Agent, Ticket, Policy, IntersectionType}

import scala.collection.mutable.{HashMap, MultiMap}
import scala.collection.mutable.{Set => MutableSet}

import utexas.aorta.{Util, cfg}

abstract class IntersectionOrdering[T <: Ordered[T]]() {
  // The client can muck with this directly if they, say, want to filter it out.
  var queue = List[T]()

  def shift_next(waiting_participants: Iterable[Ticket], client: Policy): Option[T]

  def add(item: T) = {
    queue :+= item
  }

  def clear() = {
    queue = Nil
  }
}

class FIFO_Ordering[T <: Ordered[T]]() extends IntersectionOrdering[T]() {
  def shift_next(waiting_participants: Iterable[Ticket], client: Policy): Option[T] = {
    if (queue.nonEmpty) {
      val next = queue.head
      queue = queue.tail
      return Some(next)
    } else {
      return None
    }
  }
}

// Who's willing to pay how much for what underlying purpose. Purpose will be
// null for system bids.
case class Bid[T](who: Wallet, item: T, amount: Double, purpose: Ticket)
{
  Util.assert_ge(amount, 0.0)

  override def toString = s"Bid($amount for $item)"
}

class AuctionOrdering[T <: Ordered[T]]() extends IntersectionOrdering[T]() {
  def shift_next(voters: Iterable[Ticket], client: Policy): Option[T] = {
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
    val bids = new HashMap[T, MutableSet[Bid[T]]] with MultiMap[T, Bid[T]]
    voters.foreach(who => {
      who.a.wallet.bid(queue, who, client) match {
        case Some((ticket, amount)) if amount > 0.0 => {
          bids.addBinding(ticket, Bid(who.a.wallet, ticket, amount, who))
        }
        case _ =>
      }
    })
    // Ask the System, too
    for (bid <- SystemWallets.meta_bid(queue, client) if bid.amount > 0.0) {
      bids.addBinding(bid.item, bid)
    }

    return if (bids.isEmpty) {
      // They're all apathetic, so just do FIFO.
      val next = queue.head
      queue = queue.tail
      Some(next)
    } else {
      Some(process_auction(bids, client))
    }
  }

  private def process_auction(bids: MultiMap[T, Bid[T]], client: Policy): T =
  {
    // Break ties arbitrarily but deterministically.
    val sums_by_item: List[(T, Double)] = bids.keys.map(
      t => (t, bids(t).map(_.amount).sum)
    ).toList.sortBy(pair => pair._1)
    // Now sort by sum. As long as this is a stable sort, fine.
    // (Lotsa type nonsense when I try to do this in one go.)
    val sums = sums_by_item.sortBy(pair => pair._2)

    val winner = sums.head._1
    if (client.intersection.v.id == 1) {
      println(s"winner $winner")
    }

    client.policy_type match {
      case IntersectionType.Reservation => {
        val winner_ticket = winner.asInstanceOf[Ticket]
        // The one winning group pays the bid of the highest losing group who
        // had a conflicting turn.
        sums.tail.find(
          pair => pair.asInstanceOf[(Ticket, Double)]._1.turn.conflicts_with(winner_ticket.turn)
        ) match {
          case Some(group) => {
            pay(bids(winner), group._2)
          }
          // If nobody's turn conflicts, then don't pay at all!
          case None =>
        }
      }
      case _ => {
        pay(bids(winner), sums.tail.head._2)
      }
    }

    return winner
  }

  // A group splits some cost somehow.
  private def pay(who: Iterable[Bid[T]], total: Double) = {
    // Direct payment... all the winners pay their full bid.
    //who.foreach(bid => bid.who.spend(bid.amount, bid.purpose))

    // Proportional payment... Each member of the winner pays proportional to
    // what they bid.
    val total_winners = who.map(_.amount).sum
    val rate = total / total_winners
    who.foreach(
      bid => bid.who.spend(bid.amount * rate, bid.purpose)
    )
  }
}
