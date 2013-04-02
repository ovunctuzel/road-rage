// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.market

import utexas.aorta.sim.{Agent, Ticket, Policy, IntersectionType, OrderingType}

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

  def ordering_type(): OrderingType.Value
}

class FIFO_Ordering[T <: Ordered[T]]() extends IntersectionOrdering[T]() {
  def ordering_type = OrderingType.FIFO
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
case class Bid[T](who: Wallet, item: T, amount: Int, purpose: Ticket)
{
  Util.assert_ge(amount, 0.0)

  override def toString = s"Bid($amount for $item)"
}

class AuctionOrdering[T <: Ordered[T]]() extends IntersectionOrdering[T]() {
  def ordering_type = OrderingType.Auction
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
    val multipliers = new HashMap[T, Int]()
    queue.foreach(item => multipliers(item) = 1)
    voters.foreach(who => {
      for ((ticket, amount) <- who.a.wallet.bid(queue, who, client)) {
        bids.addBinding(ticket, Bid(who.a.wallet, ticket, amount, who))
      }
    })
    // Ask the System, too, interpreting responses as multipliers to existing
    // bids
    for (bid <- SystemWallets.meta_bid(queue, client) if bid.amount > 0) {
      multipliers(bid.item) *= bid.amount
      // and implicitly add 1 unit of currency to the user bid, so the system
      // can help freeriders.
      bids.addBinding(bid.item, Bid(bid.who, bid.item, 1, null))
    }

    return if (bids.isEmpty) {
      // They're all apathetic, so just do FIFO.
      val next = queue.head
      queue = queue.tail
      Some(next)
    } else {
      val debug = client.intersection.v.id == -1
      Some(process_auction(debug, bids, multipliers, client))
    }
  }

  private def process_auction(debug: Boolean, bids: MultiMap[T, Bid[T]], multipliers: HashMap[T, Int], client: Policy): T =
  {
    // Break ties arbitrarily but deterministically.
    // The .toList is necessary, since it's a set otherwise... same bids get
    // squished together!
    val sums_by_item: List[(T, Int)] = bids.keys.map(
      t => (t, bids(t).toList.map(bid => bid.amount).sum * multipliers(t))
    ).toList.sortBy(pair => pair._1)
    if (sums_by_item.tail.isEmpty) {
      // Actually, just one choice. Freebie!
      return sums_by_item.head._1
    }

    if (debug) {
      for (i <- sums_by_item) {
        println(s"... ${i._2} (multiplier ${multipliers(i._1)}) for ${i._1}")
      }
      println("")
    }

    // Now sort by sum. As long as this is a stable sort, fine.
    // (Lotsa type nonsense when I try to do this in one go.)
    val sums = sums_by_item.sortBy(pair => pair._2).reverse

    val winner = sums.head._1

    client.policy_type match {
      case IntersectionType.Reservation => {
        val winner_ticket = winner.asInstanceOf[Ticket]
        // The one winning group pays the bid of the highest losing group who
        // had a conflicting turn.
        sums.tail.find(
          pair => pair.asInstanceOf[(Ticket, Int)]._1.turn.conflicts_with(winner_ticket.turn)
        ) match {
          case Some(group) => {
            pay(bids(winner).toList, multipliers(winner), group._2)
          }
          // If nobody's turn conflicts, then don't pay at all!
          case None =>
        }
      }
      case _ => {
        pay(bids(winner).toList, multipliers(winner), sums.tail.head._2)
      }
    }

    return winner
  }

  // A group splits some cost somehow.
  private def pay(who: List[Bid[T]], multiplier: Int, total_runnerup: Int) = {
    // Proportional payment... Each member of the winner pays proportional to
    // what they bid.
    val total_winners = who.map(_.amount).sum * multiplier
    Util.assert_ge(total_winners, total_runnerup)
    val rate = (total_runnerup.toDouble / multiplier.toDouble) / total_winners.toDouble
    who.foreach(
      bid => bid.who.spend((bid.amount * rate).toInt, bid.purpose)
    )
  }
}
