package com.github.oskin1.factnet.network

import java.net.InetSocketAddress

trait PeerStore {

  /** Get pool size.
    */
  def size: Int

  /** Add a given `address` to the store.
    */
  def add(address: InetSocketAddress, ts: Long): Either[String, PeerStore]

  /** Update a timestamp a given address `address` was last seen.
    */
  def seen(address: InetSocketAddress, ts: Long): PeerStore

  /** Confirm connection to a given `address`.
    */
  def confirm(address: InetSocketAddress, ts: Long): PeerStore

  /** Remove a given `address` from the store.
    */
  def remove(address: InetSocketAddress): PeerStore

  /** Check whether a given `address` present in the store.
    */
  def contains(address: InetSocketAddress): Boolean

  /** Get all peers from the store.
    */
  def getAll: List[InetSocketAddress]
}

object PeerStore {

  final case class PeerInfo(lastSeen: Long, confirmed: Boolean)

  def empty(poolLimit: Int): PeerStore = new Impl(Map.empty, poolLimit)

  final class Impl(pool: Map[InetSocketAddress, PeerInfo], sizeBound: Int) extends PeerStore {

    def size: Int = pool.size

    def add(address: InetSocketAddress, ts: Long): Either[String, PeerStore] =
      if (pool.size < sizeBound)
        Right(new Impl(pool + (address -> PeerInfo(ts, confirmed = false)), sizeBound))
      else Left("Pool limit exceeded")

    def seen(address: InetSocketAddress, seenAt: Long): PeerStore = {
      val updatePool = pool.get(address) match {
        case Some(peerInfo) => pool.updated(address, peerInfo.copy(lastSeen = seenAt))
        case None => pool
      }
      new Impl(updatePool, sizeBound)
    }

    def confirm(address: InetSocketAddress, ts: Long): PeerStore = {
      val updatePool = pool.updated(address, PeerInfo(ts, confirmed = true))
      new Impl(updatePool, sizeBound)
    }

    def remove(address: InetSocketAddress): PeerStore =
      new Impl(pool - address, sizeBound)

    def contains(address: InetSocketAddress): Boolean =
      pool.contains(address)

    def getAll: List[InetSocketAddress] =
      pool.keys.toList
  }
}
