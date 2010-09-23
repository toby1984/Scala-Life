package de.codesourcery.life.entities

import scala.collection.mutable.HashSet
import java.util.concurrent.locks.ReentrantLock

import java.awt.Rectangle

/**
 * Implements thread-safe locking of rectangular
 * regions , guaranteeing the clients
 * exclusive to a region until the lock is released.
 * 
 * @author tobias.gierke@code-sourcery.de
 */
class BoardLockSet(width:Int,height:Int) extends RegionLockSet {

	private val LOCK_COUNT = 32
	
	private val lockSet : Array[Rectangle]= BoardLockSet.partition(width,height, LOCK_COUNT )
	private val locks : Array[ ReentrantLock] = {
		val tmp = new Array[ReentrantLock]( lockSet.length )
		var i = 0
		while ( i < tmp.length ) {
			tmp(i) = new ReentrantLock()
			i += 1
		}
		tmp
	}
	
	def doWithRegionLock(x1:Int,y1:Int,x2:Int,y2:Int)( func: => Unit ) {

		var i = 0
		while ( i < lockSet.length ) {
			if ( lockSet(i).intersects( x1 , y1 , x2-x1 , y2-x1 ) ) 
			{
				locks(i).lock()
				try {
					func
					return
				} finally {
					locks(i).unlock()
				}
			}
			i += 1
		}
	}
}

object BoardLockSet {
	
	/**
	 * Partitions a rectangular area with 
	 * extent (0,0,width,height) into
	 * at least the given number of
	 * non-overlapping squares with equal area. 
	 * 
	 * @param areaWidth
	 * @param areaHeight
	 * @param regionCount the number of squares that should at least be created.
	 * Due to overlapping the returned number of rectangles will mostly be
	 * slightly larger than the requested number
	 * @return 
	 */
	def partition(areaWidth:Int,areaHeight:Int, regionCount : Int ) : Array[Rectangle] = {
		
	    val regionExtend : Int = {
				val area = areaWidth * areaHeight
				val areaPerRegion = area / regionCount		
				Math.floor( Math.sqrt( areaPerRegion ) ).toInt
		}
		
	    val tmp = new scala.collection.mutable.ListBuffer[Rectangle]()
		var y = 0
		while ( y < areaHeight) {
			var x = 0
			while ( x < areaWidth ) {
				tmp + new Rectangle( x , y , regionExtend, regionExtend )
				x+= regionExtend + 1
			}
			y+= regionExtend + 1
		}
		tmp.toArray
	}
}

