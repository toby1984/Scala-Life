package de.codesourcery.life.entities

import scala.collection.mutable.HashSet
import java.util.concurrent.locks.ReentrantLock
import java.awt.geom.Area
import java.awt.Rectangle
import java.util.concurrent.atomic.AtomicLong

/**
 * Implements thread-safe locking of rectangular
 * regions , guaranteeing the clients
 * exclusive to a region until the lock is released.
 * 
 * @author tobias.gierke@code-sourcery.de
 */
class BoardLockSet(width:Int,height:Int,lockCount:Int)  {

	private val lockSet : Array[Rectangle]= BoardLockSet.partition(width,height, lockCount )

	private val collisionCache = new java.util.HashMap[Int,Array[ReentrantLock]]
	
	private val debugMode = true

    private val locks : Array[ ReentrantLock] = {
			val tmp = new Array[ReentrantLock]( lockSet.length )
			var i = 0
			while ( i < tmp.length ) {
				tmp(i) = new ReentrantLock()
				i += 1
			}
			tmp
	}

	def doWithRegionLock(x1:Int,y1:Int,w:Int,h:Int)( func: => Unit ) {

		val x2 = x1 + w
		val y2 = y1 + h

		var hash = 23
		hash = hash * 31 + ( x1 ^( x1 << 16 ) )
		hash = hash * 31 + ( y1 ^( x1 << 16 ) )
		hash = hash * 31 + ( w ^( x1 << 16 ) )
		hash = hash * 31 + ( h ^( x1 << 16 ) )
		
        var locksToGet : Array[ReentrantLock] = null
		var aquiredLockCount = 0
		try 
		{
			collisionCache.synchronized 
			{
				locksToGet = collisionCache.get( hash )
				if ( locksToGet == null ) 
				{
					// no locks cached yet
					locksToGet = new Array[ReentrantLock](lockSet.length)
					var currentIndex = 0
					var i = 0
					while ( i < locksToGet.length ) 
					{
						if ( lockSet(i).intersects( x1 , y1 , w , h ) ) 
						{
							locksToGet(currentIndex) = locks(i)
							currentIndex+=1
						}
						i += 1
					}
					collisionCache.put( hash , locksToGet )
				}
			}
			
			var currentLock = locksToGet(0)
			while ( currentLock != null ) {
				currentLock.lock()
				aquiredLockCount += 1	
				if ( aquiredLockCount < lockSet.length ) {
					currentLock = locksToGet(aquiredLockCount) // at least one lock is always present
				} else {
					currentLock = null
				}
			}
			
			if ( aquiredLockCount == 0 ) {
				for ( rect <- lockSet ) {
					println("Checked: "+rect)
				}
				throw new RuntimeException("Internal error, unable to lock region ("+x1+","+y1+") -> ("+x2+","+y2+")")
			}
					
			func
		} 
		finally 
		{
		    if ( aquiredLockCount > 0 ) 
		    {
				var i : Int = 0
				while ( i < aquiredLockCount ) {
					locksToGet(i).unlock()
					i += 1
				}
		    }
		}
	}
}

object BoardLockSet {

	private val cache = new java.util.concurrent.ConcurrentHashMap[Int,Array[Rectangle]]()

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
	def partition(width:Int,height:Int, regionCount : Int ) : Array[Rectangle] = {

		var key : Int = 23
		key = key*31+(width^(width << 16 ) )
		key = key*31+(height^(height << 16 ) )
		key = key*31+(regionCount^(regionCount << 16 ) )

		if ( cache.containsKey( key ) ) {
			return cache.get( key )
		}

	val areaWidth = width-1 // [0...width-1] = width elements
	val areaHeight = height -1 // [0...height-1] = height elements

	val regionExtend : Int = {
			val area = areaWidth * areaHeight
			val areaPerRegion = area / regionCount		
			Math.floor( Math.sqrt( areaPerRegion ) ).toInt
	}

	val tmp = new scala.collection.mutable.ListBuffer[Rectangle]()
	var y = 0
	while ( y < areaHeight ) {
		val y2 = y + regionExtend
		val remainingHeight = if ( y2 > areaHeight) areaHeight -y else regionExtend
		var x = 0
		while ( x < areaWidth ) {
			val x2 = x + regionExtend
			val remainingWidth = if ( x2 > areaWidth ) areaWidth -x else regionExtend
			val rect = new Rectangle( x , y , remainingWidth , remainingHeight )
			println("Creating region "+rect)				
			tmp + rect
			x+= regionExtend + 1
		}
		y+= regionExtend + 1
	}
	println("Created "+regionCount+" lockable regions")	
	val result = tmp.toArray[Rectangle]
	                         cache.put( key ,result )
	                         result
	}
}

