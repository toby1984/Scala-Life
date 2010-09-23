package de.codesourcery.life.entities

trait RegionLockSet {

	def doWithRegionLock(x1:Int,y1:Int,x2:Int,y2:Int)( func: => Unit )
}

