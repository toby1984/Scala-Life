package de.codesourcery.life.entities

trait RegionLockSet {

	def doWithRegionLock(x1:Int,y1:Int,w:Int,h:Int)( func: => Unit )
}

