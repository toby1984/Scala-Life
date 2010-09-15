package de.codesourcery.life.ui

import de.codesourcery.life.entities.Board
import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic._

class Clock(private val caller : ClockListener ) 
{
	    private val LOCK : AnyRef = new Object()
		
	    private val tickInterval = new AtomicInteger(100)
		
		// @GuardedBy( LOCK )
		private var clock : Option[ClockThread]= None
		
		private class ClockThread extends Thread {
			
			val stopRequested  = new AtomicBoolean(false)
			val queue = new CountDownLatch(1)
			
			override def run() 
			{
				caller.clockStateChanged( true )
				try {
					do {
						if ( ! caller.onTick() ) {
							terminate()
						} else {
							val delay = tickInterval.get
							if ( delay > 0 ) {
								java.lang.Thread.sleep( delay )
							}
						}
					} while ( ! stopRequested.get )
						
				} finally {
					queue.countDown()
					caller.clockStateChanged( false )
				}
			}
			
			def terminate() {
				stopRequested.set(true)
			}
			
			def waitForDeath() {
				queue.await()
			}
		}
		
		def stop() = {
			
			val existingThread = LOCK.synchronized 
			{
				if ( clock.isDefined ){
					val tmp = clock.get
					tmp.terminate()
					clock = None
					Some(tmp)
				} else {
					None
				}
			}
			
			if ( existingThread.isDefined ) {
				existingThread.get.waitForDeath()
			}
		}
		
		def getTickInterval : Int = tickInterval.get
		
		def setTickInterval( milliseconds : Int ) {
			require( milliseconds >= 0 )
			tickInterval.set( milliseconds )
		}
			
		def start() {
			LOCK.synchronized {
				clock match {
					case Some(x) if ( ! x.isAlive ) =>  {
						clock = Some( new ClockThread )
						clock.get.start()
					}
					case None => { 
						clock = Some( new ClockThread )
						clock.get.start()
					}
					case _ =>
				}
			}
		}			
			
	}
