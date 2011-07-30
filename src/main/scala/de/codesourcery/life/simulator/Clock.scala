package de.codesourcery.life.simulator

import de.codesourcery.life.entities.Board
import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic._

/**
 * Creates a 'clock' that periodically calls a {@link ClockListener}.
 * 
 * <p>
 * Note: Setting the clock interval to zero will
 * invoke the listeners <code>onTick()</code>
 * method as fast as possible.
 * </p>
 * @author tobias.gierke@code-sourcery.de
 */
class Clock(private val caller : ClockListener ) 
{
	    private val LOCK : AnyRef = new Object()
		
	    private val tickIntervalInMillis = new AtomicInteger(100)
		
		// @GuardedBy( LOCK )
		private var clock : Option[ClockThread]= None
		
		/**
		 * Implements the actual clock
		 * and notifies listeners of clock ticks
		 * and state changes. 
		 */
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
							val delay = tickIntervalInMillis.get
							if ( delay > 0 ) {
								java.lang.Thread.sleep( delay )
							}
						}
					} while ( ! stopRequested.get )
						
				} finally {
				  	caller.clockStateChanged( false )
					queue.countDown()
				}
			}
			
			def terminate() {
				stopRequested.set(true)
			}
			
			def waitForDeath() {
				queue.await()
				println("Clock thread stopped.")
			}
		}
		
	    /**
	     * Stops this clock.
	     * 
	     * If the clock isn't running, nothing
	     * harmful happens.
	     */
		def stop() = {
			
			val existingThread = LOCK.synchronized 
			{
				clock match {
					case Some(tmp ) => {
						tmp.terminate()
						clock = None
						Some(tmp)
					}
					case _ => None
				}
			}
			// call from outside synchronized block
			if ( existingThread.isDefined ) {
				existingThread.get.waitForDeath()
			}
		}
		
		/**
		 * Returns this clock's tick interval in milliseconds.
		 * @return
		 */
		def tickIntervalMillis : Int = tickIntervalInMillis.get
		
		/**
		 * Sets this clock's tick interval in milliseconds.
		 * 
		 * @param milliseconds
		 */
		def tickIntervalMillis_=( milliseconds : Int ) {
			require( milliseconds >= 0 )
			tickIntervalInMillis.set( milliseconds )
		}
			
		/**
		 * Starts this clock.
		 * 
	     * If the clock is already running, nothing
	     * harmful happens.		 
		 */
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
					case _ => // clock is already running
				}
			}
		}			
			
	}
