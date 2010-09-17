package de.codesourcery.life.simulator

/**
 * Listens to clock ticks and state changes.
 *  
 * @author tobias.gierke@code-sourcery.de
 */
trait ClockListener {
	
	/**
	 * Called whenever the clock starts or stops.
	 * 
	 * @param isRunning whether the clock is currently running
	 */
	def clockStateChanged(isRunning:Boolean)
	
	/**
	 * Invoked on every clock tick.
	 * 
	 * @return <code>true</code> if clock should continue running, otherwise <code>false</code>
	 */
	def onTick() : Boolean
} 
