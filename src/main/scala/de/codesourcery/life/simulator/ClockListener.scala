package de.codesourcery.life.simulator

trait ClockListener {
	
	/**
	 * Called whenever the clock starts or stops.
	 * 
	 * @param isRunning whether the clock is currently running
	 */
	def clockStateChanged(isRunning:Boolean)
	
	/**
	 * 
	 * @return <code>true</code> if clock should continue running, otherwise <code>false</code>
	 */
	def onTick() : Boolean
} 
