package de.codesourcery.life.ui

/**
 * Displays the Game of Life UI.
 * 
 * @author tobias.gierke@code-sourcery.de
 */
trait View {

	/**
	 * Sets the controller for this view.
	 *  
	 * @param controller
	 */
	def setController(controller:UIController)
	   
	/**
	 * Called whenever the simulation is
	 * started or stopped.
	 * 
	 * @param isRunning indicates whether the simulation has started or stopped
	 */
	def simulatorStateChanged(isRunning:Boolean)
	
	/**
	 * Called whenever the view model has changed.
	 */
	def modelChanged()
	
	/**
	 * Invoked by the controller if saved state is available.
	 * 
	 * @see UIController#SaveStateClicked()
	 */
	def savedStateAvailable()
	
	/**
	 * Called whenever the simulation speed has changed.
	 * 
	 * @param delayInMillis
	 */
	def simulatorSpeedChanged( delayInMillis : Int )
	
	/**
	 * Lets the user choose the name of a file to
	 * save model state in.
	 * 
	 * @return filename or None if the user cancelled the operation
	 */
	def querySaveFileName() : Option[String]
	
	/**
	 * Lets the user choose the name of a file to
	 * load model state from.
	 * 
	 * @return filename or None if the user cancelled the operation
	 */	
	def queryLoadFileName() : Option[String]	
}