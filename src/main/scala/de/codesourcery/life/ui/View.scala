package de.codesourcery.life.ui

trait View {

	def setController(controller:UIController)
	    
	def simulatorStateChanged(isRunning:Boolean)
		
	def modelChanged()
	
	def savedStateAvailable()
	
	def simulatorSpeedChanged( delayInMillis : Int )
	
	def querySaveFileName() : Option[String]
	
	def queryLoadFileName() : Option[String]	
}