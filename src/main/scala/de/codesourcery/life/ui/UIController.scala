package de.codesourcery.life.ui

import de.codesourcery.life.entities.Board
import de.codesourcery.life.simulator._

abstract class UIController(private val m : Board , private val view : View ) {

	private var lastState : Option[Board] = None
	
	private val listener = new ClockListener() {
		
		def clockStateChanged(isRunning:Boolean) {
			view.simulatorStateChanged( isRunning )
		}
	
		def onTick() : Boolean = {
			if ( model.advance() ) {
				view.modelChanged()
				true
			} else {
			false
			}
		}
    }
	
	def model : Board = m
    
	private val clock = new Clock( listener )
	
	def resetButtonClicked() {
		updateBoard {
			initModel( model )
		}
	}
	
	def speedChanged(percent:Int) {
		val delay = Math.round( 1000.0  * ( percent / 100.0 ) )
		clock.setTickInterval( delay.asInstanceOf[Int] )
		view.simulatorSpeedChanged( delay.asInstanceOf[Int] )
	}
	
	protected def initModel( board : Board)
	
	def clearButtonClicked() {
		updateBoard {
			model.reset()
		}
	}
	
	private def updateBoard( func : => Unit ) {
		stopButtonClicked()
		func
		view.modelChanged()
	}
	
	def getSimulationDelayInMillis() : Int = clock.getTickInterval
	
	def saveStateClicked() {
		stopButtonClicked()
		lastState = Some( model.copy() )
		view.savedStateAvailable()
	}
	
	def recallStateClicked() {
		
		lastState match {
			case Some( state ) =>  updateBoard { model.valueOf( state ) }
			case _ =>
		}
	}
	
	def cellClicked(x:Int,y:Int) {
		updateBoard {
			if ( model.isAlive( x , y ) ) {
				model.clear( x , y )
			} else {
				model.set( x , y )
			}
			view.modelChanged()
		}
	}
	
	def stopButtonClicked() {
		clock.stop()
	}
	
	def startButtonClicked() {
		clock.start()
	}
	
	// constructor code
	speedChanged(50)
	view.setController( this ) // this escapes constructor...
	view.modelChanged()
}