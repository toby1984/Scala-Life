package de.codesourcery.life.ui

import de.codesourcery.life.entities.Board
import de.codesourcery.life.simulator._

abstract class UIController(private val m : Board , private val view : View ) {

	private var lastState : Option[Board] = None
	
	private val listener = new ClockListener() {
		
		var lastCall : Long = 0
		var allFps : Long = 0
		var counter : Long = 0
		
		def clockStateChanged(isRunning:Boolean) {
			if ( isRunning == false ) {
				allFps = 0
				lastCall = 0
				counter = 0
			}
			view.simulatorStateChanged( isRunning )
		}
	
		def onTick() : Boolean = {
			
			val currentTime = System.currentTimeMillis
			if (lastCall != 0 ) {
				
				val duration = currentTime - lastCall
				val fps = Math.round(1000.0 / duration).asInstanceOf[Int]
				
				counter+=1
				allFps += fps
				
				val avgFps = ( allFps / counter )
				println("FPS: "+avgFps)
			}
			lastCall = currentTime
			
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
		lastState = Some( model.createCopy() )
		view.savedStateAvailable()
	}
	
	def recallStateClicked() {
		
		lastState match {
			case Some( state ) =>  {
				updateBoard { 
					model.valueOf( state ) 
				}
			}
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
	
	def saveStateToFileClicked() {
		val fileName = view.querySaveFileName()
		if ( fileName.isDefined ) {
			stopButtonClicked()
			Board.saveToFile( model , fileName.get )
		}
	}
	
	def loadStateFromFileClicked() {
		val fileName = view.queryLoadFileName()
		if ( fileName.isDefined ) {
			updateBoard {
				Board.populateFromFile( model , fileName.get )
			}
		}
	}	
	
	def startButtonClicked() {
		clock.start()
	}
	
	// constructor code
	speedChanged(50)
	view.setController( this ) // this escapes constructor...
	view.modelChanged()
}