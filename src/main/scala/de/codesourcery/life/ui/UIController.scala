package de.codesourcery.life.ui

import de.codesourcery.life.entities.Board
import de.codesourcery.life.simulator._

/**
 * Application controller.
 * 
 *  <p>This class is responsible for:
 *  <ul>
 *    <li>Notifying the view of simulation state changes etc.</li>
 *    <li>Reacting to messages from the view about user interaction/requests</li>
 *    <li>Controlling the actual simulation state</li> 
 *  </ul<
 *   </p>
 * 
 * @author tobias.gierke@code-sourcery.de
 */
abstract class UIController(private val m : Board , private val view : View ) {

	private var lastState : Option[Board] = None
	
	private val listener = new ClockListener() {
		
		var counter : Long = 0
		
		def clockStateChanged(isRunning:Boolean) {
			if ( isRunning == false ) {
				counter = 0
			}
			view.simulatorStateChanged( isRunning )
		}
	
		def onTick() : Boolean = {
			
			// advance the model , stops
			// clock by returning false
			// if the model population
			// no longer changes
			
			
			var time1 : Double = -System.nanoTime
			val continue = model.advance()
			time1 += System.nanoTime
			
			counter += 1
			if ( ( counter % 200 ) == 0 ) {
				val format = new java.text.DecimalFormat("###,###,###,###")
				val cellsPerSecond =  ( m.width * m.height) / ( time1 / (1000*1000*1000) )  
				println("Cells / second : "+format.format( cellsPerSecond) )
			}			
			
			if ( continue ) {
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
		clock.tickIntervalMillis = delay.asInstanceOf[Int]
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
	
	def getSimulationDelayInMillis() : Int = clock.tickIntervalMillis
	
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
	
	def modelResizeRequested(width:Int,height:Int) {
		updateBoard {
			if ( width != model.width || height != model.height ) {
				model.resize( width , height )
				view.modelChanged
			}
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