package de.codesourcery.life.ui

import de.codesourcery.life.entities.Board

abstract class UIController(val model : Board , private val view : View ) {

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
    
	private val clock = new Clock( listener )
	
	def resetButtonClicked() {
		updateBoard {
			initModel( model )
		}
	}
	
	def speedChanged(percent:Int) {
		val delay = 1000.0 / ( 100.0 / percent )
		clock.setTickInterval( delay.asInstanceOf[Int] )
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